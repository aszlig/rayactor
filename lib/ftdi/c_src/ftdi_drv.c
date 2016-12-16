/* This file is part of the RayActor Lighting Software.
 *
 * RayActor is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * RayActor is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Rayactor. If not, see <http://www.gnu.org/licenses/>.
 */
#include <string.h>
#include <errno.h>
#include <stdbool.h>

#include <erl_driver.h>

#include <libusb.h>
#include <ftdi.h>
#include <poll.h>

#include "ftdi_drv.h"
#include "context.h"
#include "transfer.h"

#if !defined(LIBUSB_API_VERSION) || (LIBUSB_API_VERSION < 0x01000104)
#error "You need at least libusb version 1.0.20!"
#endif

static ErlDrvSSizeT ctrl_reply(uint8_t reply, char *buf, ErlDrvSizeT len,
                               char **rbuf, ErlDrvSizeT rlen)
{
    char *ptr;
    ErlDrvBinary *bin;

    if (len >= rlen) {
        if ((bin = driver_alloc_binary(len + 1)) == NULL)
            return -1;
        ptr = *rbuf = bin->orig_bytes;
    } else {
        ptr = *rbuf;
    }

    *ptr++ = reply;
    memcpy(ptr, buf, len);
    return len + 1;
}

static inline ErlDrvSSizeT ctrl_ok(char **rbuf, ErlDrvSizeT rlen)
{
    return ctrl_reply(FTDI_DRV_CTRL_REPLY_OK, NULL, 0, rbuf, rlen);
}

static inline ErlDrvSSizeT ctrl_ok_nodata(char **rbuf, ErlDrvSizeT rlen)
{
    return ctrl_reply(FTDI_DRV_CTRL_REPLY_OK_NODATA, NULL, 0, rbuf, rlen);
}

static inline ErlDrvSSizeT ctrl_error(char *err, char **rbuf, ErlDrvSizeT rlen)
{
    return ctrl_reply(FTDI_DRV_CTRL_REPLY_ERROR, err, strlen(err), rbuf, rlen);
}

static ErlDrvSSizeT ctrl_errno(char **rbuf, ErlDrvSizeT rlen)
{
    char *desc = erl_errno_id(errno);
    return ctrl_error(desc, rbuf, rlen);
}

static uint64_t add_device_ref(drv_ctx_t *ctx, struct libusb_device *dev)
{
    dev_rev_list_t *new, *iter;

    if ((new = driver_alloc(sizeof(dev_rev_list_t))) == NULL)
        return 0;

    libusb_ref_device(dev);
    new->device = dev;
    new->ref = (uint64_t)dev;
    new->next = NULL;

    if (ctx->devices == NULL) {
        ctx->devices = new;
    } else {
        for (iter = ctx->devices; iter->next != NULL; iter = iter->next);
        iter->next = new;
    }

    return new->ref;
}

static struct libusb_device *lookup_device_ref(drv_ctx_t *ctx, uint64_t ref)
{
    dev_rev_list_t *iter;

    iter = ctx->devices;
    while (iter != NULL) {
        if (iter->ref == ref)
            return iter->device;
        iter = iter->next;
    }

    return NULL;
}

static void free_device_refs(drv_ctx_t *ctx)
{
    dev_rev_list_t *next;

    while (ctx->devices != NULL) {
        libusb_unref_device(ctx->devices->device);
        next = ctx->devices->next;
        driver_free(ctx->devices);
        ctx->devices = next;
    }
}

static int poll_event_ctrl(drv_ctx_t *ctx, int fd, short events, int on)
{
    int mode;
    ErlDrvEvent event;

    mode = (events & POLLIN  ? ERL_DRV_READ  : 0)
         | (events & POLLOUT ? ERL_DRV_WRITE : 0);
    event = (ErlDrvEvent)(ErlDrvSInt)fd;

    return driver_select(ctx->port, event, mode, on);
}

static void LIBUSB_CALL add_poll_event(int fd, short events, void *drv_data)
{
    if (poll_event_ctrl((drv_ctx_t*)drv_data, fd, events, 1) != 0) {
        // TODO: error handling!
    }
}

static void LIBUSB_CALL del_poll_event(int fd, void *drv_data)
{
    if (poll_event_ctrl((drv_ctx_t*)drv_data, fd, POLLIN | POLLOUT, 0) != 0) {
        // TODO: error handling!
    }
}

static void handle_events(drv_ctx_t *ctx)
{
    struct timeval tv;
    tv.tv_sec = 0;
    tv.tv_usec = 0;

    if (libusb_handle_events_timeout_completed(ctx->ftdi_ctx->usb_ctx,
                                               &tv, NULL) != 0) {
        // TODO: error handling!
    }

    transfer_handle(ctx);
}

#define USB_TUPLE_LEN 20

struct usb_tuple_gc {
    ErlDrvUInt64 *devref;
    ErlDrvBinary *strings[3];
};

static struct usb_tuple_gc *mk_usb_tuple(drv_ctx_t *ctx,
                                         struct libusb_device *dev,
                                         ErlDrvTermData *spec)
{
    size_t len;
    uint64_t devref;
    char strings[3][200];
    struct libusb_device_descriptor desc;
    ErlDrvTermData *sptr = spec;

    struct usb_tuple_gc *gc;

    if ((gc = driver_alloc(sizeof(struct usb_tuple_gc))) == NULL)
        return NULL;

    if ((gc->devref = driver_alloc(sizeof(ErlDrvUInt64))) == NULL) {
        driver_free(gc);
        return NULL;
    }

    if (libusb_get_device_descriptor(dev, &desc) != 0) {
        driver_free(gc);
        return NULL;
    }

    if ((devref = add_device_ref(ctx, dev)) == 0) {
        driver_free(gc);
        return NULL;
    }

    memcpy(gc->devref, &devref, sizeof(ErlDrvUInt64));

    *sptr++ = ERL_DRV_UINT64;
    *sptr++ = (ErlDrvTermData)gc->devref;
    *sptr++ = ERL_DRV_UINT;
    *sptr++ = desc.idVendor;
    *sptr++ = ERL_DRV_UINT;
    *sptr++ = desc.idProduct;

    if (ftdi_usb_get_strings(ctx->ftdi_ctx, dev,
                             strings[0], 200,
                             strings[1], 200,
                             strings[2], 200) != 0) {
        driver_free(gc->devref);
        driver_free(gc);
        return NULL;
    }

    for (int i = 0; i < 3; ++i) {
        len = strlen(strings[i]);
        gc->strings[i] = driver_alloc_binary(len);
        if (gc->strings[i] == NULL) {
            driver_free(gc->devref);
            while (i > 0) driver_free_binary(gc->strings[--i]);
            driver_free(gc);
            return NULL;
        }
        memcpy(gc->strings[i]->orig_bytes, strings[i], len);
        *sptr++ = ERL_DRV_BINARY;
        *sptr++ = (ErlDrvTermData)gc->strings[i];
        *sptr++ = len;
        *sptr++ = 0;
    }

    *sptr++ = ERL_DRV_TUPLE;
    *sptr = 6;

    return gc;
}

static ErlDrvSSizeT do_usb_find_all(drv_ctx_t *ctx,
                                    char **rbuf, ErlDrvSizeT rlen)
{
    int i, speclen, len, failed;
    struct ftdi_device_list *devlist, *devp;
    ErlDrvTermData *spec, *specptr;
    struct usb_tuple_gc **gc, **gcptr;

    if ((len = ftdi_usb_find_all(ctx->ftdi_ctx, &devlist, 0, 0)) < 0)
        return ctrl_errno(rbuf, rlen);

    speclen = len * USB_TUPLE_LEN + 3;
    spec = specptr = driver_alloc(speclen * sizeof(ErlDrvTermData));
    if (spec == NULL) {
        ftdi_list_free(&devlist);
        return ctrl_errno(rbuf, rlen);
    }

    if ((gc = driver_alloc(sizeof(struct usb_tuple_gc *) * len)) == NULL) {
        ftdi_list_free(&devlist);
        driver_free(spec);
        return ctrl_errno(rbuf, rlen);
    }

    for (i = 0, failed = 0, devp = devlist, gcptr = gc; i < len;
         ++i, devp = devp->next, ++gcptr) {
        if ((*gcptr = mk_usb_tuple(ctx, devp->dev, specptr)) == NULL)
            failed++;
        else
            specptr += USB_TUPLE_LEN;
    }

    *specptr++ = ERL_DRV_NIL;
    *specptr++ = ERL_DRV_LIST;
    *specptr = (len - failed) + 1;

    erl_drv_output_term(ctx->term_port, spec,
                        speclen - (failed * USB_TUPLE_LEN));

    for (gcptr = gc; len-- > 0; ++gcptr) {
        if (*gcptr != NULL) {
            driver_free((*gcptr)->devref);
            for (i = 0; i < 3; ++i)
                driver_free_binary((*gcptr)->strings[i]);
        }
        driver_free(*gcptr);
    }
    driver_free(gc);
    driver_free(spec);
    ftdi_list_free(&devlist);
    return ctrl_ok(rbuf, rlen);
}

static ErlDrvSSizeT do_open(drv_ctx_t *ctx, char *buf, ErlDrvSizeT len,
                            char **rbuf, ErlDrvSizeT rlen)
{
    uint64_t ref;
    struct libusb_device *dev;

    if (len < 8)
        return ctrl_error("einval", rbuf, rlen);

    memcpy(&ref, buf, sizeof(uint64_t));

    if ((dev = lookup_device_ref(ctx, ref)) == NULL)
        return ctrl_error("enoent", rbuf, rlen);

    if (ftdi_usb_open_dev(ctx->ftdi_ctx, dev) != 0)
        return ctrl_errno(rbuf, rlen);

    ctx->is_open = true;
    free_device_refs(ctx);

    return ctrl_ok_nodata(rbuf, rlen);
}

static ErlDrvSSizeT do_purge(drv_ctx_t *ctx, char *buf, ErlDrvSizeT len,
                             char **rbuf, ErlDrvSizeT rlen)
{
    int ret;

    if (!ctx->is_open)
        return ctrl_error("enoent", rbuf, rlen);

    switch (*buf) {
        case FTDI_DRV_PURGE_METHOD_BOTH:
            ret = ftdi_usb_purge_buffers(ctx->ftdi_ctx);
            break;
        case FTDI_DRV_PURGE_METHOD_RX:
            ret = ftdi_usb_purge_rx_buffer(ctx->ftdi_ctx);
            break;
        case FTDI_DRV_PURGE_METHOD_TX:
            ret = ftdi_usb_purge_tx_buffer(ctx->ftdi_ctx);
            break;
        default:
            return ctrl_error("einval", rbuf, rlen);
    }

    if (ret != 0)
        return ctrl_errno(rbuf, rlen);

    return ctrl_ok_nodata(rbuf, rlen);
}

static void send_done_cb(unsigned char *buf, size_t len, drv_ctx_t *ctx)
{
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, driver_mk_atom("ftdi"),
        ERL_DRV_ATOM, driver_mk_atom("write_done"),
        ERL_DRV_TUPLE, 2
    };

    erl_drv_output_term(ctx->term_port, spec, sizeof(spec) / sizeof(spec[0]));
}

static ErlDrvSizeT do_send(drv_ctx_t *ctx, char *buf, ErlDrvSizeT len,
                           char **rbuf, ErlDrvSizeT rlen)
{
    if (transfer_send(ctx, (unsigned char*)buf, len, send_done_cb) != 0)
        return ctrl_errno(rbuf, rlen);

    handle_events(ctx);

    return ctrl_ok_nodata(rbuf, rlen);
}

static void recv_done_cb(unsigned char *buf, size_t len, drv_ctx_t *ctx)
{
    ErlDrvTermData spec[8];
    ErlDrvBinary *bin;

    if ((bin = driver_alloc_binary(len)) == NULL)
        return; // TODO: Error handling!

    memcpy(bin->orig_bytes, buf, len);

    spec[0] = ERL_DRV_ATOM;
    spec[1] = driver_mk_atom("ftdi");
    spec[2] = ERL_DRV_BINARY;
    spec[3] = (ErlDrvTermData)bin;
    spec[4] = len;
    spec[5] = 0;
    spec[6] = ERL_DRV_TUPLE;
    spec[7] = 2;

    erl_drv_output_term(ctx->term_port, spec, 8);
    driver_free_binary(bin);
}

static ErlDrvSizeT do_recv(drv_ctx_t *ctx, char *buf, ErlDrvSizeT len,
                           char **rbuf, ErlDrvSizeT rlen)
{
    uint64_t size;

    if (len < 8)
        return ctrl_error("einval", rbuf, rlen);

    memcpy(&size, buf, sizeof(uint64_t));

    if (transfer_recv(ctx, size, recv_done_cb) != 0)
        return ctrl_errno(rbuf, rlen);

    handle_events(ctx);

    return ctrl_ok_nodata(rbuf, rlen);
}

static ErlDrvData ftdi_drv_start(ErlDrvPort port, char *command)
{
    drv_ctx_t *ctx;
    const struct libusb_pollfd **pollfds;

    if ((ctx = driver_alloc(sizeof(drv_ctx_t))) == NULL) {
        return ERL_DRV_ERROR_ERRNO;
    }

    if ((ctx->ftdi_ctx = ftdi_new()) == NULL) {
        driver_free(ctx);
        return ERL_DRV_ERROR_ERRNO;
    }

    ctx->port = port;
    ctx->term_port = driver_mk_port(port);
    ctx->devices = NULL;
    ctx->is_open = false;
    ctx->transfers = NULL;

    if (libusb_pollfds_handle_timeouts(ctx->ftdi_ctx->usb_ctx) != 1) {
        ftdi_free(ctx->ftdi_ctx);
        driver_free(ctx);
        return ERL_DRV_ERROR_GENERAL;
    }

    if ((pollfds = libusb_get_pollfds(ctx->ftdi_ctx->usb_ctx)) == NULL) {
        ftdi_free(ctx->ftdi_ctx);
        driver_free(ctx);
        return ERL_DRV_ERROR_GENERAL;
    }

    for (int i = 0; pollfds[i] != NULL; ++i)
        add_poll_event(pollfds[i]->fd, pollfds[i]->events, ctx);

    libusb_free_pollfds(pollfds);

    libusb_set_pollfd_notifiers(ctx->ftdi_ctx->usb_ctx,
                                add_poll_event,
                                del_poll_event,
                                ctx);

    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

    return (ErlDrvData) ctx;
}

static void ftdi_drv_stop(ErlDrvData drv_data)
{
    drv_ctx_t *ctx = (drv_ctx_t*)drv_data;
    free_device_refs(ctx);
    transfer_list_free(ctx->transfers);
    ftdi_free(ctx->ftdi_ctx);
    driver_free(ctx);
}

static ErlDrvSSizeT ftdi_drv_control(ErlDrvData drv_data, unsigned int cmd,
                                     char *buf, ErlDrvSizeT len, char **rbuf,
                                     ErlDrvSizeT rlen)
{
    drv_ctx_t *ctx = (drv_ctx_t*)drv_data;
    switch (cmd) {
        case FTDI_DRV_CTRL_REQUEST_USB_FIND_ALL:
            return do_usb_find_all(ctx, rbuf, rlen);
        case FTDI_DRV_CTRL_REQUEST_OPEN:
            return do_open(ctx, buf, len, rbuf, rlen);
        case FTDI_DRV_CTRL_REQUEST_PURGE:
            return do_purge(ctx, buf, len, rbuf, rlen);
        case FTDI_DRV_CTRL_REQUEST_SEND:
            return do_send(ctx, buf, len, rbuf, rlen);
        case FTDI_DRV_CTRL_REQUEST_RECV:
            return do_recv(ctx, buf, len, rbuf, rlen);
    }
    return ctrl_reply(FTDI_DRV_CTRL_REPLY_UNKNOWN, NULL, 0, rbuf, rlen);
}

static void ftdi_drv_select_handler(ErlDrvData drv_data, ErlDrvEvent event)
{
    handle_events((drv_ctx_t*)drv_data);
}

static ErlDrvEntry ftdi_drv_entry;

DRIVER_INIT(ftdi_drv)
{
	ErlDrvEntry *drv = &ftdi_drv_entry;

    drv->driver_name = "ftdi_drv";
    drv->start = ftdi_drv_start;
    drv->stop = ftdi_drv_stop;
    drv->control = ftdi_drv_control;
    drv->ready_input = ftdi_drv_select_handler;
    drv->ready_output = ftdi_drv_select_handler;
    drv->extended_marker = ERL_DRV_EXTENDED_MARKER;
    drv->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    drv->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    drv->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;

    return drv;
}
