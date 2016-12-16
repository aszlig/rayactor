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
#include <ftdi.h>
#include <erl_driver.h>

#include "transfer.h"

typedef struct {
    unsigned char *buf;
    size_t len;
    notifier_cb notifier;
    struct ftdi_transfer_control *tc;
} transfer_t;

typedef struct _transfer_list_node {
    transfer_t *transfer;
    struct _transfer_list_node *prev;
    struct _transfer_list_node *next;
} transfer_list_t;

/* Add a pending transfer as a list node to ctx->transfers. */
static int add_transfer(drv_ctx_t *ctx, transfer_t *transfer)
{
    transfer_list_t *new;

    if ((new = driver_alloc(sizeof(transfer_list_t))) == NULL)
        return -1;

    new->transfer = transfer;
    new->prev = NULL;
    new->next = ctx->transfers;

    if (ctx->transfers != NULL)
        ctx->transfers->prev = new;

    ctx->transfers = new;

    return 0;
}

/* Remove a list node frem the ctx->transfers list. */
static transfer_list_t *remove_node(drv_ctx_t *ctx, transfer_list_t *node)
{
    transfer_list_t *tmp;

    if (node->prev != NULL)
        node->prev->next = node->next;
    else
        ctx->transfers = node->next;

    if (node->next != NULL)
        node->next->prev = node->prev;

    tmp = node->next;
    driver_free(node);
    return tmp;
}

/* Free a specific transfer list node. */
static void free_transfer(transfer_t *transfer)
{
    libusb_free_transfer(transfer->tc->transfer);
    free(transfer->tc);
    driver_free(transfer);
}

/* Clean up the ctx->transfers list. */
void transfer_list_free(transfer_list_t *transfers)
{
    while (transfers != NULL) {
        transfer_list_t *next = transfers->next;
        //libusb_cancel_transfer(transfers->transfer->tc->transfer);
        free_transfer(transfers->transfer);
        driver_free(transfers);
        transfers = next;
    }
}

/* Generic transfer handler, which iterates through the transfer list and calls
 * the notifier function if a particular transfer has completed.
 */
void transfer_handle(drv_ctx_t *ctx)
{
    transfer_list_t *iter;

    if (ctx == NULL)
        return;

    iter = ctx->transfers;

    while (iter != NULL) {
        transfer_t *transfer = iter->transfer;

        if (transfer->tc->completed) {
            transfer->notifier(transfer->buf, transfer->len, ctx);
            free_transfer(transfer);
            iter = remove_node(ctx, iter);
        } else {
            iter = iter->next;
        }
    }
}

/* Submit a new receive request to the FTDI device and don't wait for
 * completion.
 *
 * The received data is later picked up by transfer_handle() which calls the
 * notifier callback passed here with the data that has been received from the
 * device.
 */
int transfer_recv(drv_ctx_t *ctx, size_t len, notifier_cb notifier)
{
    transfer_t *new;

    if ((new = driver_alloc(sizeof(transfer_t))) == NULL)
        return -1;

    if ((new->buf = driver_alloc(len)) == NULL) {
        driver_free(new);
        return -1;
    }

    if ((new->tc = ftdi_read_data_submit(ctx->ftdi_ctx,
                                         new->buf, len)) == NULL) {
        driver_free(new->buf);
        driver_free(new);
        return -1;
    }

    new->len = len;
    new->notifier = notifier;

    if (add_transfer(ctx, new) != 0) {
        driver_free(new->buf);
        libusb_cancel_transfer(new->tc->transfer);
        driver_free(new);
        return -1;
    }

    return 0;
}

/* Send data in buf to the FTDI device and don't wait for completion.
 *
 * The transfer_handle() which calls the notifier function once the data has
 * been sent to the device. Note that the notifier callback gets a NULL pointer
 * in its buf argument.
 */
int transfer_send(drv_ctx_t *ctx, unsigned char *buf, size_t len,
                  notifier_cb notifier)
{
    transfer_t *new;

    if ((new = driver_alloc(sizeof(transfer_t))) == NULL)
        return -1;

    if ((new->tc = ftdi_write_data_submit(ctx->ftdi_ctx, buf, len)) == NULL) {
        driver_free(new);
        return -1;
    }

    new->buf = NULL;
    new->len = len;
    new->notifier = notifier;

    if (add_transfer(ctx, new) != 0) {
        libusb_cancel_transfer(new->tc->transfer);
        driver_free(new);
        return -1;
    }

    return 0;
}
