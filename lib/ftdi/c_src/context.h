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
#ifndef _FTDI_DRV_CONTEXT_H
#define _FTDI_DRV_CONTEXT_H

#include <stdbool.h>

#include <libusb.h>
#include <ftdi.h>
#include <erl_driver.h>

typedef struct _transfer_list_node transfer_list_t;

typedef struct _dev_ref_list {
    uint64_t ref;
    libusb_device *device;
    struct _dev_ref_list *next;
} dev_rev_list_t;

typedef struct {
    struct ftdi_context *ftdi_ctx;
    ErlDrvPort port;
    ErlDrvTermData term_port;
    dev_rev_list_t *devices;
    bool is_open;
    transfer_list_t *transfers;
} drv_ctx_t;

#endif
