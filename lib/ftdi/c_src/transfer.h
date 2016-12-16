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
#ifndef _FTDI_DRV_TRANSFER_H
#define _FTDI_DRV_TRANSFER_H

#include "context.h"

typedef void (*notifier_cb)(unsigned char *buf, size_t len, drv_ctx_t *ctx);

void transfer_list_free(transfer_list_t*);
void transfer_handle(drv_ctx_t*);
int transfer_recv(drv_ctx_t*, size_t, notifier_cb);
int transfer_send(drv_ctx_t*, unsigned char *buf, size_t, notifier_cb);

#endif
