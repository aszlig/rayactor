%%% This file is part of the RayActor Lighting Software.
%%%
%%% RayActor is free software: you can redistribute it and/or modify it under
%%% the terms of the GNU Affero General Public License as published by the Free
%%% Software Foundation, either version 3 of the License, or (at your option)
%%% any later version.
%%%
%%% RayActor is distributed in the hope that it will be useful, but WITHOUT ANY
%%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
%%% FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
%%% more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with Rayactor. If not, see <http://www.gnu.org/licenses/>.
%%%
-define(FTDI_DRV_CTRL_REQUEST_USB_FIND_ALL, 0).
-define(FTDI_DRV_CTRL_REQUEST_OPEN, 1).
-define(FTDI_DRV_CTRL_REQUEST_PURGE, 2).
-define(FTDI_DRV_CTRL_REQUEST_SEND, 3).
-define(FTDI_DRV_CTRL_REQUEST_RECV, 4).

-define(FTDI_DRV_CTRL_REPLY_OK, 0).
-define(FTDI_DRV_CTRL_REPLY_OK_NODATA, 1).
-define(FTDI_DRV_CTRL_REPLY_ERROR, 2).
-define(FTDI_DRV_CTRL_REPLY_UNKNOWN, 3).
-define(FTDI_DRV_CTRL_REPLY_REF, 4).

-define(FTDI_DRV_PURGE_METHOD_BOTH, 0).
-define(FTDI_DRV_PURGE_METHOD_RX, 1).
-define(FTDI_DRV_PURGE_METHOD_TX, 2).
