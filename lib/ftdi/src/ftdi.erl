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
-module(ftdi).
-include("ftdi.hrl").

-export([new/0, list_devices/1, open/2, close/1, purge/1, purge/2,
         send/2, recv/2]).
-export_type([handle/0, device/0]).

-type handle() :: port().
-type device() :: #{ref => integer(),
                    vendor => integer(),
                    product => integer(),
                    manufacturer => binary(),
                    description => binary(),
                    serial => binary()}.

-spec new() -> {ok, handle()} | {error, term()}.

new() ->
    case erl_ddll:load_driver(code:priv_dir(ftdi), "ftdi_drv") of
        ok -> {ok, erlang:open_port({spawn, "ftdi_drv"}, [binary])};
        Error -> Error
    end.

-spec open(handle(), device()) -> ok | {error, atom()}.

open(Handle, #{ref := Ref}) ->
    ctrl(Handle, ?FTDI_DRV_CTRL_REQUEST_OPEN,
         <<Ref:64/native-unsigned-integer>>).

-spec close(handle()) -> ok.

close(Handle) -> catch erlang:port_close(Handle), ok.

-spec list_devices(handle()) -> {ok, device()} | {error, atom()}.

list_devices(Handle) ->
    case ctrl(Handle, ?FTDI_DRV_CTRL_REQUEST_USB_FIND_ALL) of
        {ok, Infos}  -> {ok, [tuple2device(T) || T <- Infos]};
        {error, Err} -> {error, Err}
    end.

-spec purge(handle(), rx | tx | both) -> ok | {error, atom()}.

purge(Handle, Method) ->
    Arg = case Method of
        both -> ?FTDI_DRV_PURGE_METHOD_BOTH;
        rx   -> ?FTDI_DRV_PURGE_METHOD_RX;
        tx   -> ?FTDI_DRV_PURGE_METHOD_TX
    end,
    ctrl(Handle, ?FTDI_DRV_CTRL_REQUEST_PURGE, <<Arg:8>>).

-spec purge(handle()) -> ok | {error, atom()}.

purge(Handle) -> purge(Handle, both).

-spec send(handle(), binary()) -> ok.

send(Handle, Data) ->
    ctrl(Handle, ?FTDI_DRV_CTRL_REQUEST_SEND, Data),
    % XXX: Split into sync/async!
    receive
        {ftdi, write_done} -> ok
    after
        1000 -> {error, timeout}
    end.

-spec recv(handle(), integer()) -> ok.

recv(Handle, Size) ->
    ctrl(Handle, ?FTDI_DRV_CTRL_REQUEST_RECV,
         <<Size:64/native-unsigned-integer>>).

%% Internal functions and types

-type devtuple() :: {Vendor :: integer(),
                     Product :: integer(),
                     Manufacturer :: binary(),
                     Description :: binary(),
                     Serial :: binary()}.

-spec tuple2device(devtuple()) -> device().

tuple2device({R, V, P, M, D, S}) -> #{ref => R,
                                      vendor => V,
                                      product => P,
                                      manufacturer => M,
                                      description => D,
                                      serial => S}.

-spec ctrl(port(), byte()) -> {ok, term()} | {error, atom()}.

ctrl(Port, OpCode) -> ctrl(Port, OpCode, <<>>).

-spec ctrl(port(), byte(), binary()) -> {ok, term()} | {error, atom()}.

ctrl(Port, OpCode, Msg) ->
    case erlang:port_control(Port, OpCode, Msg) of
        <<?FTDI_DRV_CTRL_REPLY_ERROR, Err/binary>> ->
            {error, binary_to_atom(Err, latin1)};
        <<?FTDI_DRV_CTRL_REPLY_UNKNOWN>> ->
            {error, unknown_command};
        <<?FTDI_DRV_CTRL_REPLY_OK_NODATA>> -> ok;
        <<?FTDI_DRV_CTRL_REPLY_OK>> ->
            receive
                Data -> {ok, Data}
            after
                1000 -> {error, timeout}
            end
    end.
