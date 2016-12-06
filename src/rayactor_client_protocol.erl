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
-module(rayactor_client_protocol).

-export([handle/1, encode_dmx/1]).

-define(MAGIC, <<$R, $a, $y, $C, $l, $i, $e, $n, $t>>).

-spec handle(binary()) ->
    close | noreply | {send_dmx, rayactor_dmx:dmx()} | {reply, binary()}.

handle(?MAGIC) -> {reply, <<"TODO">>};
handle(Data)   -> {send_dmx, rayactor_dmx:from_binary(Data)}.

-spec encode_dmx(rayactor_dmx:dmx()) -> binary().

encode_dmx(Data) -> rayactor_dmx:to_binary(Data).
