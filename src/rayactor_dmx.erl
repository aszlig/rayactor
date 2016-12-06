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
-module(rayactor_dmx).

-export([new/0, from_binary/1, to_binary/1, set/3]).
-export_type([dmx/0]).

-type dmx() :: binary().
-type channel() :: integer().

-spec new() -> dmx().

new() -> binary:copy(<<0>>, 512).

-spec from_binary(binary()) -> dmx().

from_binary(<<Bin:512/binary>> = B)
    when byte_size(B) >= 512 -> Bin;
from_binary(<<Bin/binary>>)  -> Bin.

-spec to_binary(dmx()) -> binary().

to_binary(Bin) -> Bin.

-spec set(integer(), channel(), dmx()) -> dmx().

set(Pos, Chan, Data) ->
    <<Left:Pos/binary, _:8, Right/binary>> = Data,
    <<Left:Pos/binary, Chan:8, Right/binary>>.
