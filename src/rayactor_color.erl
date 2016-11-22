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
-module(rayactor_color).

-export([new/1, new/3, set/3, to_binary/1, blend/3]).

-record(color, {red :: red(), green :: green(), blue :: blue()}).

-type red() :: byte().
-type green() :: byte().
-type blue() :: byte().
-type channel() :: red() | green() | blue().
-type color() :: #color{}.

-export_type([color/0, channel/0, red/0, green/0, blue/0]).

-spec new(red() | float(), green() | float(), blue() | float()) -> color().

new(Red, Green, Blue) ->
    #color{red = round(Red), green = round(Green), blue = round(Blue)}.

-spec new(non_neg_integer() | binary()) -> color().

new(IntVal) when is_integer(IntVal) -> new(<<IntVal:24>>);
new(<<Red:8, Green:8, Blue:8>>) -> new(Red, Green, Blue).

-spec set(color(), red | green | blue, channel()) -> color().

set(C, red,   Val) -> C#color{red   = Val};
set(C, green, Val) -> C#color{green = Val};
set(C, blue,  Val) -> C#color{blue  = Val}.

-spec to_binary(color()) -> binary().

to_binary(C) -> <<(C#color.red):8, (C#color.green):8, (C#color.blue):8>>.

-spec blend(color(), color(), Weight :: float()) -> color().

blend(C1, C2, Weight) ->
    R = C1#color.red   * (1.0 - Weight) + C2#color.red   * Weight,
    G = C1#color.green * (1.0 - Weight) + C2#color.green * Weight,
    B = C1#color.blue  * (1.0 - Weight) + C2#color.blue  * Weight,
    new(R, G, B).
