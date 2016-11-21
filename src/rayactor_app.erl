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
-module(rayactor_app).

-behaviour(application).

-export([start/2, stop/1]).

widget_config() ->
    [#{module => rayactor_enttec,
       universes => #{stage => #{direction => out, port => 2},
                      licon => #{direction => in, port => 1}}},
     #{module => rayactor_tcp,
       options => #{port => 4444},
       universes => #{keyboard => #{direction => in, port => 1}}}].

start(_StartType, _StartArgs) ->
    rayactor_sup:start_link(widget_config()).

stop(_State) ->
    ok.
