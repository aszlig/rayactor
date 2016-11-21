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
-module(rayactor_config).
-include("rayactor.hrl").

-export([to_childspec/1]).

-export_type([universe/0, universe_options/0, widget_options/0]).

-type universe_options() :: #{direction => in | out, port => integer()}.

-type universe_config() :: #{universe() => universe_options()}.

-type widget_options() :: #{module => module(),
                            options => #{},
                            universes => universe_config()}.

-spec to_childspec(widget_options()) -> supervisor:child_spec().

to_childspec(#{module := Mod, universes := Unis} = WOpts) ->
    Opts = maps:get(options, WOpts, #{}),
    Start = {rayactor_widget, start_widget, [Mod, Unis, Opts]},
    #{id => WOpts, start => Start, restart => permanent,
      modules => [Mod]}.
