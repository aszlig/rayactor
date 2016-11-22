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
-module(rayactor_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-spec start_link([rayactor_config:widget_options()]) ->
    {ok, pid()} | ignore | {error, term()}.

start_link(Widgets) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Widgets).

-spec init([rayactor_config:widget_options()]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init(Widgets) ->
    Router = #{id => rayactor_router,
               start => {rayactor_router, start_link, []},
               restart => permanent,
               modules => [rayactor_router]},

    WSpecs = [rayactor_config:to_childspec(W) || W <- Widgets],

    {ok, {{one_for_one, 5, 10}, [Router] ++ WSpecs}}.
