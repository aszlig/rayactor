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
-module(rayactor_widget).
-include("rayactor.hrl").

-export([behaviour_info/1, start_widget/3, send_dmx/2, send_dmx/3]).

behaviour_info(callbacks) ->
    [{start_widget, 1}, {dmx_from_router, 3}];
behaviour_info(_) ->
    undefined.

-spec start_widget(module(), rayactor_config:universe_config(), #{}) ->
    {ok, pid()} | ignore | {error, term()}.

start_widget(Mod, UniCfg, Opts) ->
    case Mod:start_widget(Opts) of
        {ok, Pid} ->
            Args = {register_widget, Mod, Pid, UniCfg},
            gen_server:call(rayactor_router, Args),
            {ok, Pid};
        Other -> Other
    end.

-spec send_dmx(integer(), binary()) -> ok.

send_dmx(Port, Data) ->
    send_dmx(self(), Port, Data).

-spec send_dmx(pid(), integer(), binary()) -> ok.

send_dmx(MainPid, Port, Data) ->
    gen_server:cast(rayactor_router, {dmx_to_router, MainPid, Port, Data}).
