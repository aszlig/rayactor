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
-module(rayactor_router).
-include("rayactor.hrl").
-behaviour(gen_server).

-export([send_dmx/2, recv_dmx/1]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-spec send_dmx(universe(), binary()) -> ok.

send_dmx(Universe, Data) ->
    gen_server:cast(rayactor_router, {send_dmx, Universe, Data}).

-spec recv_dmx(universe()) -> ok.

recv_dmx(Universe) ->
    gen_server:call(rayactor_router, {recv_dmx, Universe}).

-spec start_link() ->
    {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.

start_link() ->
    gen_server:start_link({local, rayactor_router}, ?MODULE, [],
                          [{debug, [trace]}]).

-spec init([]) -> {ok, #{}}.

init([]) ->
    {ok, #{outputs => #{}, inputs => #{}, receivers => #{}}}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_call({register_widget, Mod, Pid, UniCfg}, _, State) ->
    NewState = maps:fold(fun
        (Name, #{direction := in, port := Port}, #{inputs := In} = Acc) ->
            Acc#{inputs => In#{{Pid, Port} => Name}};
        (Name, #{direction := out, port := Port}, #{outputs := Out} = Acc) ->
            Acc#{outputs => Out#{Name => {Mod, Pid, Port}}}
    end, State, UniCfg),
    {reply, ok, NewState};

handle_call({recv_dmx, Uni}, {Pid, _}, #{receivers := Recs} = State) ->
    NewState = State#{
        receivers => Recs#{Uni => [Pid | maps:get(Uni, Recs, [])]}
    },
    {reply, ok, NewState};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({dmx_to_router, Pid, Port, Data},
            #{inputs := In, receivers := Receivers} = State) ->
    case maps:find({Pid, Port}, In) of
        {ok, Name} -> lists:foreach(fun(P) -> P ! {dmx_data, Name, Data} end,
                                    maps:get(Name, Receivers, []));
        error      -> ignore
    end,
    {noreply, State};

handle_cast({send_dmx, Uni, Data}, #{outputs := Outputs} = State) ->
    case maps:find(Uni, Outputs) of
        {ok, {Mod, Pid, Port}} -> Mod:dmx_from_router(Pid, Port, Data);
        error                  -> ignore
    end,
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
