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

-spec recv_dmx(universe()) -> binary().

recv_dmx(Universe) ->
    gen_server:call(rayactor_router, {recv_dmx, Universe}).

-spec start_link() ->
    {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.

start_link() ->
    gen_server:start_link({local, rayactor_router}, ?MODULE, [],
                          [{debug, [trace]}]).

-spec init([]) -> {ok, #{}}.

init([]) ->
    {ok, #{outputs => #{}, inputs => #{}, input_buffers => #{}}}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_call({register_widget, Mod, Pid, UniCfg}, _, State) ->
    NewState = maps:fold(fun
        (Name, #{direction := in, port := Port},
               #{inputs := In, input_buffers := InBuf} = Acc) ->
            Acc#{inputs => In#{{Pid, Port} => Name},
                 input_buffers => InBuf#{Name => binary:copy(<<0>>, 512)}};
        (Name, #{direction := out, port := Port}, #{outputs := Out} = Acc) ->
            Acc#{outputs => Out#{Name => {Mod, Pid, Port}}}
    end, State, UniCfg),
    {reply, ok, NewState};

handle_call({recv_dmx, Uni}, _From, #{input_buffers := InBuf} = State) ->
    Reply = case maps:find(Uni, InBuf) of
        {ok, Data} -> {ok, Data};
        error -> universe_not_found
    end,
    {reply, Reply, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({dmx_to_router, Pid, Port, Data},
            #{inputs := In, input_buffers := InBuf} = State) ->
    NewState = case maps:find({Pid, Port}, In) of
        {ok, Name} -> State#{input_buffers => maps:update(Name, Data, InBuf)};
        _          -> State
    end,
    {noreply, NewState};

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
