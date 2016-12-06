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
-module(rayactor_client).
-behaviour(gen_server).
-behaviour(rayactor_widget).

-export([start_widget/1, dmx_from_router/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-type widget_opts() :: #{address => inet:ip_address(),
                         port => inet:port_number()}.

-spec dmx_from_router(pid(), integer(), binary()) -> ok | {error, term()}.

dmx_from_router(Pid, Port, Data) ->
    gen_server:cast(Pid, {send_dmx, Port, Data}).

-spec start_widget(widget_opts()) ->
    {ok, pid()} | ignore | {error, term()}.

start_widget(Opts) ->
    Port = maps:get(port, Opts, 3000),
    Address = maps:get(address, Opts, {127,0,0,1}),
    gen_server:start_link(?MODULE, {Port, Address}, []).

-spec init({inet:ip_address(), inet:port_number()}) -> {ok, #{}}.

init({Port, Address}) ->
    {ok, Sock} = gen_udp:open(Port, [binary, {ip, Address},
                                     {active, once},
                                     {reuseaddr, true}]),
    {ok, #{sock => Sock, clients => sets:new()}}.

handle_info({udp, Socket, Addr, Port, Raw},
            #{sock := Socket, clients := Clients} = State) ->
    NewClients = case rayactor_client_protocol:handle(Raw) of
        close -> sets:del_element({Addr, Port});
        noreply -> sets:add_element({Addr, Port}, Clients);
        {send_dmx, Data} ->
            rayactor_widget:send_dmx(1, Data),
            sets:add_element({Addr, Port}, Clients);
        {reply, Data} ->
            gen_udp:send(Socket, Addr, Port, Data),
            sets:add_element({Addr, Port}, Clients)
    end,
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#{clients => NewClients}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({send_dmx, _DataPort, Data},
            #{sock := Socket, clients := Clients} = State) ->
    Raw = rayactor_client_protocol:encode_dmx(Data),
    F = fun({Addr, Port}) ->
        gen_udp:send(Socket, Addr, Port, Raw),
        true
    end,
    {noreply, State#{clients => sets:filter(F, Clients)}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
