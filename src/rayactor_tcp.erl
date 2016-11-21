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
-module(rayactor_tcp).

-behaviour(supervisor).
-behaviour(rayactor_widget).

-export([start_widget/1, dmx_from_router/3]).

-export([init/1]).

-type tcp_widget_opts() :: #{address => inet:ip_address(),
                             port => inet:port_number()}.

-spec dmx_from_router(pid(), integer(), binary()) -> ok.

dmx_from_router(_Pid, _Port, _Data) -> ok.

-spec start_widget(tcp_widget_opts()) ->
    {ok, term()} | ignore | {error, term()}.

start_widget(Opts) ->
    Port = maps:get(port, Opts, 3000),
    Address = maps:get(address, Opts, {127,0,0,1}),
    case supervisor:start_link(?MODULE, {Port, Address}) of
        {ok, Pid} -> {ok, _} = supervisor:start_child(Pid, []), {ok, Pid};
        Otherwise -> Otherwise
    end.

-spec init({rayactor_widget:widget(), inet:port_number(), inet:ip_address()}) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init({Port, Address}) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {ip, Address},
                                        {active, false},
                                        {reuseaddr, true}]),
    Spec = #{id => rayactor_tcp_server,
             start => {rayactor_tcp_server, start_link, [LSock]},
             restart => temporary,
             modules => [rayactor_tcp_server]},
    {ok, {#{strategy => simple_one_for_one}, [Spec]}}.
