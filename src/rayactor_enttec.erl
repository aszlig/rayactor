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
-module(rayactor_enttec).
-behaviour(gen_fsm).
-behaviour(rayactor_widget).

-export([start_widget/1, dmx_from_router/3]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([wait_for_hw_version/2, get_parameters/2, get_parameters2/2,
         wait_for_dmx/2, wait_for_dmx_change/2]).

-record(state, {ftdi, options = #{}, dmxin = none, header = none}).

-define(USB_PRO_TIMEOUT, 2000).
-define(DEFAULT_TIMEOUT, 4000).

-type enttec_widget_opts() :: #{serial => binary()}.

-type enttec_packet_type() ::
    assign_ports | change_of_state | flash_page | get_hw_version |
    get_parameters | get_parameters2 | get_serial | receive_dmx_on_change |
    received_dmx | reprogram_firmware | send_dmx | send_dmx2 | send_rdm |
    send_rdm_discovery | set_parameters | set_parameters2.

-spec start_widget(enttec_widget_opts()) ->
    {ok, pid()} | ignore | {error, term()}.

start_widget(Opts) ->
    gen_fsm:start_link(?MODULE, Opts, []).

-spec dmx_from_router(pid(), integer(), binary()) -> ok | {error, term()}.

dmx_from_router(Pid, 1, Data) -> send_to_widget(Pid, send_dmx, Data);
dmx_from_router(Pid, 2, Data) -> send_to_widget(Pid, send_dmx2, Data);
dmx_from_router(_, _, _)      -> {error, unknown_universe}.

-spec send_to_widget(pid(), enttec_packet_type(), term()) -> ok.

send_to_widget(Pid, Type, Data) ->
    gen_fsm:send_all_state_event(Pid, {send_to_widget, Type, Data}).

-spec get_packet_type(integer()) -> enttec_packet_type() | unknown.

get_packet_type(2)   -> flash_page;
get_packet_type(3)   -> get_parameters;
get_packet_type(189) -> get_parameters2;
get_packet_type(5)   -> received_dmx;
get_packet_type(9)   -> change_of_state;
get_packet_type(10)  -> get_serial;
get_packet_type(14)  -> get_hw_version;
get_packet_type(_)   -> unknown.

-spec put_packet_type(enttec_packet_type()) -> integer() | unknown.

put_packet_type(reprogram_firmware)    -> 1;
put_packet_type(flash_page)            -> 2;
put_packet_type(get_parameters)        -> 3;
put_packet_type(get_parameters2)       -> 189;
put_packet_type(set_parameters)        -> 4;
put_packet_type(set_parameters2)       -> 195;
put_packet_type(send_dmx)              -> 6;
put_packet_type(send_rdm)              -> 7;
put_packet_type(receive_dmx_on_change) -> 8;
put_packet_type(get_serial)            -> 10;
put_packet_type(send_rdm_discovery)    -> 11;
put_packet_type(enable_api_v2)         -> 13;
put_packet_type(get_hw_version)        -> 14;
put_packet_type(send_dmx2)             -> 203;
put_packet_type(assign_ports)          -> 151;
put_packet_type(_)                     -> unknown.

-spec decode_packet_data(enttec_packet_type(), binary()) ->
    {ok, term()} | {error, term()}.

decode_packet_data(received_dmx, <<0, 0, Data/binary>>) ->
    {ok, Data};
decode_packet_data(received_dmx, <<0:6, 1:1, 0:1, _/binary>>) ->
    {error, queue_overflow};
decode_packet_data(received_dmx, <<0:7, 1:1, _/binary>>) ->
    {error, queue_overrun};
decode_packet_data(received_dmx, <<0, S, _/binary>>) when S /= 0 ->
    {error, unknown_start_code};
decode_packet_data(T, <<FwVer:16/little, Break, MarkAfterBreak,
                        Rate, UserData/binary>>) when T =:= get_parameters;
                                                      T =:= get_parameters2 ->
    {ok, #{firmware_version => FwVer,
           break_time => Break,
           mark_after_break_time => MarkAfterBreak,
           output_rate => Rate,
           user_data => UserData}};
decode_packet_data(get_hw_version, <<Ver>>) ->
    {ok, Ver};
decode_packet_data(change_of_state, <<Offset, Index:5/binary, Data/binary>>) ->
    {ok, {Offset, Index, Data}};
decode_packet_data(_, _) ->
    {error, unknown_packet}.

-spec decode_packet(integer(), binary()) ->
    {ok, enttec_packet_type(), term()} | {error, term()}.

decode_packet(Type, Data) ->
    case get_packet_type(Type) of
        unknown  -> {error, unknown_packet_type};
        TypeName ->
            case decode_packet_data(TypeName, Data) of
                {ok, Result} -> {ok, TypeName, Result};
                {error, Err} -> {error, Err}
            end
    end.

-spec encode_packet_data(enttec_packet_type(), term()) ->
    {ok, binary()} | {error, term()}.

encode_packet_data(T, Data) when T =:= send_dmx; T =:= send_dmx2 ->
    {ok, <<0, Data/binary>>};
encode_packet_data(assign_ports, {Dunno, Dunno}) ->
    {ok, <<Dunno, Dunno>>};
encode_packet_data(receive_dmx_on_change, send_always) ->
    {ok, <<0>>};
encode_packet_data(receive_dmx_on_change, on_change_only) ->
    {ok, <<1>>};
encode_packet_data(T, UserDataSize) when T =:= get_parameters;
                                         T =:= get_parameters2 ->
    {ok, <<UserDataSize:16/little>>};
encode_packet_data(T, Params) when T =:= set_parameters;
                                   T =:= set_parameters2 ->
    Break = maps:get(break_time, Params, 9),
    MAB = maps:get(mark_after_break_time, Params, 1),
    Rate = maps:get(output_rate, Params, 40),
    UserData = maps:get(user_data, Params, <<>>),
    UserSize = byte_size(UserData),
    {ok, <<UserSize:16/little, Break:8, MAB:8, Rate:8, UserData/binary>>};
encode_packet_data(enable_api_v2, Key) ->
    {ok, <<Key:32/little>>};
encode_packet_data(get_hw_version, _) ->
    {ok, <<>>};
encode_packet_data(unknown, _) ->
    {error, unknown_packet_type};
encode_packet_data(_, _) ->
    {error, invalid_packet_data}.

-spec encode_packet(enttec_packet_type(), term()) ->
    {ok, binary()} | {error, term()}.

encode_packet(Type, Data) ->
    TypeNum = put_packet_type(Type),
    case encode_packet_data(Type, Data) of
        {ok, Result} ->
            Len = byte_size(Result),
            {ok, <<16#7E, TypeNum, Len:16/little, Result/binary, 16#E7>>};
        {error, Err} -> {error, Err}
    end.

-spec reverse_bits(binary(), binary()) -> binary().

reverse_bits(<<>>, Acc) -> Acc;
reverse_bits(<<B:1, Rest/bitstring>>, Acc) ->
    reverse_bits(Rest, <<B:1, Acc/bitstring>>).

-spec reverse_bits(binary()) -> binary().

reverse_bits(Data) ->
    << <<(reverse_bits(Byte, <<>>))/binary>> || <<Byte:1/binary>> <= Data >>.

-spec splice_dmx_change(Index :: [0 | 1],
                        Changed :: binary(),
                        Data :: binary(),
                        Acc :: binary()) -> binary().

splice_dmx_change([0 | RestIdx], Unchanged,
                  <<Current, RestData/binary>>, Acc) ->
    splice_dmx_change(RestIdx, Unchanged, RestData, <<Acc/binary, Current>>);
splice_dmx_change([1 | RestIdx], <<Changed, RestChanged/binary>>,
                  <<_, RestData/binary>>, Acc) ->
    splice_dmx_change(RestIdx, RestChanged, RestData, <<Acc/binary, Changed>>);
splice_dmx_change(_, <<>>, Rest, Acc) ->
    <<Acc/binary, Rest/binary>>.

-spec splice_dmx_change(Index :: binary(),
                        Changed :: binary(),
                        Data :: binary()) -> binary().

splice_dmx_change(Index, Changed, Data) ->
    splice_dmx_change([I || <<I:1>> <= reverse_bits(Index)],
                      Changed, Data, <<>>).

-spec match_device(enttec_widget_opts(), ftdi:device()) -> boolean().

match_device(Opts, #{manufacturer := <<"ENTTEC">>,
                     description := <<"DMX USB PRO", _/binary>>,
                     serial := Serial}) ->
    case maps:get(serial, Opts, none) of
        none   -> true;
        Serial -> true;
        _      -> false
    end.

init(Opts) ->
    {ok, Ftdi} = ftdi:new(),

    {ok, Devices} = ftdi:list_devices(Ftdi),

    [Dev|_] = lists:filter(fun(D) -> match_device(Opts, D) end, Devices),

    ok = ftdi:open(Ftdi, Dev),

    {ok, SendFull} = encode_packet(receive_dmx_on_change, send_always),
    ftdi:sync_send(Ftdi, SendFull),
    {ok, GetParams} = encode_packet(get_hw_version, none),

    ftdi:purge(Ftdi, rx),
    ftdi:sync_send(Ftdi, GetParams),

    ftdi:recv(Ftdi, 4),
    NewState = #state{ftdi = Ftdi, options = Opts},
    {ok, wait_for_hw_version, NewState, ?USB_PRO_TIMEOUT}.

wait_for_hw_version({from_widget, get_hw_version, _Ver}, State) ->
    % Permission to publish the API key granted by ENTTEC.
    % Ticket reference: EU #DQA-88617-214
    {ok, EnableApi} = encode_packet(enable_api_v2, 16#BFC77FA4),
    ftdi:sync_send(State#state.ftdi, EnableApi),
    {ok, AssignPorts} = encode_packet(assign_ports, {1, 1}),
    ftdi:sync_send(State#state.ftdi, AssignPorts),
    {ok, GetParams} = encode_packet(get_parameters2, 0),
    ftdi:sync_send(State#state.ftdi, GetParams),
    {next_state, get_parameters2, State, ?DEFAULT_TIMEOUT};

wait_for_hw_version(timeout, State) ->
    {ok, GetParams} = encode_packet(get_parameters, 0),
    ftdi:sync_send(State#state.ftdi, GetParams),
    {next_state, get_parameters, State, ?DEFAULT_TIMEOUT};

wait_for_hw_version({from_widget, received_dmx, _}, State) ->
    {next_state, wait_for_hw_version, State, ?USB_PRO_TIMEOUT}.

get_parameters2({from_widget, get_parameters2, Params},
                #state{options = Opts} = State) ->
    ParamsFromOpts = case maps:find(port2_params, Opts) of
        {ok, P} -> P;
        error   -> #{}
    end,
    NewParams = maps:merge(Params, ParamsFromOpts),
    {ok, SetParams} = encode_packet(set_parameters2, NewParams),
    ftdi:sync_send(State#state.ftdi, SetParams),
    {ok, GetParams} = encode_packet(get_parameters, 0),
    ftdi:sync_send(State#state.ftdi, GetParams),
    {next_state, get_parameters, State, ?DEFAULT_TIMEOUT};

get_parameters2({from_widget, received_dmx, _}, State) ->
    {next_state, get_parameters2, State, ?DEFAULT_TIMEOUT}.

get_parameters({from_widget, get_parameters, Params},
                #state{options = Opts} = State) ->
    ParamsFromOpts = case maps:find(port_params, Opts) of
        {ok, P} -> P;
        error   -> #{}
    end,
    NewParams = maps:merge(Params, ParamsFromOpts),
    {ok, SetParams} = encode_packet(set_parameters, NewParams),
    ftdi:sync_send(State#state.ftdi, SetParams),
    {next_state, wait_for_dmx, State};

get_parameters({from_widget, received_dmx, _}, State) ->
    {next_state, get_parameters, State, ?DEFAULT_TIMEOUT}.

wait_for_dmx({from_widget, received_dmx, Data}, State) ->
    {ok, ChangeOnly} = encode_packet(receive_dmx_on_change, on_change_only),
    ftdi:sync_send(State#state.ftdi, ChangeOnly),
    Padding = binary:copy(<<0>>, 512 - byte_size(Data)),
    FullData = <<Data/binary, Padding/binary>>,
    rayactor_widget:send_dmx(1, FullData),
    {next_state, wait_for_dmx, State#state{dmxin = FullData}};

wait_for_dmx({from_widget, change_of_state, _} = Data, State) ->
    wait_for_dmx_change(Data, State).

wait_for_dmx_change({from_widget, change_of_state, {Offset, Index, Data}},
                    #state{dmxin = Old} = State) ->
    StartByte = Offset * 8,
    <<Start:StartByte/binary, Rest/binary>> = <<0, Old/binary>>,
    NewRest = splice_dmx_change(Index, Data, Rest),
    <<0, New/binary>> = <<Start/binary, NewRest/binary>>,
    rayactor_widget:send_dmx(1, New),
    {next_state, wait_for_dmx_change, State#state{dmxin = New}}.

handle_event({send_to_widget, Type, Data}, StateName, State) ->
    {ok, Raw} = encode_packet(Type, Data),
    ftdi:sync_send(State#state.ftdi, Raw),
    {next_state, StateName, State};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info({ftdi, recv, _Ref, <<16#7E, Type, Len:16/little>>}, StateName,
            #state{header = none} = State) ->
    ftdi:recv(State#state.ftdi, Len + 1),
    {next_state, StateName, State#state{header = {Type, Len}}};

handle_info({ftdi, recv, _Ref, FullData}, StateName,
            #state{header = {Type, Len}} = State)
        when byte_size(FullData) == Len + 1 ->
    <<Data:Len/binary, 16#E7>> = FullData,
    Event = case decode_packet(Type, Data) of
        {ok, DType, DData} -> {from_widget, DType, DData};
        {error, Error}     -> {widget_error, Error}
    end,
    ftdi:recv(State#state.ftdi, 4),
    ?MODULE:StateName(Event, State#state{header = none});

%handle_info({uart_async, Uart, _Ref, {error, Err}}, _StateName,
%            #state{ftdi = Ftdi} = State) ->
%    {stop, {widget_error, Err}, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #state{ftdi = Ftdi}) ->
    ftdi:close(Ftdi),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
