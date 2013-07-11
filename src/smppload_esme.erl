-module(smppload_esme).

-behaviour(gen_esme).

%% API
-export([
	start_link/0,
	start/0,
	stop/0,

	connect/2,

	bind_transmitter/1,
	bind_receiver/1,
	bind_transceiver/1,
	unbind/0,

	submit_sm/1,

	get_avg_rps/0,
	get_rps/0,
	set_max_rps/1
]).

%% gen_esme callbacks
-export([
	handle_accept/3,
	handle_alert_notification/2,
	handle_closed/2,
	handle_data_sm/3,
	handle_deliver_sm/3,
	handle_outbind/2,
	handle_req/4,
	handle_resp/3,
	handle_unbind/3
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include("smppload.hrl").
-include("gen_server_spec.hrl").
-include_lib("oserl/include/oserl.hrl").

-define(HIGH, 0).
-define(LOW, 10).

-define(BIND_TIMEOUT, 30000).
-define(UNBIND_TIMEOUT, 30000).
-define(SUBMIT_TIMEOUT, 120000).
-define(DELIVERY_TIMEOUT, 100000).

-record(state, {
	bind_type,

	bind_from,
	bind_ref,
	bind_req,

	unbind_from,
	unbind_ref,
	unbind_req,

	submit_reqs = [],
	delivery_reqs = []
}).

-type reason() :: term().
-type plist() :: [{atom(), term()}].
-type remote_system_id() :: string().
-type out_msg_id() :: binary() | string().
-type delivery_status() :: atom().
-type delivery_res() :: no_delivery | delivery_timeout | delivery_status().

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()} | {error, reason()}.
start_link() ->
	Opts = [], %[{rps, 1}, {file_queue, "./sample_esme.dqueue"}],
	gen_esme:start_link({local, ?MODULE}, ?MODULE, [], Opts).

-spec start() -> {ok, pid()} | {error, reason()}.
start() ->
	Opts = [],
	gen_esme:start({local, ?MODULE}, ?MODULE, [], Opts).

-spec stop() -> ok.
stop() ->
	gen_esme:cast(?MODULE, stop).

-spec connect(inet:ip_address(), inet:port_number()) -> ok | {error, reason()}.
connect(Host, Port) ->
	gen_esme:open(?MODULE, Host, [{port, Port}]).

-spec bind_transmitter(plist()) -> {ok, remote_system_id()} | {error, reason()}.
bind_transmitter(Params) ->
	gen_esme:call(?MODULE, {bind_transmitter, Params}, ?BIND_TIMEOUT).

-spec bind_receiver(plist()) -> {ok, string()} | {error, reason()}.
bind_receiver(Params) ->
	gen_esme:call(?MODULE, {bind_receiver, Params}, ?BIND_TIMEOUT).

-spec bind_transceiver(plist()) -> {ok, string()} | {error, reason()}.
bind_transceiver(Params) ->
	gen_esme:call(?MODULE, {bind_transceiver, Params}, ?BIND_TIMEOUT).

-spec unbind() -> ok | {error, reason()}.
unbind() ->
	try gen_esme:call(?MODULE, {unbind, []}, ?UNBIND_TIMEOUT)
	catch
		exit:Reason ->
			{error, Reason}
	end.

-spec submit_sm(plist()) ->
	{ok, out_msg_id(), delivery_res()} | {error, reason()}.
submit_sm(Params) ->
	gen_esme:call(?MODULE, {submit_sm, Params, [], ?LOW}, ?SUBMIT_TIMEOUT).

-spec get_avg_rps() -> {ok, non_neg_integer()} | {error, reason()}.
get_avg_rps() ->
	try gen_esme:rps_avg(?MODULE) of
		AvgRps ->
			{ok, AvgRps}
	catch
		exit:Reason ->
			{error, Reason}
	end.

-spec get_rps() -> non_neg_integer().
get_rps() ->
    gen_esme:rps(?MODULE).

-spec set_max_rps(non_neg_integer()) -> ok.
set_max_rps(Rps) ->
    gen_esme:rps_max(?MODULE, Rps).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({bind_transmitter, Params}, From, State) ->
	gen_esme:bind_transmitter(?MODULE, Params, []),
	{noreply, State#state{
		bind_from = From, bind_req = {bind_transmitter, Params}
	}};
handle_call({bind_receiver, Params}, From, State) ->
	gen_esme:bind_receiver(?MODULE, Params, []),
	{noreply, State#state{
		bind_from = From, bind_req = {bind_receiver, Params}
	}};
handle_call({bind_transceiver, Params}, From, State) ->
	gen_esme:bind_transceiver(?MODULE, Params, []),
	{noreply, State#state{
		bind_from = From, bind_req = {bind_transceiver, Params}
	}};
handle_call({unbind, Params}, From, State) ->
	gen_esme:unbind(?MODULE, Params),
	{noreply, State#state{
		unbind_from = From, unbind_req = {unbind, Params}
	}};
handle_call({submit_sm, Params, Args, Priority}, From, State) ->
	gen_esme:queue_submit_sm(?MODULE, Params, Args, Priority),
	Req = {submit_sm, Params},
	{noreply, State#state{
		submit_reqs = [
			{Req, From, undefined, undefined} | State#state.submit_reqs
		]
	}}.

handle_cast(stop, State) ->
	gen_esme:close(?MODULE),
	{noreply, State}.

handle_info({timeout, TimerRef, ReqRef}, State) ->
	SubmitReqs0 = State#state.submit_reqs,
	DeliveryReqs0 = State#state.delivery_reqs,

	%% find timeouted delivery request.
	{{Req, From, ReqRef, OutMsgId}, SubmitReqs1} =
		cl_lists:keyextract(ReqRef, 3, SubmitReqs0),
	{{ReqRef, TimerRef}, DeliveryReqs1} =
			cl_lists:keyextract(ReqRef, 1, DeliveryReqs0),
	?ERROR("Request: ~p~n", [Req]),
	?ERROR("Delivery timeout~n", []),

	%% reply to caller.
	gen_esme:reply(From, {ok, OutMsgId, delivery_timeout}),

	{noreply, State#state{
		submit_reqs = SubmitReqs1,
		delivery_reqs = DeliveryReqs1
	}};
handle_info(Info, State) ->
	?ERROR("Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% gen_esme callbacks
%% ===================================================================

-spec handle_req(any(), any(), reference(), any()) -> any().
handle_req({bind_transmitter, _Params}, _Args, ReqRef, State) ->
	{noreply, State#state{bind_type = tx, bind_ref = ReqRef}};
handle_req({bind_receiver, _Params}, _Args, ReqRef, State) ->
	{noreply, State#state{bind_type = rx, bind_ref = ReqRef}};
handle_req({bind_transceiver, _Params}, _Args, ReqRef, State) ->
	{noreply, State#state{bind_type = trx, bind_ref = ReqRef}};
handle_req({unbind, _Params}, _Args, ReqRef, State) ->
	{noreply, State#state{bind_type = undefined, unbind_ref = ReqRef}};
handle_req(Req, _Args, ReqRef, State) ->
	{{Req, From, undefined, undefined}, Reqs} =
		cl_lists:keyextract(Req, 1, State#state.submit_reqs),
	{noreply, State#state{submit_reqs = [{Req, From, ReqRef, undefined} | Reqs]}}.

-spec handle_resp(any(), reference(), any()) -> any().
handle_resp({ok, PduResp}, ReqRef, State = #state{
	bind_from = From,
	bind_ref = ReqRef,
	bind_req = Req
}) ->
	?DEBUG("Request: ~p~n", [Req]),
	?DEBUG("Response: ~p~n", [prettify_pdu(PduResp)]),
	SystemId = smpp_operation:get_value(system_id, PduResp),
	gen_esme:reply(From, {ok, SystemId}),
	gen_esme:resume(?MODULE),
	{noreply, State#state{
		bind_from = undefined,
		bind_ref = undefined,
		bind_req = undefined,
		submit_reqs = []
	}};
handle_resp({ok, PduResp}, ReqRef, State = #state{
	unbind_from = From,
	unbind_ref = ReqRef,
	unbind_req = Req
}) ->
	?DEBUG("Request: ~p~n", [Req]),
	?DEBUG("Response: ~p~n", [prettify_pdu(PduResp)]),
	gen_esme:reply(From, ok),
	{noreply, State#state{
		unbind_from = undefined,
		unbind_ref = undefined,
		unbind_req = undefined,
		submit_reqs = [],
		delivery_reqs = []
	}};
handle_resp({error, {command_status, Status}}, ReqRef, State = #state{
	bind_from = From,
	bind_ref = ReqRef,
	bind_req = Req
}) ->
	?DEBUG("Request: ~p~n", [Req]),
	?ERROR("Bind failed with: (~p) ~s~n", [Status, smpp_error:format(Status)]),
	gen_esme:reply(From, {error, {error_status_code, Status}}),
	gen_esme:close(?MODULE),
	{noreply, State#state{
		bind_from = undefined,
		bind_ref = undefined,
		bind_req = undefined
	}};
handle_resp({error, {command_status, Status}}, ReqRef, State = #state{
	unbind_from = From,
	unbind_ref = ReqRef,
	unbind_req = Req
}) ->
	?DEBUG("Request: ~p~n", [Req]),
	?ERROR("Bind failed with: (~p) ~s~n", [Status, smpp_error:format(Status)]),
	gen_esme:reply(From, {error, {error_status_code, Status}}),
	gen_esme:close(?MODULE),
	{noreply, State#state{
		unbind_from = undefined,
		unbind_ref = undefined,
		unbind_req = undefined
	}};
handle_resp({ok, PduResp}, ReqRef, State) ->
	{{Req, From, ReqRef, undefined}, Reqs} =
		cl_lists:keyextract(ReqRef, 3, State#state.submit_reqs),
	?DEBUG("Request: ~p~n", [Req]),
	?DEBUG("Response: ~p~n", [prettify_pdu(PduResp)]),

	OutMsgId = smpp_operation:get_value(message_id, PduResp),

	{_, Params} = Req,

	RegDlr = ?gv(registered_delivery, Params, 0),
	BindType = State#state.bind_type,
	WaitForDelivery =
		case {BindType, RegDlr} of
			{rx, 1} ->
				true;
			{trx, 1} ->
				true;
			_ ->
				false
		end,
	State1 =
		case WaitForDelivery of
			false ->
				gen_esme:reply(From, {ok, OutMsgId, no_delivery}),
				State#state{submit_reqs = Reqs};
			true ->
				%% start wait for delivery timer.
				TimerRef = erlang:start_timer(?DELIVERY_TIMEOUT, self(), ReqRef),
				State#state{
					submit_reqs = [{Req, From, ReqRef, OutMsgId} | Reqs],
					delivery_reqs = [
						{ReqRef, TimerRef} | State#state.delivery_reqs
					]
				}
		end,
	{noreply, State1};
handle_resp({error, {command_status, Status}}, ReqRef, State) ->
	{{Req, From, ReqRef, undefined}, Reqs} =
		cl_lists:keyextract(ReqRef, 3, State#state.submit_reqs),
	?DEBUG("Request: ~p~n", [Req]),
	?ERROR("Failed with: (~p) ~s~n", [Status, smpp_error:format(Status)]),
	gen_esme:reply(From, {error, {error_status_code, Status}}),
	{noreply, State#state{submit_reqs = Reqs}}.

-spec handle_deliver_sm(any(), any(), any()) -> any().
handle_deliver_sm(PduDlr, _From, State0) ->
	?DEBUG("Deliver: ~p~n", [prettify_pdu(PduDlr)]),
	{?COMMAND_ID_DELIVER_SM, 0, _SeqNum, Body} = PduDlr,
	EsmClass = smpp_operation:get_value(esm_class, PduDlr),
	IsReceipt =
		EsmClass band ?ESM_CLASS_TYPE_MC_DELIVERY_RECEIPT =:=
			?ESM_CLASS_TYPE_MC_DELIVERY_RECEIPT,
	{Reply, State1}  =
		case IsReceipt of
			true ->
				handle_receipt(Body, State0);
			false ->
				handle_message(Body, State0)
	end,
	{reply, Reply, State1}.

-spec handle_closed(any(), any()) -> any().
handle_closed(Reason, State) ->
	?DEBUG("Session closed with: ~p~n", [Reason]),
	{stop, Reason, State}.

-spec handle_unbind(any(), any(), any()) -> any().
handle_unbind(_Pdu, _From, State) ->
	?ERROR("Unexpected Unbind~n", []),
    {reply, ok, State}.

-spec handle_outbind(any(), any()) -> any().
handle_outbind(Pdu, State) ->
	erlang:error(function_clause, [Pdu, State]).

-spec handle_data_sm(any(), any(), any()) -> any().
handle_data_sm(Pdu, From, State) ->
    erlang:error(function_clause, [Pdu, From, State]).

-spec handle_accept(any(), any(), any()) -> any().
handle_accept(Addr, From, State) ->
    erlang:error(function_clause, [Addr, From, State]).

-spec handle_alert_notification(any(), any()) -> any().
handle_alert_notification(Pdu, State) ->
    erlang:error(function_clause, [Pdu, State]).

%% ===================================================================
%% Internal
%% ===================================================================

handle_receipt(Body, State) ->
	?DEBUG("Receipt: ~p~n", [Body]),
	{OutMsgId, DlrState} = receipt_data(Body),
	SubmitReqs0 = State#state.submit_reqs,
	DeliveryReqs0 = State#state.delivery_reqs,
	{SubmitReqs2, DeliveryReqs2}  =
		case lists:keyfind(OutMsgId, 4, SubmitReqs0) of
			false ->
				?DEBUG("Ignored~n", []),
				{SubmitReqs0, DeliveryReqs0};
			_ ->
				%% process request.
				{{_Req, From, ReqRef, OutMsgId}, SubmitReqs1} =
					cl_lists:keyextract(OutMsgId, 4, SubmitReqs0),
				%% cancel wait for delivery timer.
				{{ReqRef, TimerRef}, DeliveryReqs1} =
					cl_lists:keyextract(ReqRef, 1, DeliveryReqs0),
				erlang:cancel_timer(TimerRef),
				%% reply to caller.
				gen_esme:reply(From, {ok, OutMsgId, DlrState}),
				?DEBUG("Processed~n", []),
				{SubmitReqs1, DeliveryReqs1}
		end,
	{{ok, []}, State#state{
		submit_reqs = SubmitReqs2,
		delivery_reqs = DeliveryReqs2
	}}.

handle_message(Body, State) ->
	?ERROR("Unexpected message: ~p~n", [Body]),
	{{ok, []}, State}.

receipt_data(Body) ->
    case receipt_data_from_tlv(Body) of
        false -> receipt_data_from_text(Body);
        Data  -> Data
    end.

receipt_data_from_tlv(Body) ->
	ID = ?gv(receipted_message_id, Body),
	State = ?gv(message_state, Body),
	case ID =/= undefined andalso State =/= undefined of
		true  -> {ID, State};
		false -> false
	end.

receipt_data_from_text(Body) ->
	Text = ?gv(short_message, Body),
	Opts = [caseless, {capture, all_but_first, list}],
	{match, [ID]} = re:run(Text, "id:([[:xdigit:]]+)", Opts),
	{match, [State]} = re:run(Text, "stat:(\\w+)", Opts),
	{ID, State}.

prettify_pdu({CmdId, Status, SeqNum, Body}) ->
	case CmdId of
		?COMMAND_ID_DELIVER_SM ->
			{deliver_sm, Status, SeqNum, Body};
		?COMMAND_ID_SUBMIT_SM_RESP ->
			{submit_sm_resp, Status, SeqNum, Body};
		?COMMAND_ID_BIND_RECEIVER_RESP ->
			{bind_receiver_resp, Status, SeqNum, Body};
		?COMMAND_ID_BIND_TRANSMITTER_RESP ->
			{bind_transmitter_resp, Status, SeqNum, Body};
		?COMMAND_ID_BIND_TRANSCEIVER_RESP ->
			{bind_transceiver_resp, Status, SeqNum, Body};
		?COMMAND_ID_UNBIND_RESP ->
			{unbind_resp, Status, SeqNum, Body};
		_ ->
			{CmdId, Status, SeqNum, Body}
	end.
