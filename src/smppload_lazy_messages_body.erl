-module(smppload_lazy_messages_body).

-behaviour(smppload_lazy_messages).

-export([
    init/1,
    deinit/1,
    get_next/1
]).

-include("smppload_lazy_messages.hrl").
-include("smppload.hrl").
-include("message.hrl").

-record(state, {
    source,
    destination,
    body,
    count,
    delivery,
    data_coding,
    esm_class,
    max_msg_len,
    max_seg_len,
    %% for long messages
    parts = []
}).

%% ===================================================================
%% API
%% ===================================================================

-spec init(config()) -> {ok, state()}.
init(Config) ->
    Source =
        case ?gv(source, Config) of
            [] ->
                [];
            Address ->
                smppload_parser:parse_address(Address)
        end,
    Destination = smppload_parser:parse_address(?gv(destination, Config)),
    BodyUtf8 = ?gv(body, Config),
    Count = ?gv(count, Config),
    Delivery = ?gv(delivery, Config),
    DataCoding = ?gv(data_coding, Config),
    EsmClass = ?gv(esm_class, Config),
    BodyEncoded = smppload_utils:encode_or_abort(BodyUtf8, DataCoding, EsmClass),
    {MaxMsgLen, MaxSegLen} = smppload_utils:max_msg_seg(DataCoding),
    {ok, #state{
        source = Source,
        destination = Destination,
        body = BodyEncoded,
        count = Count,
        delivery = Delivery,
        data_coding = DataCoding,
        esm_class = EsmClass,
        max_msg_len = MaxMsgLen,
        max_seg_len = MaxSegLen
    }}.

-spec deinit(state()) -> ok.
deinit(_State) ->
    ok.

-spec get_next(state()) -> {ok, #message{}, state()} | {no_more, state()}.
get_next(State = #state{
    count = Count,
    parts = Parts
}) when Count =< 0, length(Parts) =:= 0 ->
    {no_more, State};
get_next(State = #state{
    source = Source,
    destination = Destination,
    body = Body,
    count = Count,
    delivery = Delivery,
    data_coding = DataCoding,
    esm_class = EsmClass,
    max_msg_len = MaxMsgLen,
    parts = []
}) when length(Body) =< MaxMsgLen->
    Message = #message{
        source = smppload_utils:process_address(Source),
        destination = smppload_utils:process_address(Destination),
        body = Body,
        delivery = Delivery,
        data_coding = DataCoding,
        esm_class = EsmClass
    },
    {ok, Message, State#state{count = Count - 1}};
get_next(State = #state{
    source = Source,
    destination = Destination,
    body = Body,
    count = Count,
    delivery = Delivery,
    data_coding = DataCoding,
    max_seg_len = MaxSegLen,
    parts = Parts0
}) ->
    case Parts0 of
        [Part | Parts1] ->
            Message = #message{
                source = smppload_utils:process_address(Source),
                destination = smppload_utils:process_address(Destination),
                body = ?gv(short_message, Part),
                esm_class = ?gv(esm_class, Part),
                delivery = Delivery,
                data_coding = DataCoding
            },
            {ok, Message, State#state{parts = Parts1}};
        [] ->
            RefNum = smppload_ref_num:next(?MODULE),
            [Part | Parts1] =
                smpp_sm:split([{short_message, Body}], RefNum, udh, MaxSegLen),
            Message = #message{
                source = smppload_utils:process_address(Source),
                destination = smppload_utils:process_address(Destination),
                body = ?gv(short_message, Part),
                esm_class = ?gv(esm_class, Part),
                delivery = Delivery,
                data_coding = DataCoding
            },
            {ok, Message, State#state{
                count = Count - 1,
                parts = Parts1
            }}
    end.
