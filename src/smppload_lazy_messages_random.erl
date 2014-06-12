-module(smppload_lazy_messages_random).

-behaviour(smppload_lazy_messages).

-export([
    init/1,
    deinit/1,
    get_next/1
]).

-include("smppload_lazy_messages.hrl").
-include("message.hrl").
-include("smppload.hrl").
-include_lib("oserl/include/smpp_globals.hrl").

-record(state, {
    source,
    destination,
    count,
    length,
    max_msg_len,
    max_seg_len,
    delivery,
    data_coding,
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
            undefined ->
                undefined;
            Address ->
                smppload_parser:parse_address(Address)
        end,
    Destination = smppload_parser:parse_address(?gv(destination, Config)),
    Count = ?gv(count, Config),
    Delivery = ?gv(delivery, Config),
    DataCoding = ?ENCODING_SCHEME_LATIN_1,
    {MaxMsgLen, MaxSegLen} = smppload_utils:max_msg_seg(DataCoding),
    Length = ?gv(length, Config, MaxMsgLen),
    {ok, #state{
        source = Source,
        destination = Destination,
        count = Count,
        length = Length,
        max_msg_len = MaxMsgLen,
        max_seg_len = MaxSegLen,
        delivery = Delivery,
        data_coding = DataCoding
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
    count = Count,
    length = Length,
    max_msg_len = MaxMsgLen,
    delivery = Delivery,
    data_coding = DataCoding
}) when Length =< MaxMsgLen ->
    Body = smppload_random:get_alnum_string(Length),
    Message = #message{
        source = smppload_utils:process_address(Source),
        destination = smppload_utils:process_address(Destination),
        body = Body,
        delivery = Delivery,
        data_coding = DataCoding
    },
    {ok, Message, State#state{count = Count - 1}};
get_next(State = #state{
    source = Source,
    destination = Destination,
    count = Count,
    length = Length,
    max_seg_len = MaxSegLen,
    delivery = Delivery,
    data_coding = DataCoding,
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
            Body = smppload_random:get_alnum_string(Length),
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
            {ok, Message, State#state{count = Count - 1, parts = Parts1}}
    end.
