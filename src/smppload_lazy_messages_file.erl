-module(smppload_lazy_messages_file).

-behaviour(smppload_lazy_messages).

-export([
    init/1,
    deinit/1,
    get_next/1
]).

-include("smppload.hrl").
-include("smppload_lazy_messages.hrl").
-include("message.hrl").
-include_lib("oserl/include/oserl.hrl").

-record(state, {
    fd,
    %% for long messages
    parts = [],
    source,
    destination,
    delivery,
    data_coding
}).

%% ===================================================================
%% API
%% ===================================================================

-spec init(config()) -> {ok, state()}.
init(Config) ->
    case string:strip(?gv(file, Config), both) of
        "-" ->
            {ok, #state{fd = standard_io}};
        File ->
            case file:open(File, [read]) of
                {ok, Fd} ->
                    {ok, #state{fd = Fd}};
                {error, Reason} ->
                    ?ABORT("Open file ~p failed with: ~p~n", [File, Reason])
            end
    end.

-spec deinit(state()) -> ok.
deinit(#state{fd = standard_io}) ->
    ok;
deinit(#state{fd = Fd}) ->
    file:close(Fd).

-spec get_next(state()) -> {ok, #message{}, state()} | {no_more, state()}.
get_next(State = #state{fd = Fd, parts = []}) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            case string:strip(string:strip(Line, right, $\n), both) of
                [] ->
                    %% handle empty strings.
                    get_next(State);
                [$# | _] ->
                    %% handle comments.
                    get_next(State);
                Stripped ->
                    Message = smppload_parser:parse_message(Stripped),

                    Source      = Message#message.source,
                    Destination = Message#message.destination,
                    Body        = Message#message.body,
                    Delivery    = Message#message.delivery,
                    DataCoding  = Message#message.data_coding,

                    case length(Body) =< ?MAX_MSG_LEN of
                        true ->
                            {ok, Message, State};
                        false ->
                            RefNum = smppload_ref_num:next(?MODULE),
                            [Part | Parts] =
                                smpp_sm:split([{short_message, Body}], RefNum, udh, ?MAX_SEG_LEN),
                            Message2 = #message{
                                source = smppload_utils:process_address(Source),
                                destination = smppload_utils:process_address(Destination),
                                body = ?gv(short_message, Part),
                                esm_class = ?gv(esm_class, Part),
                                delivery = Delivery,
                                data_coding = DataCoding
                            },
                            {ok, Message2, State#state{
                                parts = Parts,
                                source = Source,
                                destination = Destination,
                                delivery = Delivery,
                                data_coding = DataCoding
                            }}
                    end
            end;
        eof ->
            {no_more, State}
    end;
get_next(State = #state{
    parts = [Part],
    source = Source,
    destination = Destination,
    delivery = Delivery,
    data_coding = DataCoding
}) ->
    Message = #message{
        source = smppload_utils:process_address(Source),
        destination = smppload_utils:process_address(Destination),
        body = ?gv(short_message, Part),
        esm_class = ?gv(esm_class, Part),
        delivery = Delivery,
        data_coding = DataCoding
    },
    {ok, Message, State#state{
        parts = [],
        source = undefined,
        destination = undefined,
        delivery = undefined,
        data_coding = undefined
    }};
get_next(State = #state{
    parts = [Part | Parts],
    source = Source,
    destination = Destination,
    delivery = Delivery,
    data_coding = DataCoding
}) ->
    Message = #message{
        source = smppload_utils:process_address(Source),
        destination = smppload_utils:process_address(Destination),
        body = ?gv(short_message, Part),
        esm_class = ?gv(esm_class, Part),
        delivery = Delivery,
        data_coding = DataCoding
    },
    {ok, Message, State#state{
        parts = Parts
    }}.
