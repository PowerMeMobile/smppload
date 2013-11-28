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
	Body = ?gv(body, Config),
	Count = ?gv(count, Config),
	Delivery = ?gv(delivery, Config),
	{ok, #state{
		source = Source,
		destination = Destination,
		body = Body,
		count = Count,
		delivery = Delivery
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
	parts = []
}) when length(Body) =< ?MAX_MSG_LEN->
	Message = #message{
		source = Source,
		destination = Destination,
		body = Body,
		delivery = Delivery
	},
	{ok, Message, State#state{count = Count - 1}};
get_next(State = #state{
	source = Source,
	destination = Destination,
	body = Body,
	count = Count,
	delivery = Delivery,
	parts = Parts0
}) ->
	case Parts0 of
		[Part | Parts1] ->
			Message = #message{
				source = Source,
				destination = Destination,
				body = ?gv(short_message, Part),
				esm_class = ?gv(esm_class, Part),
				delivery = Delivery
			},
			{ok, Message, State#state{parts = Parts1}};
		[] ->
			RefNum = smppload_ref_num:next(?MODULE),
		    [Part | Parts1] =
				smpp_sm:split([{short_message, Body}], RefNum, udh, ?MAX_SEG_LEN),
			Message = #message{
				source = Source,
				destination = Destination,
				body = ?gv(short_message, Part),
				esm_class = ?gv(esm_class, Part),
				delivery = Delivery
			},
			{ok, Message, State#state{
				count = Count - 1,
				parts = Parts1
			}}
	end.
