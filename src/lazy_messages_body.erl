-module(lazy_messages_body).

-behaviour(lazy_messages).

-export([
	init/1,
	deinit/1,
	get_next/1
]).

-include("lazy_messages.hrl").
-include("message.hrl").

-record(state, {
	source,
	destination,
	body,
	count,
	delivery
}).

%% ===================================================================
%% API
%% ===================================================================

-spec init(config()) -> {ok, state()}.
init(Config) ->
	Source =
		case proplists:get_value(source, Config) of
			undefined ->
				undefined;
			Address ->
				parser:parse_address(Address)
		end,
	Destination = parser:parse_address(proplists:get_value(destination, Config)),
	Body = proplists:get_value(body, Config),
	Count = proplists:get_value(count, Config),
	Delivery = proplists:get_value(delivery, Config),
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
get_next(State = #state{count = Count}) when Count =< 0 ->
	{no_more, State};
get_next(State = #state{
	source = Source,
	destination = Destination,
	body = Body,
	count = Count,
	delivery = Delivery
}) ->
	Message = #message{
		source = Source,
		destination = Destination,
		body = Body,
		delivery = Delivery
	},
	{ok, Message, State#state{count = Count - 1}}.
