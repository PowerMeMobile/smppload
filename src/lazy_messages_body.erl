-module(lazy_messages_body).

-behaviour(lazy_messages).

-export([
	init/1,
	deinit/1,
	get_next/1
]).

-include("lazy_messages.hrl").
-include("submit_message.hrl").

%-define(TEST, 1).
-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
	source,
	destination,
	body,
	count,
	delivery
}).

-spec init(config()) -> state().
init(Config) ->
	Source = proplists:get_value(source, Config),
	Destination = proplists:get_value(destination, Config),
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

-spec get_next(state()) -> {ok, #submit_message{}, state()} | {no_more, state()}.
get_next(State = #state{count = Count}) when Count =< 0 ->
	{no_more, State};
get_next(State = #state{
	source = Source,
	destination = Destination,
	body = Body,
	count = Count,
	delivery = Delivery
}) ->
	Message = #submit_message{
		source = Source,
		destination = Destination,
		body = Body,
		delivery = Delivery
	},
	{ok, Message, State#state{count = Count - 1}}.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

one_test() ->
	Config = [{source, "s"}, {destination, "d"}, {body, "b"}, {delivery, true}, {count, 3}],
	{ok, State0} = lazy_messages_body:init(Config),
	{ok, Msg, State1} = lazy_messages_body:get_next(State0),
	#submit_message{source = Source, destination = Destination, body = Body, delivery = Delivery} = Msg,
	?assertEqual("s", Source),
	?assertEqual("d", Destination),
	?assertEqual("b", Body),
	?assert(Delivery),
	{ok, Msg, State2} = lazy_messages_body:get_next(State1),
	{ok, Msg, State3} = lazy_messages_body:get_next(State2),
	{no_more, State4} = lazy_messages_body:get_next(State3),
	ok = lazy_messages_body:deinit(State4).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
