-module(lazy_messages_body_tests).

-include("../src/lazy_messages.hrl").
-include("../src/message.hrl").

-include_lib("eunit/include/eunit.hrl").
-spec test() -> ok | {error, term()}.

%% ===================================================================
%% Tests begin
%% ===================================================================

-spec full_test() -> ok | {error, term()}.
full_test() ->
	Config = [{source, "s"}, {destination, "d"}, {body, "b"}, {delivery, true}, {count, 3}],
	{ok, State0} = lazy_messages_body:init(Config),
	{ok, Msg, State1} = lazy_messages_body:get_next(State0),
	#message{source = Source, destination = Destination, body = Body, delivery = Delivery} = Msg,
	?assertEqual(#address{addr = "s", ton = 1, npi = 1}, Source),
	?assertEqual(#address{addr = "d", ton = 1, npi = 1}, Destination),
	?assertEqual("b", Body),
	?assert(Delivery),
	{ok, Msg, State2} = lazy_messages_body:get_next(State1),
	{ok, Msg, State3} = lazy_messages_body:get_next(State2),
	{no_more, State4} = lazy_messages_body:get_next(State3),
	ok = lazy_messages_body:deinit(State4).

-spec no_source_test() -> ok | {error, term()}.
no_source_test() ->
	Config = [{destination, "d"}, {body, "b"}, {delivery, true}, {count, 1}],
	{ok, State0} = lazy_messages_body:init(Config),
	{ok, Msg, State1} = lazy_messages_body:get_next(State0),
	#message{source = Source} = Msg,
	?assertEqual(undefined, Source),
	{no_more, State2} = lazy_messages_body:get_next(State1),
	ok = lazy_messages_body:deinit(State2).

%% ===================================================================
%% Tests end
%% ===================================================================
