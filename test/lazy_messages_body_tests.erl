-module(lazy_messages_body_tests).

-include("../src/lazy_messages.hrl").
-include("../src/message.hrl").

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests begin
%% ===================================================================

one_test() ->
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

%% ===================================================================
%% Tests end
%% ===================================================================
