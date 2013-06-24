-module(lazy_messages_file_tests).

-include("../src/lazy_messages.hrl").
-include("../src/message.hrl").

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests begin
%% ===================================================================

normal_test() ->
	Config = [{file, "../test/messages.test"}],
	{ok, State0} = lazy_messages_file:init(Config),

	{ok, Msg1, State1} = lazy_messages_file:get_next(State0),
	#message{source = Source1, destination = Destination1, body = Body1, delivery = Delivery1} = Msg1,
	?assertEqual(#address{addr = "375293332211", ton = 1, npi = 1}, Source1),
	?assertEqual(#address{addr = "375291112233", ton = 1, npi = 1}, Destination1),
	?assertEqual("Hello here; there!", Body1),
	?assertNot(Delivery1),

	{ok, Msg2, State2} = lazy_messages_file:get_next(State1),
	#message{source = Source2, destination = Destination2, body = Body2, delivery = Delivery2} = Msg2,
	?assertEqual(#address{addr = "FromBank", ton = 5, npi = 0}, Source2),
	?assertEqual(#address{addr = "375293332211", ton = 1, npi = 1}, Destination2),
	?assertEqual("Return our money, looser!", Body2),
	?assert(Delivery2),

	{no_more, State3} = lazy_messages_file:get_next(State2),

	ok = lazy_messages_body:deinit(State3).

%% ===================================================================
%% Tests end
%% ===================================================================
