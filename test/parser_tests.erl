-module(parser_tests).

-include("../src/message.hrl").

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests begin
%% ===================================================================

parse_simple_address_test() ->
	Address = parser:parse_address("375293332211"),
	?assertEqual(#address{addr = "375293332211", ton = 1, npi = 1}, Address).

parse_full_address_test() ->
	Address = parser:parse_address("FromBank,5,0"),
	?assertEqual(#address{addr = "FromBank", ton = 5, npi = 0}, Address).

parse_delivery_test() ->
	?assertNot(parser:parse_delivery("0")),
	?assertNot(parser:parse_delivery("false")),
	?assert(parser:parse_delivery("1")),
	?assert(parser:parse_delivery("true")).

parse_message_without_ton_npi_test() ->
	String = "375293332211;375291112233;Hello there!;0",
	Message = parser:parse_message(String),

	Expected = #message{
		source = #address{addr = "375293332211", ton = 1, npi = 1},
		destination = #address{addr = "375291112233", ton = 1, npi = 1},
		body = "Hello there!",
		delivery = false
	},
	?assertEqual(Expected, Message).

parse_full_message_test() ->
	String = "FromBank,5,0;375291112233,2,3;We want our money back, looser!;true",
	Message = parser:parse_message(String),

	Expected = #message{
		source = #address{addr = "FromBank", ton = 5, npi = 0},
		destination = #address{addr = "375291112233", ton = 2, npi = 3},
		body = "We want our money back, looser!",
		delivery = true
	},
	?assertEqual(Expected, Message).

parse_message_with_double_semicolon_test() ->
	String = "375293332211;375291112233;Hello here;; there!;0",
	Message = parser:parse_message(String),

	Expected = #message{
		source = #address{addr = "375293332211", ton = 1, npi = 1},
		destination = #address{addr = "375291112233", ton = 1, npi = 1},
		body = "Hello here; there!",
		delivery = false
	},
	?assertEqual(Expected, Message).

parse_message_without_source_test() ->
	String = ";375291112233;Hello there!;0",
	Message = parser:parse_message(String),

	Expected = #message{
		source = undefined,
		destination = #address{addr = "375291112233", ton = 1, npi = 1},
		body = "Hello there!",
		delivery = false
	},
	?assertEqual(Expected, Message).

%% ===================================================================
%% Tests end
%% ===================================================================
