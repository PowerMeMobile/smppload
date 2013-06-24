-module(parser_tests).

-include("../src/message.hrl").

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests begin
%% ===================================================================

parse_simple_address_test() ->
	Address = parser:parse_address("375293332211"),
	?assertEqual(#address{addr = "375293332211", ton = ?TON_INTERNATIONAL, npi = ?NPI_ISDN}, Address).

parse_full_address_test() ->
	Address = parser:parse_address("FromBank,5,0"),
	?assertEqual(#address{addr = "FromBank", ton = ?TON_ALPHANUMERIC, npi = ?NPI_UNKNOWN}, Address).
%% ===================================================================
%% Tests end
%% ===================================================================
