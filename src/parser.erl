-module(parser).

-export([
	parse_address/1
]).

-include("message.hrl").
-include_lib("oserl/include/smpp_globals.hrl").

-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec parse_address(string()) -> #address{}.
parse_address(String) ->
	case string:tokens(String, ",") of
		[Addr, Ton, Npi] ->
			#address{addr = Addr, ton = list_to_integer(Ton), npi = list_to_integer(Npi)};
		[Addr] ->
			#address{addr = Addr, ton = ?TON_INTERNATIONAL, npi = ?NPI_ISDN}
	end.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

parse_simple_address_test() ->
	Address = parse_address("375293332211"),
	?assertEqual(#address{addr = "375293332211", ton = ?TON_INTERNATIONAL, npi = ?NPI_ISDN}, Address).

parse_full_address_test() ->
	Address = parse_address("FromBank,5,0"),
	?assertEqual(#address{addr = "FromBank", ton = ?TON_ALPHANUMERIC, npi = ?NPI_UNKNOWN}, Address).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
