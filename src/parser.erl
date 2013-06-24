-module(parser).

-export([
	parse_address/1
]).

-include("message.hrl").
-include_lib("oserl/include/smpp_globals.hrl").

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
