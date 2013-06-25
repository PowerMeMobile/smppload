-module(parser).

-export([
	parse_address/1,
	parse_delivery/1,
	parse_message/1
]).

-include("pagload.hrl").
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
			#address{addr = Addr, ton = ?TON_INTERNATIONAL, npi = ?NPI_ISDN};
		_ ->
			?ABORT("Bad address: ~p~n", [String])
	end.

-spec parse_delivery(string()) -> true | false.
parse_delivery("0")     -> false;
parse_delivery("false") -> false;
parse_delivery("1")     -> true;
parse_delivery("true")  -> true;
parse_delivery(String)  -> ?ABORT("Bad delivery: ~p~n", [String]).

-spec parse_message(string()) -> #message{}.
parse_message(String) ->
	case parse_message(String, [], []) of
		[[], Destination, Body, Delivery] ->
			#message{
				source = undefined,
				destination = parse_address(Destination),
				body = Body,
				delivery = parse_delivery(Delivery)
			};
		[Source, Destination, Body, Delivery] ->
			#message{
				source = parse_address(Source),
				destination = parse_address(Destination),
				body = Body,
				delivery = parse_delivery(Delivery)
			};
		_ ->
			?ABORT("Bad message: ~p~n", [String])
	end.

%% ===================================================================
%% Internal
%% ===================================================================

parse_message([], Chunk, Chunks) ->
	lists:reverse([lists:reverse(Chunk) | Chunks]);
parse_message([$;,$;,Char | Chars], Chunk, Chunks) ->
	parse_message(Chars, [Char, $; | Chunk], Chunks);
parse_message([$;,Char | Chars], Chunk, Chunks) ->
	parse_message(Chars, [Char], [lists:reverse(Chunk) | Chunks]);
parse_message([Char | Chars], Chunk, Chunks) ->
	parse_message(Chars, [Char | Chunk], Chunks).
