-module(smppload_utils).

-export([
    process_address/1,
    encode/2,
    max_msg_seg/1
]).

-include("message.hrl").
-include("smppload.hrl").

-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
-endif.

-type utf8_list()    :: [byte()].
-type encoded_list() :: [byte()].
-type data_coding()  :: non_neg_integer().
-type msg_len()      :: non_neg_integer().
-type seg_len()      :: non_neg_integer().

%% ===================================================================
%% API
%% ===================================================================

-spec process_address(#address{}) -> #address{}.
process_address(Address = #address{
    addr = #rand_addr{prefix = Prefix, rand_len = RandLen}
}) ->
    Addr = Prefix ++ smppload_random:get_digit_string(RandLen),
    Address#address{addr = Addr};
process_address(Address) ->
    Address.

-spec encode(utf8_list(), data_coding()) -> encoded_list().
encode(Utf8, DC) when DC =:= 0; DC =:= 16; DC =:= 240 ->
    %% to gsm0338.
    Utf8Bin = list_to_binary(Utf8),
    case gsm0338:from_utf8(Utf8Bin) of
        {valid, EncodedBin} ->
            binary_to_list(EncodedBin);
        {invalid, _} ->
            ?ABORT("Impossible to convert '~s' to data coding: ~p~n", [Utf8, DC])
    end;
encode(Utf8, DC) when DC =:= 1; DC =:= 3 ->
    %% to ascii or latin1.
    Utf8Bin = list_to_binary(Utf8),
    case unicode:characters_to_binary(Utf8Bin, utf8, latin1) of
        {_, _, _} ->
            ?ABORT("Impossible to convert '~s' to data coding: ~p~n", [Utf8, DC]);
        EncodedBin ->
            binary_to_list(EncodedBin)
    end;
encode(Utf8, DC) when DC =:= 8; DC =:= 24 ->
    %% to ucs2-be.
    Utf8Bin = list_to_binary(Utf8),
    case unicode:characters_to_binary(Utf8Bin, utf8, {utf16, big}) of
        {_, _, _} ->
            ?ABORT("Impossible to convert '~s' to data coding: ~p~n", [Utf8, DC]);
        EncodedBin ->
            binary_to_list(EncodedBin)
    end;
encode(Utf8, _DC) ->
    Utf8.

-spec max_msg_seg(data_coding()) -> {msg_len(), seg_len()}.
max_msg_seg(DC) when DC =:= 0; DC =:= 1 ->
    {160, 153};
max_msg_seg(DC) when DC =:= 8; DC =:= 24 ->
    {140, 134};
max_msg_seg(_DC) ->
    {140, 134}.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

-spec test() -> ok | {error, term()}.

-spec process_address_undefined_addr_test() -> ok | {error, term()}.
process_address_undefined_addr_test() ->
    Address = #address{
        addr = undefined,
        ton = 1,
        npi = 2
    },
    Expected = #address{
        addr = undefined,
        ton = 1,
        npi = 2
    },
    ?assertEqual(Expected, process_address(Address)).

-spec process_address_normal_addr_test() -> ok | {error, term()}.
process_address_normal_addr_test() ->
    Address = #address{
        addr = "1234567890",
        ton = 1,
        npi = 2
    },
    Expected = #address{
        addr = "1234567890",
        ton = 1,
        npi = 2
    },
    ?assertEqual(Expected, process_address(Address)).

-spec process_address_rand_addr_test() -> ok | {error, term()}.
process_address_rand_addr_test() ->
    {ok, _} = smppload_random:start_link({1, 1, 1}),
    Address = #address{
        addr = #rand_addr{prefix = "12345", rand_len = 5},
        ton = 1,
        npi = 2
    },
    Expected = #address{
        addr = "1234529204",
        ton = 1,
        npi = 2
    },
    ?assertEqual(Expected, process_address(Address)),
    ok = smppload_random:stop().

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
