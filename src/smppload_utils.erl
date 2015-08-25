-module(smppload_utils).

-export([
    process_address/1,
    encode/2,
    encode_abort/2,
    max_msg_seg/1
]).

-include("message.hrl").
-include("smppload.hrl").

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

-spec encode(utf8_list(), data_coding()) -> {ok, encoded_list()} | error.
encode(Utf8, DC) when DC =:= 0; DC =:= 16; DC =:= 240 ->
    %% to gsm0338.
    Utf8Bin = list_to_binary(Utf8),
    case gsm0338:from_utf8(Utf8Bin) of
        {valid, EncodedBin} ->
            {ok, binary_to_list(EncodedBin)};
        {invalid, _} ->
            error
    end;
encode(Utf8, DC) when DC =:= 1; DC =:= 3 ->
    %% to ascii or latin1.
    Utf8Bin = list_to_binary(Utf8),
    case unicode:characters_to_binary(Utf8Bin, utf8, latin1) of
        {_, _, _} ->
            error;
        EncodedBin ->
            {ok, binary_to_list(EncodedBin)}
    end;
encode(Hexdump, DC) when DC =:= 2; DC =:= 4 ->
    try ac_hexdump:hexdump_to_list(Hexdump) of
        Bin ->
            {ok, Bin}
    catch
        _:_ ->
            error
    end;
encode(Utf8, DC) when DC =:= 8; DC =:= 24 ->
    %% to ucs2-be.
    Utf8Bin = list_to_binary(Utf8),
    case unicode:characters_to_binary(Utf8Bin, utf8, {utf16, big}) of
        {_, _, _} ->
            error;
        EncodedBin ->
            {ok, binary_to_list(EncodedBin)}
    end;
encode(Utf8, _DC) ->
    {ok, Utf8}.

-spec encode_abort(utf8_list(), data_coding()) -> encoded_list() | no_return().
encode_abort(Utf8, DC) ->
    case encode(Utf8, DC) of
        {ok, Encoded} ->
            Encoded;
        error ->
            ?ABORT("Impossible to convert '~s' to data coding: ~p~n", [Utf8, DC])
    end.

-spec max_msg_seg(data_coding()) -> {msg_len(), seg_len()}.
max_msg_seg(DC) when DC =:= 0; DC =:= 1 ->
    {160, 153};
max_msg_seg(DC) when DC =:= 8; DC =:= 24 ->
    {140, 134};
max_msg_seg(_DC) ->
    {140, 134}.
