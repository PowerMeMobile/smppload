-module(smppload_utils).

-export([
    process_address/1,
    encode/2,
    encode/3,
    encode_or_abort/4,
    max_msg_seg/1
]).

-include("message.hrl").
-include("smppload.hrl").
-include_lib("oserl/include/oserl.hrl").

-type utf8_list()    :: [byte()].
-type encoded_list() :: [byte()].
-type data_coding()  :: non_neg_integer().
-type esm_class()    :: non_neg_integer().
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

-spec encode(utf8_list(), data_coding(), esm_class()) ->
    {ok, encoded_list()} | error.
encode(Body, DC, EC) ->
    case udhi_value(EC) of
        false ->
            encode(Body, DC);
        true ->
            unhexdump_and_encode(Body, DC)
    end.

-spec encode(utf8_list(), data_coding()) ->
    {ok, encoded_list()} | error.
encode(Utf8, DC) when DC =:= 0; DC =:= 16; DC =:= 240 ->
    %% to gsm0338.
    case gsm0338:from_utf8(Utf8) of
        {valid, EncodedBin} ->
            {ok, binary_to_list(EncodedBin)};
        {invalid, _} ->
            error
    end;
encode(Utf8, DC) when DC =:= 1; DC =:= 3 ->
    %% to ascii or latin1.
    case unicode:characters_to_binary(Utf8, utf8, latin1) of
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
    case unicode:characters_to_binary(Utf8, utf8, {utf16, big}) of
        {_, _, _} ->
            error;
        EncodedBin ->
            {ok, binary_to_list(EncodedBin)}
    end;
encode(Utf8, _DC) ->
    {ok, Utf8}.

-spec encode_or_abort(utf8_list(), body_format(), data_coding(), esm_class()) ->
    encoded_list() | no_return().
encode_or_abort(Utf8, default, DC, EC) ->
    case encode(Utf8, DC, EC) of
        {ok, Encoded} ->
            Encoded;
        error ->
            ?ABORT("Impossible to convert '~s' to data coding: ~p, esm_class: ~p~n",
                [Utf8, DC, EC])
    end;
encode_or_abort(Hexdump, hexdump, _DC, _EC) ->
    try ac_hexdump:hexdump_to_list(Hexdump)
    catch
        _:_ ->
            ?ABORT("Impossible to convert '~s' to hexdump~n", [Hexdump])
    end.

-spec max_msg_seg(data_coding()) -> {msg_len(), seg_len()}.
max_msg_seg(DC) when DC =:= 0; DC =:= 1 ->
    {160, 153};
max_msg_seg(DC) when DC =:= 8; DC =:= 24 ->
    {140, 134};
max_msg_seg(_DC) ->
    {140, 134}.

%% ===================================================================
%% Internal
%% ===================================================================

udhi_value(EsmClass)
  when (EsmClass band ?ESM_CLASS_GSM_UDHI) == ?ESM_CLASS_GSM_UDHI ->
    true;
udhi_value(_EsmClass) ->
    false.

unhexdump_and_encode(Body, DC) ->
    try
        [F,S|_] = Body,
        [UDHLen] = ac_hexdump:hexdump_to_list([F,S]),
        %% for "06050415811581abc" would be {"06050415811581", "abc"}
        {UDH, Body2} = lists:split((UDHLen+1)*2, Body),
        UDHEnc = ac_hexdump:hexdump_to_list(UDH),
        {ok, Body2Enc} = encode(Body2, DC),
        %% !!! It's over-simplification here !!!
        %% For DC=0 (and probably 1) there MUST be 1 filling bit
        %% between UDH and Encoded Body as described in
        %% http://stackoverflow.com/questions/21098643/smpp-submit-long-message-and-message-split/21121353#21121353
        %% But since the gsm0338 library doesn't seem to do 7-bit packing,
        %% I have no idea how to implement it correctly right now.
        {ok, UDHEnc ++ Body2Enc}
    catch
        _:_ ->
            error
    end.
