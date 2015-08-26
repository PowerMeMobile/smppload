-module(smppload_utils_tests).

-include("../src/message.hrl").

-include_lib("eunit/include/eunit.hrl").
-spec test() -> ok | {error, term()}.

%% ===================================================================
%% Tests begin
%% ===================================================================

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
    ?assertEqual(Expected, smppload_utils:process_address(Address)).

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
    ?assertEqual(Expected, smppload_utils:process_address(Address)).

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
    ?assertEqual(Expected, smppload_utils:process_address(Address)),
    ok = smppload_random:stop().

-spec encode_gsm0338_test() -> ok | {error, term()}.
encode_gsm0338_test() ->
    Inp = "[@£$¥€]|{^abc~}",
    Exp = [27,60,0,1,2,3,27,101,27,62,27,64,27,40,27,20,97,98,99,27,61,27,41],
    ?assertEqual({ok, Exp}, smppload_utils:encode(Inp, 0)).

-spec encode_ascii_latin1_test() -> ok | {error, term()}.
encode_ascii_latin1_test() ->
    ?assertEqual(error, smppload_utils:encode("£¥€", 1)),
    ?assertEqual(error, smppload_utils:encode("£¥€", 3)),
    Inp = "[@$]|{^abc~}",
    ?assertEqual({ok, Inp}, smppload_utils:encode(Inp, 1)),
    ?assertEqual({ok, Inp}, smppload_utils:encode(Inp, 3)).

-spec encode_binary_test() -> ok | {error, term()}.
encode_binary_test() ->
    Inp1 = "616263",
    Exp1 = "abc",
    ?assertEqual({ok, Exp1}, smppload_utils:encode(Inp1, 2)),
    ?assertEqual({ok, Exp1}, smppload_utils:encode(Inp1, 4)),

    Inp2 = "0616263",
    ?assertEqual(error, smppload_utils:encode(Inp2, 2)),
    ?assertEqual(error, smppload_utils:encode(Inp2, 4)),

    Inp3 = "5b40245d7c7b5e6162637e7d",
    Exp3 = "[@$]|{^abc~}",
    ?assertEqual({ok, Exp3}, smppload_utils:encode(Inp3, 2)),
    ?assertEqual({ok, Exp3}, smppload_utils:encode(Inp3, 4)).

-spec encode_ucs2_test() -> ok | {error, term()}.
encode_ucs2_test() ->
    Inp1 = "АБВ123",
    Exp1 = [4,16,4,17,4,18,0,49,0,50,0,51],
    ?assertEqual({ok, Exp1}, smppload_utils:encode(Inp1, 8)),
    ?assertEqual({ok, Exp1}, smppload_utils:encode(Inp1, 24)).

-spec encode_udh_body_test() -> ok | {error, term()}.
encode_udh_body_test() ->
    %% concat 8-bit
    Inp1 = "050003370201abc",
    Exp1 = [5,0,3,55,2,1,97,98,99],
    ?assertEqual({ok, Exp1}, smppload_utils:encode(Inp1, 3, 64)),
    %% concat 16-bit
    Inp2 = "06080400370201abc",
    Exp2 = [6,8,4,0,55,2,1,97,98,99],
    ?assertEqual({ok, Exp2}, smppload_utils:encode(Inp2, 3, 64)),
    %% port addr 8-bit
    Inp3 = "0404021515abc",
    Exp3 = [4,4,2,21,21,97,98,99],
    ?assertEqual({ok, Exp3}, smppload_utils:encode(Inp3, 3, 64)),
    %% port addr 16bit
    Inp4 = "06050415811581abc",
    Exp4 = [6,5,4,21,129,21,129,97,98,99],
    ?assertEqual({ok, Exp4}, smppload_utils:encode(Inp4, 3, 64)).

%% ===================================================================
%% Tests end
%% ===================================================================
