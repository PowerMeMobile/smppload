-module(smppload_parser_tests).

-include("../src/message.hrl").

-include_lib("eunit/include/eunit.hrl").
-spec test() -> ok | {error, term()}.

%% ===================================================================
%% Tests begin
%% ===================================================================

-spec parse_simple_address_test() -> ok | {error, term()}.
parse_simple_address_test() ->
    Address = smppload_parser:parse_address("375293332211"),
    ?assertEqual(#address{addr = "375293332211", ton = 1, npi = 1}, Address).

-spec parse_random_address_with_prefix_test() -> ok | {error, term()}.
parse_random_address_with_prefix_test() ->
    Address = smppload_parser:parse_address("37529:7"),

    Expected = #address{
        addr = #rand_addr{prefix = "37529", rand_len = 7},
        ton = 1,
        npi = 1
    },
    ?assertEqual(Expected, Address).

-spec parse_random_address_without_prefix_test() -> ok | {error, term()}.
parse_random_address_without_prefix_test() ->
    Address = smppload_parser:parse_address(":12"),

    Expected = #address{
        addr = #rand_addr{prefix = "", rand_len = 12},
        ton = 1,
        npi = 1
    },
    ?assertEqual(Expected, Address).

-spec parse_full_address_test() -> ok | {error, term()}.
parse_full_address_test() ->
    Address = smppload_parser:parse_address("FromBank,5,0"),
    ?assertEqual(#address{addr = "FromBank", ton = 5, npi = 0}, Address).

-spec parse_delivery_test() -> ok | {error, term()}.
parse_delivery_test() ->
    ?assertNot(smppload_parser:parse_delivery("0")),
    ?assertNot(smppload_parser:parse_delivery("false")),
    ?assert(smppload_parser:parse_delivery("1")),
    ?assert(smppload_parser:parse_delivery("true")).

-spec parse_message_without_ton_npi_test() -> ok | {error, term()}.
parse_message_without_ton_npi_test() ->
    String = "375293332211;375291112233;Hello there!;0;3",
    Message = smppload_parser:parse_message(String),

    Expected = #message{
        source = #address{addr = "375293332211", ton = 1, npi = 1},
        destination = #address{addr = "375291112233", ton = 1, npi = 1},
        body = "Hello there!",
        delivery = false,
        data_coding = 3
    },
    ?assertEqual(Expected, Message).

-spec parse_full_message_test() -> ok | {error, term()}.
parse_full_message_test() ->
    String = "FromBank,5,0;375291112233,2,3;We want our money back, looser!;true;3",
    Message = smppload_parser:parse_message(String),

    Expected = #message{
        source = #address{addr = "FromBank", ton = 5, npi = 0},
        destination = #address{addr = "375291112233", ton = 2, npi = 3},
        body = "We want our money back, looser!",
        delivery = true,
        data_coding = 3
    },
    ?assertEqual(Expected, Message).

-spec parse_message_with_double_semicolon_test() -> ok | {error, term()}.
parse_message_with_double_semicolon_test() ->
    String = "375293332211;375291112233;Hello here;; there!;0;3",
    Message = smppload_parser:parse_message(String),

    Expected = #message{
        source = #address{addr = "375293332211", ton = 1, npi = 1},
        destination = #address{addr = "375291112233", ton = 1, npi = 1},
        body = "Hello here; there!",
        delivery = false,
        data_coding = 3
    },
    ?assertEqual(Expected, Message).

-spec parse_message_without_source_test() -> ok | {error, term()}.
parse_message_without_source_test() ->
    String = ";375291112233;Hello there!;0;3",
    Message = smppload_parser:parse_message(String),

    Expected = #message{
        source = undefined,
        destination = #address{addr = "375291112233", ton = 1, npi = 1},
        body = "Hello there!",
        delivery = false,
        data_coding = 3
    },
    ?assertEqual(Expected, Message).

%% ===================================================================
%% Tests end
%% ===================================================================
