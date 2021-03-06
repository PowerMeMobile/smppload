-module(smppload_lazy_messages_file_tests).

-include("../src/smppload_lazy_messages.hrl").
-include("../src/message.hrl").

-include_lib("eunit/include/eunit.hrl").
-spec test() -> ok | {error, term()}.

%% ===================================================================
%% Tests begin
%% ===================================================================

-spec file_test() -> ok | {error, term()}.
file_test() ->
    Config = [{file, "./test/messages.test"}, {body_format, default}],
    {ok, State0} = smppload_lazy_messages_file:init(Config),

    %% normal message
    {ok, Msg1, State1} = smppload_lazy_messages_file:get_next(State0),
    #message{source = Source1, destination = Destination1, body = Body1, delivery = Delivery1} = Msg1,
    ?assertEqual(#address{addr = "FromBank", ton = 5, npi = 0}, Source1),
    ?assertEqual(#address{addr = "375293332211", ton = 1, npi = 1}, Destination1),
    ?assertEqual("Return our money, looser!", Body1),
    ?assert(Delivery1),

    %% message with some spaces in front
    {ok, Msg2, State2} = smppload_lazy_messages_file:get_next(State1),
    #message{source = Source2, destination = Destination2, body = Body2, delivery = Delivery2} = Msg2,
    ?assertEqual(#address{addr = "375293332211", ton = 1, npi = 1}, Source2),
    ?assertEqual(#address{addr = "375291112233", ton = 1, npi = 1}, Destination2),
    ?assertEqual("Message 2", Body2),
    ?assertNot(Delivery2),

    %% message with double semicolon in the body
    {ok, Msg3, State3} = smppload_lazy_messages_file:get_next(State2),
    #message{source = Source3, destination = Destination3, body = Body3, delivery = Delivery3} = Msg3,
    ?assertEqual(#address{addr = "375293332211", ton = 1, npi = 1}, Source3),
    ?assertEqual(#address{addr = "375291112233", ton = 1, npi = 1}, Destination3),
    ?assertEqual("Hello here; there!", Body3),
    ?assertNot(Delivery3),

    %% message with an empty source
    {ok, Msg4, State4} = smppload_lazy_messages_file:get_next(State3),
    #message{source = Source4, destination = Destination4, body = Body4, delivery = Delivery4} = Msg4,
    ?assertEqual(undefined, Source4),
    ?assertEqual(#address{addr = "375291112233", ton = 1, npi = 1}, Destination4),
    ?assertEqual("Message 4", Body4),
    ?assertNot(Delivery4),

    %% long message part1
    {ok, Msg5, State5} = smppload_lazy_messages_file:get_next(State4),
    #message{body = Body5, esm_class = Class5} = Msg5,
    Beginning = "EB8Muy7hqOSmO6vVc694aqNBCOr75gNg8wDikK7IoMak7L4HdTOtMiZFyTegBP"
                "Xc2N8pujGaCjFkCQX0whI0yByj8kY0g0MpHYbtFfPI9O9OELzxKrb3upTjB9r2"
                "pNoj2MqQbW",
    %?debugFmt("~p~n", [Beginning]),
    %?debugFmt("~p~n", [Body5]),
    ?assertMatch([_, _, _, _, _, _ | Beginning], Body5),
    ?assert(Class5 =:= 64),

    %% long message part2
    {ok, Msg6, State6} = smppload_lazy_messages_file:get_next(State5),
    #message{body = Body6, esm_class = Class6} = Msg6,
    Ending = "WHTdYwo9IWftCc204rIYyYPTRtPlesn",
    %?debugFmt("~p~n", [Ending]),
    %?debugFmt("~p~n", [Body6]),
    ?assertMatch([_, _, _, _, _, _ | Ending], Body6),
    ?assert(Class6 =:= 64),

    %% UDH + message
    {ok, Msg7, State7} = smppload_lazy_messages_file:get_next(State6),
    #message{body = Body7, esm_class = Class7} = Msg7,
    %?debugFmt("~p~n", [Body7]),
    %?debugFmt("~p~n", [Class7]),
    ?assertMatch([6,5,4,21,129,21,129,97,98,99], Body7),
    ?assert(Class7 =:= 64),

    {no_more, State8} = smppload_lazy_messages_file:get_next(State7),

    ok = smppload_lazy_messages_body:deinit(State8).

-spec file_dos_bom_test() -> ok | {error, term()}.
file_dos_bom_test() ->
    Config = [{file, "./test/messages_utf8_dos_bom.txt"}, {body_format, default}],
    {ok, State0} = smppload_lazy_messages_file:init(Config),

    {ok, Msg1, State1} = smppload_lazy_messages_file:get_next(State0),
    #message{source = Source1} = Msg1,
    ?assertEqual(#address{addr = "uuu", ton = 1, npi = 1}, Source1),

    {ok, Msg2, State2} = smppload_lazy_messages_file:get_next(State1),
    #message{source = Source2} = Msg2,
    ?assertEqual(#address{addr = "uuu", ton = 1, npi = 1}, Source2),

    {no_more, State3} = smppload_lazy_messages_file:get_next(State2),

    ok = smppload_lazy_messages_body:deinit(State3).

-spec file_hexdump_test() -> ok | {error, term()}.
file_hexdump_test() ->
    Config = [{file, "./test/messages_hexdump.txt"}, {body_format, hexdump}],
    {ok, State0} = smppload_lazy_messages_file:init(Config),

    {ok, Msg1, State1} = smppload_lazy_messages_file:get_next(State0),
    #message{body = Body1} = Msg1,
    ?assertEqual("Hello from hexdump!", Body1),

    {ok, Msg2, State2} = smppload_lazy_messages_file:get_next(State1),
    #message{body = Body2} = Msg2,
    ?assertEqual("GSM © 0338", Body2),

    {no_more, State3} = smppload_lazy_messages_file:get_next(State2),

    ok = smppload_lazy_messages_body:deinit(State3).

%% ===================================================================
%% Tests end
%% ===================================================================
