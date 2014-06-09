-module(smppload_lazy_messages_random_tests).

-include("../src/smppload_lazy_messages.hrl").
-include("../src/message.hrl").

-include_lib("eunit/include/eunit.hrl").
-spec test() -> ok | {error, term()}.

%% ===================================================================
%% Tests begin
%% ===================================================================

-spec full_test() -> ok | {error, term()}.
full_test() ->
    Config = [{source, "s"}, {destination, "d"}, {delivery, true}, {count, 3}, {length, 5}],
    {ok, _} = smppload_random:start_link({1, 1, 1}),
    {ok, State0} = smppload_lazy_messages_random:init(Config),
    {ok, Msg, State1} = smppload_lazy_messages_random:get_next(State0),
    #message{source = Source, destination = Destination, body = Body1, delivery = Delivery} = Msg,
    ?assertEqual(#address{addr = "s", ton = 1, npi = 1}, Source),
    ?assertEqual(#address{addr = "d", ton = 1, npi = 1}, Destination),
    ?assert(Delivery),
    ?assertEqual("Plesn", Body1),
    {ok, #message{body = Body2}, State2} = smppload_lazy_messages_random:get_next(State1),
    ?assertEqual("YPTRt", Body2),
    {ok, #message{body = Body3}, State3} = smppload_lazy_messages_random:get_next(State2),
    ?assertEqual("4rIYy", Body3),
    {no_more, State4} = smppload_lazy_messages_random:get_next(State3),
    ok = smppload_lazy_messages_random:deinit(State4),
    ok = smppload_random:stop().

-spec long_body_test() -> ok | {error, term()}.
long_body_test() ->
    Config = [{source, "s"}, {destination, "d"}, {count, 1}, {length, 165}],
    {ok, _} = smppload_random:start_link({1, 1, 1}),
    {ok, State0} = smppload_lazy_messages_random:init(Config),

    {ok, Msg1, State1} = smppload_lazy_messages_random:get_next(State0),
    #message{body = Body1, esm_class = Class1} = Msg1,
    Beginning = "EB8Muy7hqOSmO6vVc694aqNBCOr75gNg8wDikK7IoMak7L4HdTOtMiZFyTegBP"
                "Xc2N8pujGaCjFkCQX0whI0yByj8kY0g0MpHYbtFfPI9O9OELzxKrb3upTjB9r2"
                "pNoj2MqQbW",
    %?debugFmt("~p~n", [Beginning]),
    %?debugFmt("~p~n", [Body1]),
    ?assertMatch([_, _, _, _, _, _ | Beginning], Body1),
    ?assert(Class1 =/= 0),

    {ok, Msg2, State2} = smppload_lazy_messages_random:get_next(State1),
    #message{body = Body2, esm_class = Class2} = Msg2,
    Ending = "WHTdYwo9IWftCc204rIYyYPTRtPlesn",
    %?debugFmt("~p~n", [Ending]),
    %?debugFmt("~p~n", [Body2]),
    ?assertMatch([_, _, _, _, _, _ | Ending], Body2),
    ?assert(Class2 =/= 0),

    {no_more, State3} = smppload_lazy_messages_random:get_next(State2),
    ok = smppload_lazy_messages_body:deinit(State3),
    ok = smppload_random:stop().

-spec no_source_test() -> ok | {error, term()}.
no_source_test() ->
    Config = [{destination, "d"}, {body, "b"}, {delivery, true}, {count, 1}],
    {ok, _} = smppload_random:start_link({1, 1, 1}),
    {ok, State0} = smppload_lazy_messages_random:init(Config),
    {ok, Msg, State1} = smppload_lazy_messages_random:get_next(State0),
    #message{source = Source} = Msg,
    ?assertEqual(undefined, Source),
    {no_more, State2} = smppload_lazy_messages_random:get_next(State1),
    ok = smppload_lazy_messages_random:deinit(State2),
    ok = smppload_random:stop().

%% ===================================================================
%% Tests end
%% ===================================================================
