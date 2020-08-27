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
    %% NB: Random algorithm (exsplus) and seed dependent results
    Config = [{source, "s"}, {destination, "d"}, {delivery, true}, {count, 3}, {length, 5}],
    {ok, _} = smppload_random:start_link({1, 1, 1}),
    {ok, State0} = smppload_lazy_messages_random:init(Config),
    {ok, Msg, State1} = smppload_lazy_messages_random:get_next(State0),
    #message{source = Source, destination = Destination, body = Body1, delivery = Delivery} = Msg,
    ?assertEqual(#address{addr = "s", ton = 1, npi = 1}, Source),
    ?assertEqual(#address{addr = "d", ton = 1, npi = 1}, Destination),
    ?assert(Delivery),
    ?assertEqual("oSygM", Body1),
    {ok, #message{body = Body2}, State2} = smppload_lazy_messages_random:get_next(State1),
    ?assertEqual("MZeZK", Body2),
    {ok, #message{body = Body3}, State3} = smppload_lazy_messages_random:get_next(State2),
    ?assertEqual("ygXvs", Body3),
    {no_more, State4} = smppload_lazy_messages_random:get_next(State3),
    ok = smppload_lazy_messages_random:deinit(State4),
    ok = smppload_random:stop().

-spec long_body_test() -> ok | {error, term()}.
long_body_test() ->
    %% NB: Random algorithm (exsplus) and seed dependent results
    Config = [{source, "s"}, {destination, "d"}, {count, 1}, {length, 165}],
    {ok, _} = smppload_random:start_link({1, 1, 1}),
    {ok, State0} = smppload_lazy_messages_random:init(Config),

    {ok, Msg1, State1} = smppload_lazy_messages_random:get_next(State0),
    #message{body = Body1, esm_class = Class1} = Msg1,
    Beginning = "OQ06qFLPrisLX6yv1IKu3DpQtBZeMcyFYDaWYUPBj1uG8oD8HL24dmPvPki1PC9"
                "LSJmLwPa6XoLrPc7fItq5kojSkeiU4yIAtwJqRt88o0LYFtjYwE7TOcaRpAGJya"
                "90KBndRx",
    %?debugFmt("~p~n", [Beginning]),
    %?debugFmt("~p~n", [Body1]),
    ?assertMatch([_, _, _, _, _, _ | Beginning], Body1),
    ?assert(Class1 =/= 0),

    {ok, Msg2, State2} = smppload_lazy_messages_random:get_next(State1),
    #message{body = Body2, esm_class = Class2} = Msg2,
    Ending = "httupEBZBBbmFR64ygXvsMZeZKoSygM",
    %?debugFmt("~p~n", [Ending]),
    %?debugFmt("~p~n", [Body2]),
    ?assertMatch([_, _, _, _, _, _ | Ending], Body2),
    ?assert(Class2 =/= 0),

    {no_more, State3} = smppload_lazy_messages_random:get_next(State2),
    ok = smppload_lazy_messages_body:deinit(State3),
    ok = smppload_random:stop().

-spec no_source_test() -> ok | {error, term()}.
no_source_test() ->
    Config = [{source, ""}, {destination, "d"}, {body, "b"}, {delivery, true}, {count, 1}],
    {ok, _} = smppload_random:start_link({1, 1, 1}),
    {ok, State0} = smppload_lazy_messages_random:init(Config),
    {ok, Msg, State1} = smppload_lazy_messages_random:get_next(State0),
    #message{source = Source} = Msg,
    ?assertEqual("", Source),
    {no_more, State2} = smppload_lazy_messages_random:get_next(State1),
    ok = smppload_lazy_messages_random:deinit(State2),
    ok = smppload_random:stop().

%% ===================================================================
%% Tests end
%% ===================================================================
