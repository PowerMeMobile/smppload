-module(smppload_stats).

-export([
    init/0,
    deinit/0,

    %% increments
    incr_send_succ/1,
    incr_send_fail/1,
    incr_dlr_succ/1,
    incr_dlr_fail/1,
    incr_incomings/1,
    incr_errors/1,

    % totals
    send_succ/0,
    send_fail/0,
    dlr_succ/0,
    dlr_fail/0,
    incomings/0,
    errors/0
]).

-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec init() -> ok.
init() ->
    counters = ets:new(counters, [named_table, public]),
    ets:insert_new(counters, [
        {send_succ, 0},
        {send_fail, 0},
        {dlr_succ, 0},
        {dlr_fail, 0},
        {incomings, 0},
        {errors, 0}
    ]),
    ok.

-spec deinit() -> ok.
deinit() ->
    true = ets:delete(counters),
    ok.

-spec incr_send_succ(integer()) -> integer().
incr_send_succ(Incr) ->
    incr(send_succ, Incr).

-spec incr_send_fail(integer()) -> integer().
incr_send_fail(Incr) ->
    incr(send_fail, Incr).

-spec incr_dlr_succ(integer()) -> integer().
incr_dlr_succ(Incr) ->
    incr(dlr_succ, Incr).

-spec incr_dlr_fail(integer()) -> integer().
incr_dlr_fail(Incr) ->
    incr(dlr_fail, Incr).

-spec incr_incomings(integer()) -> integer().
incr_incomings(Incr) ->
    incr(incomings, Incr).

-spec incr_errors(integer()) -> integer().
incr_errors(Incr) ->
    incr(errors, Incr).

-spec send_succ() -> integer().
send_succ() ->
    counter(send_succ).

-spec send_fail() -> integer().
send_fail() ->
    counter(send_fail).

-spec dlr_succ() -> integer().
dlr_succ() ->
    counter(dlr_succ).

-spec dlr_fail() -> integer().
dlr_fail() ->
    counter(dlr_fail).

-spec incomings() -> integer().
incomings() ->
    counter(incomings).

-spec errors() -> integer().
errors() ->
    counter(errors).

%% ===================================================================
%% Internal
%% ===================================================================

incr(Key, Incr) ->
    ets:update_counter(counters, Key, Incr).

counter(Key) ->
    [{Key, Value}] = ets:lookup(counters, Key),
    Value.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

-spec test() -> ok | {error, term()}.

-spec empty_test() -> ok | {error, term()}.
empty_test() ->
    ok = init(),
    ?assertEqual(0, send_succ()),
    ?assertEqual(0, send_fail()),
    ?assertEqual(0, dlr_succ()),
    ?assertEqual(0, dlr_fail()),
    ?assertEqual(0, incomings()),
    ?assertEqual(0, errors()),
    ok = deinit().

-spec incr_test() -> ok | {error, term()}.
incr_test() ->
    ok = init(),
    incr_send_succ(1),
    incr_send_fail(1),
    incr_dlr_succ(1),
    incr_dlr_fail(1),
    incr_incomings(1),
    incr_errors(1),
    ?assertEqual(1, send_succ()),
    ?assertEqual(1, send_fail()),
    ?assertEqual(1, dlr_succ()),
    ?assertEqual(1, dlr_fail()),
    ?assertEqual(1, incomings()),
    ?assertEqual(1, errors()),
    ok = deinit().
-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
