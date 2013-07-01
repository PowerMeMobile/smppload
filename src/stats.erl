-module(stats).

-export([
	new/0,

	add/2,

	inc_send_succ/1,
	inc_send_succ/2,
	inc_send_fail/1,
	inc_send_fail/2,

	inc_dlr_succ/1,
	inc_dlr_succ/2,
	inc_dlr_fail/1,
	inc_dlr_fail/2,

	inc_errors/1,
	inc_errors/2,

	inc_rps/2,

	send_succ/1,
	send_fail/1,
	dlr_succ/1,
	dlr_fail/1,
	errors/1,
	rps/1
]).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.

-record(stats, {
	send_succ = 0,
	send_fail = 0,
	dlr_succ = 0,
	dlr_fail = 0,
	errors = 0,
	rps = 0
}).

-opaque stats() :: #stats{}.

%% ===================================================================
%% API
%% ===================================================================

-spec new() -> stats().
new() ->
	#stats{}.

-spec add(stats(), stats()) -> stats().
add(StatsL, StatsR) ->
	#stats{
		send_succ = StatsL#stats.send_succ + StatsR#stats.send_succ,
		send_fail = StatsL#stats.send_fail + StatsR#stats.send_fail,
		dlr_succ = StatsL#stats.dlr_succ + StatsR#stats.dlr_succ,
		dlr_fail = StatsL#stats.dlr_fail + StatsR#stats.dlr_fail,
		errors = StatsL#stats.errors + StatsR#stats.errors
	}.

-spec inc_send_succ(stats()) -> stats().
inc_send_succ(Stats) ->
	inc_send_succ(Stats, 1).

-spec inc_send_fail(stats()) -> stats().
inc_send_fail(Stats) ->
	inc_send_fail(Stats, 1).

-spec inc_send_succ(stats(), integer()) -> stats().
inc_send_succ(Stats, Inc) ->
	inc(Stats, #stats.send_succ, Inc).

-spec inc_send_fail(stats(), integer()) -> stats().
inc_send_fail(Stats, Inc) ->
	inc(Stats, #stats.send_fail, Inc).

-spec inc_dlr_succ(stats()) -> stats().
inc_dlr_succ(Stats) ->
	inc_dlr_succ(Stats, 1).

-spec inc_dlr_fail(stats()) -> stats().
inc_dlr_fail(Stats) ->
	inc_dlr_fail(Stats, 1).

-spec inc_dlr_succ(stats(), integer()) -> stats().
inc_dlr_succ(Stats, Inc) ->
	inc(Stats, #stats.dlr_succ, Inc).

-spec inc_dlr_fail(stats(), integer()) -> stats().
inc_dlr_fail(Stats, Inc) ->
	inc(Stats, #stats.dlr_fail, Inc).


-spec inc_errors(stats()) -> stats().
inc_errors(Stats) ->
	inc_errors(Stats, 1).

-spec inc_errors(stats(), integer()) -> stats().
inc_errors(Stats, Inc) ->
	inc(Stats, #stats.errors, Inc).

-spec inc_rps(stats(), integer()) -> stats().
inc_rps(Stats, Inc) ->
	inc(Stats, #stats.rps, Inc).

-spec send_succ(stats()) -> integer().
send_succ(Stats) ->
	Stats#stats.send_succ.

-spec send_fail(stats()) -> integer().
send_fail(Stats) ->
	Stats#stats.send_fail.

-spec dlr_succ(stats()) -> integer().
dlr_succ(Stats) ->
	Stats#stats.dlr_succ.

-spec dlr_fail(stats()) -> integer().
dlr_fail(Stats) ->
	Stats#stats.dlr_fail.

-spec errors(stats()) -> integer().
errors(Stats) ->
	Stats#stats.errors.

-spec rps(stats()) -> integer().
rps(Stats) ->
	Stats#stats.rps.

%% ===================================================================
%% Internal
%% ===================================================================

inc(Stats, Field, Inc) ->
	Val = element(Field, Stats) + Inc,
	setelement(Field, Stats, Val).

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

-spec test() -> ok | {error, term()}.

-spec empty_test() -> ok | {error, term()}.
empty_test() ->
	Empty = new(),
	?assertEqual(0, send_succ(Empty)),
	?assertEqual(0, send_fail(Empty)),
	?assertEqual(0, dlr_succ(Empty)),
	?assertEqual(0, dlr_fail(Empty)),
	?assertEqual(0, errors(Empty)),
	?assertEqual(0, rps(Empty)).

-spec inc1_test() -> ok | {error, term()}.
inc1_test() ->
	Stats0 = new(),
	Stats1 = inc_send_succ(Stats0),
	Stats2 = inc_send_fail(Stats1),
	Stats3 = inc_dlr_succ(Stats2),
	Stats4 = inc_dlr_fail(Stats3),
	Stats5 = inc_errors(Stats4),
	?assertEqual(1, send_succ(Stats5)),
	?assertEqual(1, send_fail(Stats5)),
	?assertEqual(1, dlr_succ(Stats5)),
	?assertEqual(1, dlr_fail(Stats5)),
	?assertEqual(1, errors(Stats5)).

-spec inc2_test() -> ok | {error, term()}.
inc2_test() ->
	Stats0 = new(),
	Stats1 = inc_send_succ(Stats0, 2),
	Stats2 = inc_send_fail(Stats1, 3),
	Stats3 = inc_dlr_succ(Stats2, 4),
	Stats4 = inc_dlr_fail(Stats3, 5),
	Stats5 = inc_errors(Stats4, 6),
	Stats6 = inc_rps(Stats5, 7),
	?assertEqual(2, send_succ(Stats6)),
	?assertEqual(3, send_fail(Stats6)),
	?assertEqual(4, dlr_succ(Stats6)),
	?assertEqual(5, dlr_fail(Stats6)),
	?assertEqual(6, errors(Stats6)),
	?assertEqual(7, rps(Stats6)).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
