-module(lazy_messages_random).

-behaviour(lazy_messages).

-export([
	init/1,
	deinit/1,
	get_next/1
]).

-include("lazy_messages.hrl").
-include("submit_message.hrl").

%-define(TEST, 1).
-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
	seed,
	source,
	destination,
	count,
	length,
	delivery
}).

-spec init(config()) -> state().
init(Config) ->
	Seed = proplists:get_value(seed, Config, now()),
	Source = proplists:get_value(source, Config),
	Destination = proplists:get_value(destination, Config),
	Count = proplists:get_value(count, Config),
	Length = proplists:get_value(length, Config),
	Delivery = proplists:get_value(delivery, Config),
	{ok, #state{
		seed = Seed,
		source = Source,
		destination = Destination,
		count = Count,
		length = Length,
		delivery = Delivery
	}}.

-spec deinit(state()) -> ok.
deinit(_State) ->
	ok.

-spec get_next(state()) -> {ok, #submit_message{}, state()} | {no_more, state()}.
get_next(State = #state{count = Count}) when Count =< 0 ->
	{no_more, State};
get_next(State = #state{
	seed = Seed0,
	source = Source,
	destination = Destination,
	count = Count,
	length = Length,
	delivery = Delivery
}) ->
	{Body, Seed1}  = build_random_body(Length, Seed0),
	Message = #submit_message{
		source = Source,
		destination = Destination,
		body = Body,
		delivery = Delivery
	},
	{ok, Message, State#state{seed = Seed1, count = Count - 1}}.

%% ===================================================================
%% Internal
%% ===================================================================

build_random_body(Length, Seed) ->
	build_random_body(Length, "", Seed).

build_random_body(Length, Body, Seed) when length(Body) =:= Length ->
	{Body, Seed};
build_random_body(Length, Body, Seed0) ->
	{R, Seed1} = random:uniform_s($z, Seed0),
	if
		(R >= $0 andalso R =< $9) orelse
		(R >= $A andalso R =< $Z) orelse
		(R >= $a andalso R =< $z) ->
			build_random_body(Length, [R | Body], Seed1);
		true ->
			build_random_body(Length, Body, Seed1)
	end.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

one_test() ->
	Config = [{seed, {1,1,1}}, {source, "s"}, {destination, "d"}, {delivery, true}, {count, 3}, {length, 5}],
	{ok, State0} = lazy_messages_random:init(Config),
	{ok, Msg, State1} = lazy_messages_random:get_next(State0),
	#submit_message{source = Source, destination = Destination, body = Body1, delivery = Delivery} = Msg,
	?assertEqual("s", Source),
	?assertEqual("d", Destination),
	?assert(Delivery),
	?assertEqual("Plesn", Body1),
	{ok, #submit_message{body = Body2}, State2} = lazy_messages_random:get_next(State1),
	?assertEqual("YPTRt", Body2),
	{ok, #submit_message{body = Body3}, State3} = lazy_messages_random:get_next(State2),
	?assertEqual("4rIYy", Body3),
	{no_more, State4} = lazy_messages_random:get_next(State3),
	ok = lazy_messages_user:deinit(State4).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
