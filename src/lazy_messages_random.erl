-module(lazy_messages_random).

-behaviour(lazy_messages).

-export([
	init/1,
	deinit/1,
	get_next/1
]).

-include("lazy_messages.hrl").
-include("message.hrl").
-include("smppload.hrl").
-include_lib("oserl/include/oserl.hrl").

-record(state, {
	seed,
	source,
	destination,
	count,
	length,
	delivery,
	%% for long messages
	parts = []
}).

%% ===================================================================
%% API
%% ===================================================================

-spec init(config()) -> {ok, state()}.
init(Config) ->
	Seed = ?gv(seed, Config, now()),
	Source =
		case ?gv(source, Config) of
			undefined ->
				undefined;
			Address ->
				parser:parse_address(Address)
		end,
	Destination = parser:parse_address(?gv(destination, Config)),
	Count = ?gv(count, Config),
	Length = ?gv(length, Config, ?SM_MAX_SIZE),
	Delivery = ?gv(delivery, Config),
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

-spec get_next(state()) -> {ok, #message{}, state()} | {no_more, state()}.
get_next(State = #state{
	count = Count,
	parts = Parts
}) when Count =< 0, length(Parts) =:= 0 ->
	{no_more, State};
get_next(State = #state{
	seed = Seed0,
	source = Source,
	destination = Destination,
	count = Count,
	length = Length,
	delivery = Delivery
}) when Length =< ?SM_MAX_SIZE ->
	{Body, Seed1}  = build_random_body(Length, Seed0),
	Message = #message{
		source = Source,
		destination = Destination,
		body = Body,
		delivery = Delivery
	},
	{ok, Message, State#state{
		seed = Seed1,
		count = Count - 1
	}};
get_next(State = #state{
	seed = Seed0,
	source = Source,
	destination = Destination,
	count = Count,
	length = Length,
	delivery = Delivery,
	parts = Parts0
}) ->
	case Parts0 of
		[Part | Parts1] ->
			Message = #message{
				source = Source,
				destination = Destination,
				body = ?gv(short_message, Part),
				esm_class = ?gv(esm_class, Part),
				delivery = Delivery
			},
			{ok, Message, State#state{parts = Parts1}};
		[] ->
			{Body, Seed1} = build_random_body(Length, Seed0),
			RefNum = smppload_ref_num:next(?MODULE),
		    [Part | Parts1] =
				smpp_sm:split([{short_message, Body}], RefNum, udh),
			Message = #message{
				source = Source,
				destination = Destination,
				body = ?gv(short_message, Part),
				esm_class = ?gv(esm_class, Part),
				delivery = Delivery
			},
			{ok, Message, State#state{
				seed = Seed1,
				count = Count - 1,
				parts = Parts1
			}}
	end.

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
