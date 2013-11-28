-module(smppload_lazy_messages).

-export([
	init/2,
	deinit/1,
	get_next/1
]).

-include("smppload_lazy_messages.hrl").
-include("message.hrl").

-callback init(config()) -> {ok, state()} | {error, reason()}.
-callback deinit(state()) -> ok.
-callback get_next(state()) -> {ok, #message{}, state()} | {no_more, state()}.

-record(state, {
	mod,
	mod_state
}).

-spec init(module(), config()) -> {ok, state()} | {error, reason()}.
init(Module, Config) ->
	case Module:init(Config) of
		{ok, ModState} ->
			{ok, #state{mod = Module, mod_state = ModState}};
		Error ->
			Error
	end.

-spec deinit(state()) -> ok.
deinit(State) ->
	Module = State#state.mod,
	ModState = State#state.mod_state,
	Module:deinit(ModState).

-spec get_next(state()) -> {ok, #message{}, state()} | {no_more, state()}.
get_next(State) ->
	Module = State#state.mod,
	ModState0 = State#state.mod_state,
	case Module:get_next(ModState0) of
		{ok, Message, ModState1} ->
			{ok, Message, State#state{mod_state = ModState1}};
		{no_more, ModState1} ->
			{no_more, State#state{mod_state = ModState1}}
	end.
