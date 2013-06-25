-module(lazy_messages_file).

-behaviour(lazy_messages).

-export([
	init/1,
	deinit/1,
	get_next/1
]).

-include("pagload.hrl").
-include("lazy_messages.hrl").
-include("message.hrl").

-record(state, {
	fd
}).

%% ===================================================================
%% API
%% ===================================================================

-spec init(config()) -> {ok, state()}.
init(Config) ->
	case string:strip(proplists:get_value(file, Config), both) of
		"-" ->
			{ok, #state{fd = standard_io}};
		File ->
			case file:open(File, [read]) of
				{ok, Fd} ->
					{ok, #state{fd = Fd}};
				{error, Reason} ->
					?ABORT("Open file ~p failed with: ~p~n", [File, Reason])
			end
	end.

-spec deinit(state()) -> ok.
deinit(#state{fd = standard_io}) ->
	ok;
deinit(#state{fd = Fd}) ->
	file:close(Fd).

-spec get_next(state()) -> {ok, #message{}, state()} | {no_more, state()}.
get_next(State = #state{fd = Fd}) ->
	case file:read_line(Fd) of
		{ok, Line} ->
			case string:strip(string:strip(Line, right, $\n), both) of
				[] ->
					%% handle empty strings.
					get_next(State);
				[$# | _] ->
					%% handle comments.
					get_next(State);
				Stripped ->
					Message = parser:parse_message(Stripped),
					{ok, Message, State}
			end;
		eof ->
			{no_more, State}
	end.
