-module(lazy_messages_file).

-behaviour(lazy_messages).

-export([
	init/1,
	deinit/1,
	get_next/1
]).

-include("smppload.hrl").
-include("lazy_messages.hrl").
-include("message.hrl").
-include_lib("oserl/include/oserl.hrl").

-record(state, {
	fd,
	%% for long messages
	parts = [],
	source,
	destination,
	delivery
}).

%% ===================================================================
%% API
%% ===================================================================

-spec init(config()) -> {ok, state()}.
init(Config) ->
	case string:strip(?gv(file, Config), both) of
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
get_next(State = #state{fd = Fd, parts = []}) ->
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
					Message0 = parser:parse_message(Stripped),

					Source = Message0#message.source,
					Destination = Message0#message.destination,
					Body = Message0#message.body,
					Delivery = Message0#message.delivery,

					case length(Body) =< ?MAX_MSG_LEN of
						true ->
							{ok, Message0, State};
						false ->
							RefNum = smppload_ref_num:next(?MODULE),
						    [Part | Parts] =
								smpp_sm:split([{short_message, Body}], RefNum, udh, ?MAX_SEG_LEN),
							Message1 = #message{
								source = Source,
								destination = Destination,
								body = ?gv(short_message, Part),
								esm_class = ?gv(esm_class, Part),
								delivery = Delivery
							},
							{ok, Message1, State#state{
								parts = Parts,
								source = Source,
								destination = Destination,
								delivery = Delivery
							}}
					end
			end;
		eof ->
			{no_more, State}
	end;
get_next(State = #state{
	parts = [Part],
	source = Source,
	destination = Destination,
	delivery = Delivery
}) ->
	Message = #message{
		source = Source,
		destination = Destination,
		body = ?gv(short_message, Part),
		esm_class = ?gv(esm_class, Part),
		delivery = Delivery
	},
	{ok, Message, State#state{
		parts = [],
		source = undefined,
		destination = undefined,
		delivery = undefined
	}};
get_next(State = #state{
	parts = [Part | Parts],
	source = Source,
	destination = Destination,
	delivery = Delivery
}) ->
	Message = #message{
		source = Source,
		destination = Destination,
		body = ?gv(short_message, Part),
		esm_class = ?gv(esm_class, Part),
		delivery = Delivery
	},
	{ok, Message, State#state{
		parts = Parts
	}}.
