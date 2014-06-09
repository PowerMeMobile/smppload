-module(smppload_random).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/0,

    get_alnum_string/1,
    get_digit_string/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("gen_server_spec.hrl").

-record(state, {
    seed :: random:seed()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link(now()).

-spec start_link(random:seed()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Seed) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Seed], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec get_alnum_string(pos_integer()) -> string().
get_alnum_string(Length) ->
    {ok, String} = gen_server:call(?MODULE, {get_alnum_string, Length}),
    String.

-spec get_digit_string(pos_integer()) -> string().
get_digit_string(Length) ->
    {ok, String} = gen_server:call(?MODULE, {get_digit_string, Length}),
    String.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Seed]) ->
    {ok, #state{seed = Seed}}.

handle_call({get_alnum_string, Length}, _From, State = #state{seed = Seed0}) ->
    {String, Seed1} = random(Length, fun is_alnum/1, Seed0),
    {reply, {ok, String}, State#state{seed = Seed1}};

handle_call({get_digit_string, Length}, _From, State = #state{seed = Seed0}) ->
    {String, Seed1} = random(Length, fun is_digit/1, Seed0),
    {reply, {ok, String}, State#state{seed = Seed1}};

handle_call(Request, _From, State = #state{}) ->
    {stop, {bad_arg, Request}, State}.

handle_cast(stop, State = #state{}) ->
    {stop, normal, State};

handle_cast(Request, State = #state{}) ->
    {stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
    {stop, {bad_arg, Message}, State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

random(Length, Pred, Seed) ->
    random(Length, Pred, "", Seed).

random(Length, _Pred, Body, Seed) when length(Body) =:= Length ->
    {Body, Seed};
random(Length, Pred, Body, Seed0) ->
    {R, Seed1} = random:uniform_s($z, Seed0),
    case Pred(R) of
        true ->
            random(Length, Pred, [R | Body], Seed1);
        false ->
            random(Length, Pred, Body, Seed1)
    end.

is_alnum(C) ->
    (C >= $0 andalso C =< $9) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $a andalso C =< $z).

is_digit(C) ->
    (C >= $0 andalso C =< $9).
