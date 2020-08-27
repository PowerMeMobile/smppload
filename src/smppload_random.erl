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

-include_lib("alley_common/include/gen_server_spec.hrl").

-record(state, {
    rand_state :: rand:state()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link(os:timestamp()).

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
    RandState = rand:seed(exsplus, Seed),
    {ok, #state{rand_state = RandState}}.

handle_call({get_alnum_string, Length}, _From, State = #state{rand_state = RandState0}) ->
    {String, RandState1} = random(Length, fun is_alnum/1, RandState0),
    {reply, {ok, String}, State#state{rand_state = RandState1}};

handle_call({get_digit_string, Length}, _From, State = #state{rand_state = RandState0}) ->
    {String, RandState1} = random(Length, fun is_digit/1, RandState0),
    {reply, {ok, String}, State#state{rand_state = RandState1}};

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

random(Length, Pred, RandState) ->
    random(Length, Pred, "", RandState).

random(Length, _Pred, Body, RandState) when length(Body) =:= Length ->
    {Body, RandState};
random(Length, Pred, Body, RandState0) ->
    {R, RandState1} = rand:uniform_s($z, RandState0),
    case Pred(R) of
        true ->
            random(Length, Pred, [R | Body], RandState1);
        false ->
            random(Length, Pred, Body, RandState1)
    end.

is_alnum(C) ->
    (C >= $0 andalso C =< $9) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $a andalso C =< $z).

is_digit(C) ->
    (C >= $0 andalso C =< $9).
