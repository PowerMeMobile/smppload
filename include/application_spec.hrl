-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-type start_args() :: term().
-type state() :: term().
-type error_reason() :: term().

-spec start(start_type(), start_args()) -> {ok, pid()} | {ok, pid(), state()} | {error, error_reason()}.

-type start_phase() :: atom().
-type phase_args() :: term().

%-spec start_phase(start_phase(), start_type(),  phase_args()) -> ok | {error, error_reason()}.

%-spec prep_stop(state()) -> state().

-spec stop(state()) -> no_return().

-type param() :: atom().
-type value() :: term().
-type changed() :: [{param(), value()}].
-type new() :: [{param(), value()}].
-type removed() :: [param()].

%-spec config_change(changed(), new(), removed()) -> ok.
