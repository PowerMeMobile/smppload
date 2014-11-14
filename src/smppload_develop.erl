-module(smppload_develop).

-export([init/0]).

-spec init() -> void.
init() ->
    application:ensure_started(sync),
    smppload_log:init(3).
