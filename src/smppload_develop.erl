-module(smppload_develop).

-export([init/0]).

-spec init() -> void.
init() ->
	smppload_log:init(3).
