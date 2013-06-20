-module(pagload_develop).

-export([init/0]).

-spec init() -> void.
init() ->
	pagload_log:init(3).
