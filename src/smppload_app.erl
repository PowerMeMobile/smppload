-module(smppload_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include_lib("alley_common/include/application_spec.hrl").

%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    smppload_sup:start_link().

stop(_State) ->
    ok.
