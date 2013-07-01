%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------

-module(smppload_log).

-export([
    init/1,
    set_level/1, default_level/0,
    log/3
]).

%% ===================================================================
%% API
%% ===================================================================

init(Verbosity) ->
    case valid_level(Verbosity) of
        0 -> set_level(error);
        1 -> set_level(info);
        2 -> set_level(debug);
		_ -> set_level(debug)
    end.

set_level(Level) ->
    ok = application:set_env(smppload, log_level, Level).

log(error, Str, Args) ->
	io:format(log_prefix(error) ++ Str, Args);
log(Level, Str, Args) ->
	LogLevel = proplists:get_value(
		log_level, application:get_all_env(smppload), default_level()),
    case should_log(LogLevel, Level) of
        true ->
            io:format(log_prefix(Level) ++ Str, Args);
        false ->
            ok
    end.

default_level() -> error_level().

%% ===================================================================
%% Internal
%% ===================================================================

valid_level(Level) ->
    erlang:max(error_level(), erlang:min(Level, debug_level())).

error_level() -> 0.
debug_level() -> 3.

should_log(debug, _)     -> true;
should_log(info, debug)  -> false;
should_log(info, _)      -> true;
should_log(error, error) -> true;
should_log(error, _)     -> false;
should_log(_, _)         -> false.

log_prefix(debug) -> "DEBUG: ";
log_prefix(info)  -> "INFO:  ";
log_prefix(error) -> "ERROR: ".
