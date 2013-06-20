-ifndef(pagload_hrl).
-define(pagload_hrl, 1).

-define(DEBUG(Str, Args), pagload_log:log(debug, Str, Args)).
-define(INFO(Str, Args), pagload_log:log(info, Str, Args)).
-define(WARN(Str, Args), pagload_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), pagload_log:log(error, Str, Args)).

-define(ABORT(Str, Args), pagload_log:log(error, Str, Args), halt(1)).

-define(gv(Key, Params), proplists:get_value(Key, Params)).
-define(gv(Key, Params, Default), proplists:get_value(Key, Params, Default)).

-endif.
