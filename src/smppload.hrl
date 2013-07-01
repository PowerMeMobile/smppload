-ifndef(smppload_hrl).
-define(smppload_hrl, 1).

-define(DEBUG(Str, Args), smppload_log:log(debug, Str, Args)).
-define(INFO(Str, Args), smppload_log:log(info, Str, Args)).
-define(ERROR(Str, Args), smppload_log:log(error, Str, Args)).

-define(ABORT(Str, Args), smppload_log:log(error, Str, Args), halt(1)).

-define(gv(Key, Params), proplists:get_value(Key, Params)).
-define(gv(Key, Params, Default), proplists:get_value(Key, Params, Default)).

-endif.
