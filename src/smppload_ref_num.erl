-module(smppload_ref_num).

-export([
    next/1
]).

%% ===================================================================
%% API
%% ===================================================================

%%
%% The only purpose for this module/function is to make tests work.
%% For some reason under test smpp_ref_num:next/1 fails due to
%% strange ets:new/2 failure. This is a quick workaround.
%%
-spec next(atom()) -> pos_integer().
-ifndef(TEST).
next(Key) ->
    smpp_ref_num:next(Key).
-else.
next(_Key) ->
    1.
-endif.
