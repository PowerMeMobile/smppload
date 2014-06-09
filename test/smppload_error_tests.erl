-module(smppload_error_tests).

-include_lib("eunit/include/eunit.hrl").
-spec test() -> ok | {error, term()}.

%% ===================================================================
%% Tests begin
%% ===================================================================

-spec format_test() -> ok | no_return().
format_test() ->
    ?assertEqual("No Error.", smppload_error:format(16#00000000)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000009)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000010)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000012)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000016)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000032)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000035)),
    ?assertEqual("Reserved.", smppload_error:format(16#0000003F)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000041)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000046)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000047)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000052)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000056)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000057)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000059)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000060)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000068)),
    ?assertEqual("Reserved.", smppload_error:format(16#000000BF)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000032)),
    ?assertEqual("Reserved.", smppload_error:format(16#000000C5)),
    ?assertEqual("Reserved.", smppload_error:format(16#000000FD)),
    ?assertEqual("Reserved for SMPP extension.", smppload_error:format(16#00000100)),
    ?assertEqual("Reserved for SMPP extension.", smppload_error:format(16#000003FF)),
    ?assertEqual("Reserved for SMSC vendor specific errors.", smppload_error:format(16#00000400)),
    ?assertEqual("Reserved for SMSC vendor specific errors.", smppload_error:format(16#000004FF)),
    ?assertEqual("Reserved.", smppload_error:format(16#00000500)),
    ?assertEqual("Reserved.", smppload_error:format(16#FFFFFFFF)).

%% ===================================================================
%% Tests end
%% ===================================================================
