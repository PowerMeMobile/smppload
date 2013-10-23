-module(smppload_error).

-export([
	format/1
]).

-type error_status() :: pos_integer().
-type error_description() :: string().

%% ===================================================================
%% API
%% ===================================================================

-spec format(error_status()) -> error_description().
format(16#00000009) ->
	"Reserved.";
format(16#00000010) ->
	"Reserved.";
format(16#00000012) ->
	"Reserved.";
format(Status) when Status >= 16#00000016 andalso Status =< 16#00000032 ->
	"Reserved.";
format(Status) when Status >= 16#00000035 andalso Status =< 16#0000003F ->
	"Reserved.";
format(16#00000041) ->
	"Reserved.";
format(16#00000046) ->
	"Reserved.";
format(16#00000047) ->
	"Reserved.";
format(16#00000052) ->
	"Reserved.";
format(16#00000056) ->
	"Reserved.";
format(16#00000057) ->
	"Reserved.";
format(16#00000059) ->
	"Reserved.";
format(16#00000060) ->
	"Reserved.";
format(Status) when Status >= 16#00000068 andalso Status =< 16#000000BF ->
	"Reserved.";
format(Status) when Status >= 16#000000C5 andalso Status =< 16#000000FD ->
	"Reserved.";
format(Status) when Status >= 16#00000100 andalso Status =< 16#000003FF ->
	"Reserved for SMPP extension.";
format(Status) when Status >= 16#00000400 andalso Status =< 16#000004FF ->
	"Reserved for SMSC vendor specific errors.";
format(Status) when Status >= 16#00000500 andalso Status =< 16#FFFFFFFF ->
	"Reserved.";
format(Status) ->
	smpp_error:format(Status).
