-module(smppload_utils).

-export([
    process_address/1
]).

-include("message.hrl").

-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec process_address(#address{}) -> #address{}.
process_address(Address = #address{
    addr = #rand_addr{prefix = Prefix, rand_len = RandLen}
}) ->
    Addr = Prefix ++ smppload_random:get_digit_string(RandLen),
    Address#address{addr = Addr};
process_address(Address) ->
    Address.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

-spec test() -> ok | {error, term()}.

-spec process_address_undefined_addr_test() -> ok | {error, term()}.
process_address_undefined_addr_test() ->
    Address = #address{
        addr = undefined,
        ton = 1,
        npi = 2
    },
    Expected = #address{
        addr = undefined,
        ton = 1,
        npi = 2
    },
    ?assertEqual(Expected, process_address(Address)).

-spec process_address_normal_addr_test() -> ok | {error, term()}.
process_address_normal_addr_test() ->
    Address = #address{
        addr = "1234567890",
        ton = 1,
        npi = 2
    },
    Expected = #address{
        addr = "1234567890",
        ton = 1,
        npi = 2
    },
    ?assertEqual(Expected, process_address(Address)).

-spec process_address_rand_addr_test() -> ok | {error, term()}.
process_address_rand_addr_test() ->
    {ok, _} = smppload_random:start_link({1, 1, 1}),
    Address = #address{
        addr = #rand_addr{prefix = "12345", rand_len = 5},
        ton = 1,
        npi = 2
    },
    Expected = #address{
        addr = "1234529204",
        ton = 1,
        npi = 2
    },
    ?assertEqual(Expected, process_address(Address)),
    ok = smppload_random:stop().

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
