-module(smppload).

-export([
    main/0,
    main/1
]).

-include("message.hrl").
-include("smppload.hrl").
-include_lib("oserl/include/smpp_globals.hrl").

-define(TX_DELIVERY_TIMEOUT, 80000).
-define(RX_DELIVERY_TIMEOUT, infinity).

%% ===================================================================
%% API
%% ===================================================================

-spec main() -> no_return().
main() ->
    PlainArgs = init:get_plain_arguments(),
    case PlainArgs of
        ["--", Args] ->
            main(Args);
        _ ->
            main([])
    end.

-spec main([string()]) -> no_return().
main([]) ->
    AppName = app_name(),
    OptSpecs = opt_specs(),
    print_usage(AppName, OptSpecs);
main("--") ->
    main([]);
main(Args) ->
    AppName = app_name(),
    OptSpecs = opt_specs(),

    case getopt:parse(OptSpecs, Args) of
        {ok, {Opts, _NonOptArgs}} ->
            process_opts(AppName, Opts, OptSpecs);
        {error, {Reason, Data}} ->
            ?ABORT("Parse data ~p failed with: ~s~n", [Data, Reason])
    end.

%% ===================================================================
%% Internal
%% ===================================================================

opt_specs() ->
    DC = ?ENCODING_SCHEME_LATIN_1,
    {MaxMsgLen, _} = smppload_utils:max_msg_seg(DC),
    [
        %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
        {help, $h, "help", undefined, "Show this message"},
        {host, $H, "host", {string, "127.0.0.1"}, "SMSC server host name or IP address"},
        {port, $P, "port", {integer, 2775}, "SMSC server port"},
        {bind_type, $B, "bind_type", {string, "TRX"}, "SMSC bind type: TX | TRX | RX"},
        {system_id, $i, "system_id", {string, "user"}, "SMSC system_id"},
        {password, $p, "password", {string, "password"}, "SMSC password"},
        {system_type, $t, "system_type", {string, ""}, "SMSC service_type"},
        {rps, $r, "rps", {integer, 1000}, "Number of requests per second. Ignored for RX"},
        {source, $s, "source", {string, ""}, "SMS source address Addr[:Len=0][,Ton=1,Npi=1]. Ignored for RX"},
        {destination , $d, "destination", string,
            "SMS destination address Addr[:Len=0][,Ton=1,Npi=1]. Ignored for RX"},
        {body, $b, "body", string, "SMS body, randomly generated if not set. Ignored for RX"},
        {length, $l, "length", {integer, MaxMsgLen}, "Randomly generated body length. Ignored for RX"},
        {count, $c, "count", {integer, 1}, "Count of SMS to send. Ignored for RX"},
        {delivery, $D, "delivery", {integer, 0}, "Delivery receipt. Ignored for RX"},
        {data_coding, $C, "data_coding", {integer, DC}, "Data coding. Ignored for RX"},
        {esm_class, $E, "esm_class", {integer, 0}, "ESM class. Ignored for RX"},
        {file, $f, "file", string, "Send messages from file. Ignored for RX"},
        {verbosity, $v, "verbosity", {integer, 1}, "Verbosity level"},
        {thread_count, $T, "thread_count", {integer, 10}, "Thread/process count. Ignored for RX"},
        {bind_timeout, undefined, "bind_timeout", {integer, 10000}, "Bind timeout, ms"},
        {unbind_timeout, undefined, "unbind_timeout", {integer, 5000}, "Unbind timeout, ms"},
        {submit_timeout, undefined, "submit_timeout", {integer, 20000}, "Submit timeout, ms. Ignored for RX"},
        {delivery_timeout, undefined, "delivery_timeout", integer,
            io_lib:format("Delivery timeout, ms [default: TX=~p, RX=~p]",
                [?TX_DELIVERY_TIMEOUT, ?RX_DELIVERY_TIMEOUT])},
        {ssl, undefined, "ssl", undefined, "Use ssl/tls connection"}
    ].

process_opts(AppName, Opts, OptSpecs) ->
    case ?gv(help, Opts, false) of
        true ->
            print_usage(AppName, OptSpecs);
        false ->
            %% initialize the logger.
            smppload_log:init(?gv(verbosity, Opts)),
            ?DEBUG("Options: ~p~n", [Opts]),

            %% check params.
            check_destination(Opts),

            %% start needed applications.
            error_logger:tty(false),
            {ok, _} = application:ensure_all_started(smppload),

            %% initialize stats.
            ok = smppload_stats:init(),

            Host = ?gv(host, Opts),
            Port = ?gv(port, Opts),
            UseSSL = lists:member(ssl, Opts),
            Peer = format_peer(Host, Port, UseSSL),

            {ok, _} = smppload_esme:start(),
            case smppload_esme:connect(Host, Port, UseSSL) of
                ok ->
                    ?INFO("Connected to ~s~n", [Peer]);
                {error, Reason1} ->
                    ?ABORT("Connect to ~s failed with: ~s~n", [Peer, Reason1])
            end,

            BindTypeFun = get_bind_type_fun(Opts),
            ?DEBUG("BindTypeFun: ~p~n", [BindTypeFun]),

            SystemType = ?gv(system_type, Opts),
            SystemId = ?gv(system_id, Opts),
            Password = ?gv(password, Opts),
            BindTimeout = ?gv(bind_timeout, Opts),
            BindParams = [
                {system_type, SystemType},
                {system_id, SystemId},
                {password, Password},
                {bind_timeout, BindTimeout}
            ],
            case apply(smppload_esme, BindTypeFun, [BindParams]) of
                {ok, RemoteSystemId} ->
                    ?INFO("Bound to ~s~n", [RemoteSystemId]);
                {error, Reason2} ->
                    ?ABORT("Bind failed with: ~p~n", [Reason2])
            end,

            Rps = ?gv(rps, Opts),
            ok = smppload_esme:set_max_rps(Rps),

            case BindTypeFun of
                bind_receiver ->
                    Opts2 = add_default_opts(
                        [{delivery_timeout, ?RX_DELIVERY_TIMEOUT}], Opts),
                    timer:sleep(?gv(delivery_timeout, Opts2));
                _ ->
                    Opts2 = add_default_opts(
                        [{destination, ""},
                         {delivery_timeout, ?TX_DELIVERY_TIMEOUT}], Opts),
                    MessagesModule = get_lazy_messages_module(Opts2),
                    ?DEBUG("MessagesModule: ~p~n", [MessagesModule]),
                    send_messages(MessagesModule, Opts2)
            end,

            ?INFO("Stats:~n", []),
            ?INFO("   Send success:     ~p~n", [smppload_stats:send_succ()]),
            ?INFO("   Send fail:        ~p~n", [smppload_stats:send_fail()]),
            ?INFO("   Delivery success: ~p~n", [smppload_stats:dlr_succ()]),
            ?INFO("   Delivery fail:    ~p~n", [smppload_stats:dlr_fail()]),
            ?INFO("   Incomings:        ~p~n", [smppload_stats:incomings()]),
            ?INFO("   Errors:           ~p~n", [smppload_stats:errors()]),

            {ok, AvgRps} = smppload_esme:get_avg_rps(),
            ?INFO("   Avg Rps:          ~p mps~n", [AvgRps]),

            UnbindTimeout = ?gv(unbind_timeout, Opts),
            UnbindParams = [
                {unbind_timeout, UnbindTimeout}
            ],
            smppload_esme:unbind(UnbindParams),
            ?INFO("Unbound~n", []),

            %% stop applications.
            error_logger:tty(false),
            application:stop(smppload),
            application:stop(common_lib)
    end.

add_default_opts([], Opts) ->
    Opts;
add_default_opts([{Name, Value} | Defaults], Opts) ->
    case ?gv(Name, Opts) of
        undefined ->
            add_default_opts(Defaults, [{Name, Value} | Opts]);
        _ ->
            add_default_opts(Defaults, Opts)
    end.

format_peer(Host, Port, UseSSL) when is_boolean(UseSSL) ->
  ConnectionType =
  case UseSSL of
    true -> ssl;
    false -> tcp
  end,
  format_peer(Host, Port, ConnectionType);
format_peer({A, B, C, D}, Port, ConnectionType) ->
    io_lib:format("~p.~p.~p.~p:~p (~p)", [A, B, C, D, Port, ConnectionType]);
format_peer(Host, Port, ConnectionType) when is_list(Host) ->
    io_lib:format("~s:~p (~p)", [Host, Port, ConnectionType]).

get_bind_type_fun(Opts) ->
    BindType = ?gv(bind_type, Opts),
    case string:to_lower(BindType) of
        "tx" ->
            bind_transmitter;
        "trx" ->
            bind_transceiver;
        "rx" ->
            bind_receiver;
        _ ->
            ?ABORT("Unknown bind type: ~p~n", [BindType])
    end.

get_lazy_messages_module(Opts) ->
    case ?gv(file, Opts) of
        undefined ->
            case ?gv(body, Opts) of
                undefined ->
                    smppload_lazy_messages_random;
                _ ->
                    smppload_lazy_messages_body
            end;
        _ ->
            smppload_lazy_messages_file
    end.

check_destination(Opts) ->
    case ?gv(destination, Opts) of
        undefined ->
            case ?gv(count, Opts) of
                0 ->
                    ok;
                _ ->
                    case string:to_lower(?gv(bind_type, Opts)) of
                        "rx" ->
                            ok;
                        _ ->
                            case ?gv(file, Opts) of
                                undefined ->
                                    ?ABORT("Destination address is not provided~n", []);
                                _ ->
                                    ok
                            end
                    end
            end;
        _ ->
            ok
    end.

send_messages(Module, Opts) ->
    {ok, State0} = smppload_lazy_messages:init(Module, Opts),
    {ok, State1} = send_parallel_messages(State0, Opts),
    ok = smppload_lazy_messages:deinit(State1).

send_parallel_messages(State0, Opts) ->
    process_flag(trap_exit, true),
    ReplyTo = self(),
    ReplyRef = make_ref(),
    ThreadCount = ?gv(thread_count, Opts),
    {ok, MsgSent, State1} = send_parallel_init_messages(
        ReplyTo, ReplyRef, Opts, ThreadCount, 0, State0
    ),
    send_parallel_messages_and_collect_replies(
        ReplyTo, ReplyRef, Opts,
        MsgSent, State1
    ).

%% start phase
send_parallel_init_messages(_ReplyTo, _ReplyRef, _Opts, MaxMsgCnt, MaxMsgCnt, State0) ->
    {ok, MaxMsgCnt, State0};
send_parallel_init_messages(ReplyTo, ReplyRef, Opts, MaxMsgCnt, MsgCnt, State0) ->
    case smppload_lazy_messages:get_next(State0) of
        {ok, Submit, State1} ->
            spawn_link(
                fun() ->
                    SubmitTimeout = ?gv(submit_timeout, Opts),
                    DeliveryTimeout = ?gv(delivery_timeout, Opts),
                    send_message_and_reply(ReplyTo, ReplyRef, Submit, SubmitTimeout, DeliveryTimeout)
                end
            ),
            send_parallel_init_messages(
                ReplyTo, ReplyRef, Opts, MaxMsgCnt, MsgCnt + 1, State1
            );
        {no_more, State1} ->
            {ok, MsgCnt, State1}
    end.

%% collect and send new messages phase.
send_parallel_messages_and_collect_replies(
    _ReplyTo, _ReplyRef, _Opts, 0, State0
) ->
    {ok, State0};
send_parallel_messages_and_collect_replies(
    ReplyTo, ReplyRef, Opts, MsgSent, State0
) ->
    SubmitTimeout = ?gv(submit_timeout, Opts),
    DeliveryTimeout = ?gv(delivery_timeout, Opts),
    Timeout =
        case ?gv(delivery, Opts, 0) of
            0 ->
                SubmitTimeout;
            _ ->
                SubmitTimeout + DeliveryTimeout
        end,
    receive
        ReplyRef ->
            send_parallel_messages_and_collect_replies(
                ReplyTo, ReplyRef, Opts,
                MsgSent - 1 + 1, State0
            );

        {'EXIT', _Pid, Reason} ->
            case Reason of
                normal ->
                    nop;
                _Other ->
                    ?ERROR("Submit failed with: ~p~n", [Reason]),
                    smppload_stats:incr_errors(1)
            end,
            case smppload_lazy_messages:get_next(State0) of
                {ok, Submit, State1} ->
                    spawn_link(
                        fun() ->
                            send_message_and_reply(ReplyTo, ReplyRef, Submit,
                                SubmitTimeout, DeliveryTimeout)
                        end
                    ),
                    send_parallel_messages_and_collect_replies(
                        ReplyTo, ReplyRef, Opts,
                        MsgSent - 1 + 1, State1
                    );
                {no_more, State1} ->
                    send_parallel_messages_and_collect_replies(
                        ReplyTo, ReplyRef, Opts,
                        MsgSent - 1, State1
                    )
            end
    after
        Timeout ->
            ?ERROR("Timeout~n", []),
            smppload_stats:incr_send_fail(MsgSent),
            smppload_stats:incr_errors(MsgSent),
            {ok, State0}
    end.

send_message_and_reply(ReplyTo, ReplyRef, Submit, SubmitTimeout, DeliveryTimeout) ->
    send_message(Submit, SubmitTimeout, DeliveryTimeout),
    ReplyTo ! ReplyRef.

send_message(Msg, SubmitTimeout, DeliveryTimeout) ->
    SourceAddr =
        case Msg#message.source of
            [] ->
                [];
            _ ->
                [
                    {source_addr_ton , Msg#message.source#address.ton},
                    {source_addr_npi , Msg#message.source#address.npi},
                    {source_addr     , Msg#message.source#address.addr}
                ]
        end,
    RegDlr =
        case Msg#message.delivery of
            true  ->
                1;
            false ->
                0;
            Int when is_integer(Int), Int > 0 ->
                1;
            _Other ->
                0
        end,
    Params = SourceAddr ++ [
        {dest_addr_ton      , Msg#message.destination#address.ton},
        {dest_addr_npi      , Msg#message.destination#address.npi},
        {destination_addr   , Msg#message.destination#address.addr},
        {short_message      , Msg#message.body},
        {data_coding        , Msg#message.data_coding},
        {esm_class          , Msg#message.esm_class},
        {registered_delivery, RegDlr},
        {submit_timeout     , SubmitTimeout},
        {delivery_timeout   , DeliveryTimeout}
    ],

    case smppload_esme:submit_sm(Params) of
        {ok, _OutMsgId, no_delivery} ->
            smppload_stats:incr_send_succ(1);
        {ok, _OutMsgId, delivery_timeout} ->
            smppload_stats:incr_send_succ(1),
            smppload_stats:incr_dlr_fail(1);
        {ok, _OutMsgId, _DlrRes} ->
            smppload_stats:incr_send_succ(1),
            smppload_stats:incr_dlr_succ(1);
        {error, _Reason} ->
            smppload_stats:incr_send_fail(1)
    end.

print_usage(AppName, OptSpecs) ->
    print_description_vsn(AppName),
    getopt:usage(OptSpecs, AppName).

print_description_vsn(AppName) ->
    case description_vsn(AppName) of
        {Description, Vsn} ->
            io:format("~s (~s)~n", [Description, Vsn]);
        _ ->
            ok
    end.

description_vsn(AppName) ->
    case app_options(AppName) of
        undefined ->
            undefined;
        Options ->
            Description = ?gv(description, Options),
            Vsn = ?gv(vsn, Options),
            {Description, Vsn}
    end.

is_escript() ->
    case init:get_argument(mode) of
        {ok, [["embedded"]]} ->
            false;
        _ ->
            true
    end.

app_name() ->
    case is_escript() of
        true ->
            escript:script_name();
        false ->
            {ok, [[AppName]]} = init:get_argument(progname),
            AppName
    end.

app_options(AppName) ->
    case is_escript() of
        true ->
            escript_options(AppName);
        false ->
            application_options(AppName)
    end.

escript_options(ScriptName) ->
    {ok, Sections} = escript:extract(ScriptName, []),
    Zip = ?gv(archive, Sections),
    AppName = lists:flatten(io_lib:format("~p.app", [?MODULE])),
    case zip:extract(Zip, [{file_list, [AppName]}, memory]) of
        {ok, [{AppName, Binary}]} ->
            {ok, Tokens, _} = erl_scan:string(binary_to_list(Binary)),
            {ok, {application, ?MODULE, Options}} = erl_parse:parse_term(Tokens),
            Options;
        _ ->
            undefined
    end.

application_options(_AppName) ->
    case application:get_all_key(?MODULE) of
        undefined ->
            undefined;
        {ok, Options} ->
            Options
    end.
