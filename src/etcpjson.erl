-module(etcpjson).

-behaviour(application).

% CLI
-export([start/0, stop/0]).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> application:ensure_all_started(?MODULE).

stop() -> application:stop(?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, ListenPort} = application:get_env(port),
    {ok, Interface} = application:get_env(interface),
    {ok, ListenIf} = inet:getaddr(Interface, inet),
    TransOpts0 = [{ip, ListenIf}, {port, ListenPort}],
    {Transport, TransOpts} =
    case application:get_env(ssl) of
        {ok, true} ->
            {ok, SSLOpts} = application:get_env(ssl_opts),
            TransOpts1 = TransOpts0 ++ SSLOpts,
            lager:info("ssl server listening ~s:~p ~p",
                       [inet:ntoa(ListenIf), ListenPort, TransOpts1]),
            {ranch_ssl, TransOpts1};
        {ok, false} ->
            lager:info("tcp server listening ~s:~p",
                       [inet:ntoa(ListenIf), ListenPort]),
            {ranch_tcp, TransOpts0}
    end,
    ranch:start_listener(?MODULE, 100, Transport, TransOpts, etcpjson_srv,
                         [etcpjson_echo]),
    supervisor:start_link(?MODULE, []).

stop(_State) ->
    lager:info("stopping tcl/ssl listener"),
    ranch:stop_listener(?MODULE).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================
init(_Args) -> {ok, {#{strategy => one_for_one, intensity => 5, period => 10},[]}}.
