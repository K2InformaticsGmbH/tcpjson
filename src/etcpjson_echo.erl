-module(etcpjson_echo).
-behavior(etcpjson_srv).

% etcpjson_srv callbacks
-export([init/3, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {ip, port, srv}).

init(IP, Port, Srv) ->
    {ok, #state{ip = IP, port = Port, srv = Srv}}.

handle_info({rx, Json}, #state{srv = Srv} = State) ->
    Srv:send(#{echo => Json}),
    {noreply, State};
handle_info(Json, State) ->
    lager:info("RX: ~p", [Json]),
    erlang:send_after(2000, self(), {rx, Json}),
    {noreply, State}.

handle_cast(Request, State) -> {stop, {unsupported_cast, Request}, State}.

handle_call(Request, From, State) ->
    {stop, {unsupported_call, Request, From}, unsupported, State}.

terminate(Reason, #state{ip = IP, port = Port}) ->
    lager:info("terminate ~s:~p : ~p", [inet:ntoa(IP), Port, Reason]).

code_change(_OldVsn, State, _Extra) -> {ok, State}.
