-module(etcpjson_srv).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

%% ranch_protocol callbacks
-export([start_link/4]).

%% ranch_protocol and gen_server common callback
-export([init/1]).

-record(state, {sock, trnsprt, trnsprt_msgs, peer_ip, peer_port}).

start_link(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

init({Ref, Socket, Transport, _Opts = []}) ->
    %% Perform any required state initialization here.
    ok = ranch:accept_ack(Ref),
    {ok, {IP, Port}} = Transport:peername(Socket),
    lager:info("peer connected ~s:~p", [inet:ntoa(IP), Port]),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(
      ?MODULE, [],
      #state{sock=Socket, trnsprt=Transport, peer_ip=IP, peer_port=Port,
             trnsprt_msgs=Transport:messages()}).

handle_info({OK, Sock, Data},
            #state{trnsprt_msgs = {OK, _, _}, sock = Sock,
                   trnsprt = Transport} = State) ->
    ok = Transport:setopts(Sock, [{active, once}]),
    lager:info("RX ~p", [Data]),
    {noreply, State};
handle_info({Closed, Sock},
            #state{trnsprt_msgs = {_, Closed, _}, sock = Sock,
                   trnsprt = Transport} = State) ->
    lager:info("peer closed ~s:~p",
               [inet:ntoa(State#state.peer_ip), State#state.peer_port]),
    catch Transport:close(),
    {stop, normal, State};
handle_info({Error, Sock, Reason},
            #state{trnsprt_msgs = {_, _, Error}, sock = Sock,
                   trnsprt = Transport} = State) ->
    lager:error("Error ~p", [Reason]),
    catch Transport:close(),
    {stop, Reason, State};
handle_info(Info, State) -> {stop, {unsupported_info, Info}, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(Request, _From, State) -> {stop, {unsupported_call, Request}, State}.
handle_cast(Request, State) -> {stop, {unsupported_cast, Request}, State}.

terminate(normal, _State) -> ok;
terminate(Reason, #state{peer_ip = IP, peer_port = Port}) ->
    lager:info("terminate ~s:~p : ~p", [inet:ntoa(IP), Port, Reason]).
