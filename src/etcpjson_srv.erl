-module(etcpjson_srv).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-callback init(inet:ip_address(), inet:port_number(), tuple()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().

-optional_callbacks([format_status/2]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

%% ranch_protocol callbacks
-export([start_link/4]).

%% ranch_protocol and gen_server common callback
-export([init/1]).

%% behavior APIs
-export([send/2, close/0]).

-record(st, {sock, trnsprt, trnsprt_msgs, peer_ip, peer_port, buf = <<>>,
             mod, mod_state}).

start_link(Ref, Sock, Transport, [Mod]) ->
    {ok, proc_lib:spawn_opt(?MODULE, init, [{Ref, Sock, Transport, Mod}],
                            [link, {fullsweep_after, 0}])}.

init({Ref, Sock, Transport, Mod}) ->
    ok = ranch:accept_ack(Ref),
    {ok, {IP, Port}} = Transport:peername(Sock),
    ModState = case catch Mod:init(IP, Port, {?MODULE, Transport, Sock}) of
                   {ok, MS} -> MS;
                   {ok, MS, hibernate} -> MS; % TODO
                   {ok, MS, Timeout} when is_integer(Timeout) -> MS; % TODO
                   {stop, Reason} ->
                       lager:warn("~p:init stop : ~p", [Mod, Reason]),
                       exit({Mod, init_stop, Reason});
                   ignore ->
                       lager:warn("~p:init ignore", [Mod]),
                       exit({Mod, init_ignore});
                   {'EXIT', Error} ->
                       lager:error("~p:init error", [Error]),
                       error({Mod, Error})
               end,
    ok = Transport:setopts(Sock, [{active, once}]),
    gen_server:enter_loop(
      ?MODULE, [],
      #st{sock=Sock, trnsprt=Transport, peer_ip=IP, peer_port=Port,
             trnsprt_msgs=Transport:messages(), mod = Mod,
             mod_state = ModState}).

handle_info({?MODULE, '$close_i'}, #st{trnsprt = Transport} = S) ->
    catch Transport:close(),
    {stop, server_close, S};
handle_info({OK, Sock, Data},
            #st{trnsprt_msgs = {OK, _, _}, sock = Sock,
                trnsprt = Transport, buf = Buf} = S) ->
    ok = Transport:setopts(Sock, [{active, once}]),
    case parse_json_stream(<<Buf/binary, Data/binary>>) of
        {Json, Rest} ->
            handle_info_mod(Json, S#st{buf = Rest});
        Partial -> {noreply, S#st{buf = Partial}}
    end;
handle_info({Closed, Sock},
            #st{trnsprt_msgs = {_, Closed, _}, sock = Sock,
                trnsprt = Transport} = S) ->
    catch Transport:close(),
    {stop, {shutdown, peer_closed}, S};
handle_info({Error, Sock, Reason},
            #st{trnsprt_msgs = {_, _, Error}, sock = Sock,
                trnsprt = Transport} = S) ->
    lager:error("Error ~p", [Reason]),
    catch Transport:close(),
    {stop, Reason, S};
handle_info(Info, S) -> handle_info_mod(Info, S).

code_change(OldVsn, #st{mod = Mod, mod_state = ModS} = S, Extra) ->
    case catch Mod:code_change(OldVsn, ModS, Extra) of
        {ok, NewModS} -> {ok, S#st{mod_state = NewModS}};
        {error, Reason} -> {error, Reason};
        {'EXIT', Error} -> {error, {Mod, Error}}
    end.

handle_call(Request, From, #st{mod = Mod, mod_state = ModS} = S) ->
    case catch Mod:handle_call(Request, From, ModS) of
        {reply, Reply, NewModS} -> {reply, Reply, S#st{mod_state = NewModS}};
        {reply, Reply, NewModS, HOrT} ->
            {reply, Reply, S#st{mod_state = NewModS}, HOrT};
        {noreply, NewModS} -> {noreply, S#st{mod_state = NewModS}};
        {noreply, NewModS, HOrT} -> {noreply, S#st{mod_state = NewModS}, HOrT};
        {stop, Reason, Reply, NewModS} ->
            {stop, Reason, Reply, S#st{mod_state = NewModS}};
        {stop, Reason, NewModS} -> {stop, Reason, S#st{mod_state = NewModS}};
        {'EXIT', Error} -> {stop, {Mod, Error}, S}
    end.

handle_cast(Request, #st{mod = Mod, mod_state = ModS} = S) ->
    case catch Mod:handle_cast(Request, ModS) of
        {noreply, NewModS} -> {noreply, S#st{mod_state = NewModS}};
        {noreply, NewModS, HOrT} -> {noreply, S#st{mod_state = NewModS}, HOrT};
        {stop, Reason, NewModS} -> {stop, Reason, S#st{mod_state = NewModS}};
        {'EXIT', Error} -> {stop, {Mod, Error}, S}
    end.

terminate(Reason, #st{mod = Mod, mod_state = ModS}) ->
    case catch Mod:terminate(Reason, ModS) of
        {'EXIT', Error} -> lager:error("~p:terminate -> ~p", [Mod, Error]);
        Other -> Other
    end.

send(Json, {?MODULE, Transport, Sock}) ->
    JsonBin = jsx:encode(Json),
    JsonFrame = <<(byte_size(JsonBin)):4/little-unsigned-integer-unit:8,
                  JsonBin/binary>>,
    Transport:send(Sock, JsonFrame).

close() -> self() ! {?MODULE, '$close_i'}.

parse_json_stream(<<Len:4/little-unsigned-integer-unit:8, Rest/binary>>) ->
    case Rest of
        <<JsonBin:Len/binary, Partial/binary>> ->
            {jsx:decode(JsonBin, [return_maps]), Partial};
        Rest -> Rest
    end.

handle_info_mod(Info, #st{mod = Mod, mod_state = ModS} = S) ->
    case catch Mod:handle_info(Info, ModS) of
        {noreply, NewModS} -> {noreply, S#st{mod_state = NewModS}};
        {noreply, NewModS, HOrT} -> {noreply, S#st{mod_state = NewModS}, HOrT};
        {stop, Reason, NewModS} -> {stop, Reason, S#st{mod_state = NewModS}};
        {'EXIT', Error} -> {stop, {Mod, Error}, S}
    end.
