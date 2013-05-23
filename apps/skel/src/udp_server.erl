-module(udp_server).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Args) ->
  Serv = spawn(fun() -> server(9595) end),
  link(Serv),
  {ok, Args}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(stats, State) ->
  io:format("got stats message~n"),
  ets:foldl(
    fun({Key, ConnState}, _) ->
        {Host, Port} = Key,
        io:format("~p: ~p~n", [{Host, Port}, ConnState])
    end, infonotused, udp_clients),
  {noreply, State};
handle_info({makespam, Ip}, State) ->
  spam(10000, Ip, 9595, 1),
    {noreply, State};
handle_info(makespam, State) ->
  io:format("spamming~n"),
  spam(10000, {166,78,12,187}, 9595, 1),
  {noreply, State};
handle_info(Info, State) ->
    io:format("got message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

server(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, false}, {recbuf, 65536}, {sndbuf, 65536}, {buffer, 65536}, {read_packets, 16000}]),
  io:format("server opened socket:~p~n",[Socket]),
  ets:new(udp_clients, [set, named_table]),
  ets:new(pregistry, [set, named_table, public]),
  %Heartbeat = spawn(fun() -> heartbeat(Socket) end),
  %link(Heartbeat),
  loop(Socket).

loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, Socket, Host, Port, Bin} ->
      case ets:lookup(udp_clients, { Host, Port }) of
        [] -> 
          % new client
          ets:insert(udp_clients, { { Host, Port }, 0 }),
          handle(Socket, Host, Port, {1}, Bin);
        [{{Host, Port}, ConnState}] ->
          handle(Socket, Host, Port, ConnState, Bin);
        [_Unknown] -> 
          io:format("bad news!~n")

          % something bad has happened
      end,
      loop(Socket)
  end.

heartbeat(Socket) ->
  timer:sleep(1000),
  ets:foldl(
    fun({Key, ConnState}, _) ->
        {Host, Port} = Key
        %gen_udp:send(Socket, Host, Port, <<65:8>>),
    end, heartbeatnotused, udp_clients),
  heartbeat(Socket).

  
handle(Socket, Host, Port, ConnState, <<Command/utf8, Id:32, Seq:16, Ack:16, Appstring/binary>>) ->
  %io:format("handling: ~p~n", [{Host, Port, ConnState, Command, Id, Seq, Ack, Appstring}]),
  ets:update_counter(udp_clients, { Host, Port }, 1),
  ets:update_counter(pregistry, Id, 1),
  case Command of
    65 ->
      ok;
    _ -> 
    Outbound = <<65:8, Id:32, Seq:16, Ack:16, Appstring/binary>>,
    gen_udp:send(Socket, Host, Port, Outbound)
  end;
handle(_, _, _, _, _) -> err.

spam(Amount, Host, Port, PacketLength) ->
  {ok, Socket} = gen_udp:open(5599, [binary, {active, false}, {sndbuf, 65536}, {buffer, 65536}]),
  spam_impl(Amount, Socket, Host, Port, PacketLength),
  gen_udp:close(Socket).

spam_impl(0, _, _, _, _) -> ok;
spam_impl(Amount, Socket, Host, Port, PacketLength) ->
  Seq = 0,
  Ack = 0,
  Appstring = <<"boop">>,
  Outbound = <<99:8, Amount:32, Seq:16, Ack:16, Appstring/binary>>,
  timer:sleep(1),
  erlang:yield(),
  gen_udp:send(Socket, Host, Port, Outbound),
  ets:insert(pregistry, { Amount, 0 }),
  spam_impl(Amount -1, Socket, Host, Port, PacketLength).
  
