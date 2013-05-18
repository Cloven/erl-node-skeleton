-module(udp_server).
-export([start/0]).

start() ->
  spawn(fun() -> server(9595) end).

server(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
  io:format("server opened socket:~p~n",[Socket]),
  ets:new(udp_clients, [set, named_table]),
  spawn(fun() -> heartbeat() end),
  loop(Socket).

loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, Socket, Host, Port, Bin} ->
      io:format("received ~p~n", [{ Host, Port, Bin }]),
      case ets:lookup(udp_clients, { Host, Port }) of
        [] -> 
          io:format("never seen ~p before~n", [{ Host, Port } ]),
          ets:insert(udp_clients, {{ Host, Port }, 1}),
          handle(Socket, Host, Port, {}, Bin);
        [{{Host, Port}, HostInfo}] ->
          io:format("oh hey return visitor ~p~n", [{Host, Port, HostInfo}]),
          handle(Socket, Host, Port, HostInfo, Bin);
        [_Unknown] -> 
          io:format("oh hey WTF visitor ~p~n", [{Host, _Unknown}])
      end,
      loop(Socket)
  end.

heartbeat() ->
  timer:sleep(1000),
  ets:foldl(
    fun({Host, Port}, DontCare) ->
        io:format("hello to ~p", [{Host, Port}]),
        DontCare
    end, notused, udp_clients),
  heartbeat().

  
handle(Socket, Host, Port, HostInfo, <<Command/utf8, Id:16, Seq:16, Ack:16, Appstring/binary>>) ->
  Outbound = <<65:8, Id:16, Seq:16, Ack:16, Appstring/binary>>,
  io:format("handling: ~p~n", [{Host, Port, HostInfo, Command, Id, Seq, Ack, Appstring}]),
  gen_udp:send(Socket, Host, Port, Outbound);
handle(_, _, _, _, _) -> err.
