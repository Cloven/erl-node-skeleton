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

handle_info(Info, State) ->
    io:format("got message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

server(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
  io:format("server opened socket:~p~n",[Socket]),
  ets:new(udp_clients, [set, named_table]),
  Heartbeat = spawn(fun() -> heartbeat(Socket) end),
  link(Heartbeat),
  loop(Socket).

loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, Socket, Host, Port, Bin} ->
      io:format("received ~p~n", [{ Host, Port, Bin }]),
      case ets:lookup(udp_clients, { Host, Port }) of
        [] -> 
          % new client
          io:format("never seen ~p before~n", [{ Host, Port } ]),
          ets:insert(udp_clients, { { Host, Port }, {} }),
          handle(Socket, Host, Port, {}, Bin);
        [{{Host, Port}, HostInfo}] ->
          % previously seen client
          io:format("oh hey return visitor ~p~n", [{Host, Port, HostInfo}]),
          handle(Socket, Host, Port, HostInfo, Bin);
        [_Unknown] -> 
          % something bad has happened
      end,
      loop(Socket)
  end.

heartbeat(Socket) ->
  io:format("heartbeating in 1s~n"),
  timer:sleep(1000),
  ets:foldl(
    fun({Key, _}, DontCare) ->
        {Host, Port} = Key,
        io:format("sending heartbeat: ~p~n", [{Host, Port}]),
        gen_udp:send(Socket, Host, Port, <<65:8>>),
        DontCare
    end, notused, udp_clients),
  heartbeat(Socket).

  
handle(Socket, Host, Port, HostInfo, <<Command/utf8, Id:16, Seq:16, Ack:16, Appstring/binary>>) ->
  Outbound = <<65:8, Id:16, Seq:16, Ack:16, Appstring/binary>>,
  io:format("handling: ~p~n", [{Host, Port, HostInfo, Command, Id, Seq, Ack, Appstring}]),
  gen_udp:send(Socket, Host, Port, Outbound);
handle(_, _, _, _, _) -> err.
