-module(mori_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Local Function Exports
%% ------------------------------------------------------------------

-export([reserve/0,info/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  Serv = spawn(fun() -> server(9595) end),
  ClientSlots = lists:duplicate(16, 0),
  link(Serv),
  State = { Serv, ClientSlots },
  {ok, State}.

handle_call(new_client, _From, State) ->
  io:format("new_client!~n", []),
  ClientId = 2432,
  {reply, {ok, ClientId}, State};
handle_call(info, _From, State) ->
  {reply, ok, State};
handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%%

reserve() ->
  gen_server:call(?MODULE, {reserve}).

info() ->
  gen_server:call(?MODULE, {info}).

server(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, false}, {recbuf, 65536}, {sndbuf, 65536}, {buffer, 65536}, {read_packets, 16000}]),
  io:format("mori starting.  Socket:~p~n",[Socket]),
  ets:new(udp_clients, [set, named_table]),
  Heartbeat = spawn(fun() -> heartbeat(Socket) end),
  link(Heartbeat),
  socket_loop(Socket).

socket_loop(Socket) ->
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
      socket_loop(Socket)
  end.

heartbeat(Socket) ->
  timer:sleep(1000),
  ets:foldl(
    fun({Key, _}, _) ->
        {Host, Port} = Key,
        gen_udp:send(Socket, Host, Port, <<65:8, 1:32, 1:16, 1:16, "pong" >>)
    end, heartbeatnotused, udp_clients),
  heartbeat(Socket).

handle(Socket, Host, Port, ConnState, <<Command/utf8, Id:32, Seq:16, Ack:16, Appstring/binary>>) ->
  io:format("handling: ~p~n", [{Host, Port, ConnState, Command, Id, Seq, Ack, Appstring}]),
  case Command of
    65 ->
      io:format("got back packet with id ~p~n", [Id]),
      ets:update_counter(udp_clients, { Host, Port }, 1),
      ok;
    _ -> 
    Outbound = <<65:8, Id:32, Seq:16, Ack:16, Appstring/binary>>,
    gen_udp:send(Socket, Host, Port, Outbound)
  end;
handle(_, _, _, _, _) -> err.
