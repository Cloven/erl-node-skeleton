-module(mori_client).
-behaviour(gen_fsm).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([start/1, init/1, handle_event/3, handle_sync_event/4, 
         handle_info/3, terminate/3, code_change/4]).

-export([s_connected/2, s_connected/3, s_playing/2, s_dead/2]).

%% ------------------------------------------------------------------
%% Internal Record Definition
%% ------------------------------------------------------------------

-record(cs,
  {
    conn_info,
    packet_number = 0,
    packets_recvd = 0,
    unacked = [],
    mode_coalesce = 0,
    mode_enforce = 0,
    mode_debug = 0
  }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(State) ->
  {ok, Pid}  = start_link(State),
  Pid.

start_link(State) ->
    gen_fsm:start_link(?MODULE, State, []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(NetworkState) ->
    %{Socket, Host, Port} = NetworkState,
    io:format("client init~n",[]),
    State = #cs{conn_info = NetworkState},
    {ok, s_connected, State}.

s_connected(_Event, State) ->
    {next_state, s_playing, State}.

s_connected(_Event, _From, State) ->
    %io:format("3 form moving into s_playing state~n"),
    {reply, ok, s_playing, State}.

s_playing(_Event, State) ->
    %io:format("moving into s_dead state~n"),
    {Socket, Host, Port} = State,
    gen_udp:send(Socket, Host, Port, << "trilobites are glorious" >>),
    {next_state, s_playing, State}.

s_dead(_Event, State) ->
    io:format("leaving now~n"),
    {_, Host, Port} = State,
    {stop, {shutdown, {Host, Port}}, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info( { cmd, <<1:8, Seq:16, Seen:16, Ack:32, Cmd:8, Arg/binary>> }, StateName, State) ->
  {Socket, Host, Port} = State#cs.conn_info,
  PacketsRecvd = State#cs.packets_recvd + 1,
  case Cmd of
    $R ->
    io:format("reflecting a packet~n",[]),
    OutString = io_lib:format("~p~n", [{Cmd, Seq, Seen, Ack, Arg}]),
    gen_udp:send(Socket, Host, Port, OutString);
    $J ->
      io:format("got a join packet~n",[]);
    _ -> 
      io:format("got a nonreflecting packet: ~p~n",[[Seq, Seen, Ack, Cmd, Arg]]),
      ok
  end,
  NewState = State#cs{packets_recvd = PacketsRecvd},
  {next_state, StateName, NewState};
handle_info({cmd, Unrecognized}, StateName, State) ->
  io:format("unrecognized packet: ~p~n", [Unrecognized]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

