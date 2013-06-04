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

init(State) ->
    {Socket, Host, Port} = State,
    gen_udp:send(Socket, Host, Port, <<"trilobites are truly glorious">>),
    {ok, s_connected, State}.

s_connected(_Event, State) ->
    {next_state, s_playing, State}.

s_connected(_Event, _From, State) ->
    io:format("3 form moving into s_playing state~n"),
    {reply, ok, s_playing, State}.

s_playing(_Event, State) ->
    io:format("moving into s_dead state~n"),
    {Socket, Host, Port} = State,
    gen_udp:send(Socket, Host, Port, << "trilobites are glorious" >>),
    {next_state, s_dead, State}.

s_dead(_Event, State) ->
    io:format("leaving now~n"),
    {_, Host, Port} = State,
    {stop, {shutdown, {Host, Port}}, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info({cmd, Bin}, StateName, State) ->
  case Bin of
    <<R:8, C:16, Rest/binary>> ->
      ets:update_counter(udp_clients, packets_seen, 1),
      {Socket, Host, Port} = State,
      gen_udp:send(Socket, Host, Port, << "trilobites are glorious" >>);
    <<Failure/binary>> ->
      io:format("not well formed ~p~n", [Failure])
  end,
  {next_state, StateName, State};
handle_info(_, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

