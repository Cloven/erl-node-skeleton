-module(skel_engine).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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
%% SkelEngine Function Exports
%% ------------------------------------------------------------------

-export([greet/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    start_http(),
    {ok, Args}.

handle_call(hello, _From, State) ->
    io:format("hello, world~n"),
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

greet() ->
  gen_server:call(?MODULE, {hello}).

start_http() ->
  Dispatch = cowboy_router:compile([
      %% {URIHost, list({URIPath, Handler, Opts})}
      {'_', [{'_', http_handler, []}]}
  ]),
  %% Name, NbAcceptors, TransOpts, ProtoOpts
  cowboy:start_http(http_listener, 100,
      [{port, 8080}],
      [{env, [{dispatch, Dispatch}]}]
  ).
