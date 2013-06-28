-module(mori_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mori_sup:start_link().

start() ->
  application:start(mori).

stop(_State) ->
    ok.
