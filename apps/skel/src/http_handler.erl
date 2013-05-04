-module(http_handler).
-behaviour(cowboy_http_handler).
 
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
 
init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.
 
handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], <<"Hello World!">>, Req),
    {ok, Req2, State}.
 
terminate(Reason, Req, State) ->
    ok.
