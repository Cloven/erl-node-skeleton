-module(bops).

-export([brl/2,brl/3,bset/2,btest/0]).

brl(Bin, N) ->
  <<_:N, Rotted/bits>> = Bin,
  <<Rotted/bits, 0:N>>.

% broken
brl(Bin, N, Fill) ->
  <<_:N, Rotted/bits>> = Bin,
  <<Rotted/bits, Fill:N>>.

bset(Bin,Pos) ->
  <<_:Pos, B:1, _/bits>> = Bin,
  B == 1.


btest() ->
  X = <<0:1,1:1,0:1,1:1,0:1,0:1,1:1,1:1>>,
  io:fwrite("~.2B~n", [X]),
  X1 = brl(X, 1),
  io:fwrite("~.2B~n", [X1]),
  X2 = brl(X1, 1),
  io:fwrite("~.2B~n", [X2]),
  X3 = brl(X2, 1),
  io:fwrite("~.2B~n", [X3]),
  X4 = brl(X3, 1),
  io:fwrite("~.2B~n", [X4]),
  X5 = brl(X4, 1),
  io:fwrite("~.2B~n", [X5]),
  X6 = brl(X5, 1),
  io:fwrite("~.2B~n", [X6]),
  X7 = brl(X6, 1),
  io:fwrite("~.2B~n", [X7]),
  X8 = brl(X7, 1),
  io:fwrite("~.2B~n", [X8]).
