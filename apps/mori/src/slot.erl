-module(slot).

-export([reserve/2, release/2]).

reserve(L, X) ->
  reserve(L, X, []).

reserve([E | Rest], Insertion, AccList) ->
  case E == 0 of
    true -> {ok, lists:reverse(lists:reverse(Rest) ++ [ Insertion | AccList])};
    false -> reserve(Rest, Insertion, [E | AccList])
  end;
reserve([], _, AccList) -> {noslot, lists:reverse(AccList)}.

release(L, X) ->
  Pos = findinlist(L, X),
  case Pos of
    false -> {noslot, L};
    _ -> {ok, setnth(Pos, L, 0)}
  end.

% Thanks Robert Virding
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)];
setnth(_, [], New) -> New.

%%
%% findinlist(List, Element) -> list_index().
%%

findinlist(L, X) ->
  findinlist(L, X, 1).

findinlist([], _, _) -> 
  false;
findinlist([E | Rest], X, Acc) ->
  case E == X of
    true -> Acc;
    false -> findinlist(Rest, X, Acc + 1)
  end.
