-module(ch03).
-author(alex_burkov).

-compile(export_all).

-include_lib("proper/include/proper.hrl").



sum(1) -> 1;
sum(N) when N > 1 -> sum(N - 1) + N.

sum(M, M) -> M;
sum(N, M) when N < M -> sum(N + 1, M) + N;
sum(N, M) when N > M -> exit(badarg).

create(X) when X >= 0 ->
  (fun
    Recur(0, Acc) -> Acc;
    Recur(N, Acc) -> Recur(N - 1, [N | Acc])
  end)(X, []).

reverse_create(0) -> [];
reverse_create(N) when N >= 0 ->
  [N | reverse_create(N - 1)].

print_nums(X) ->
  (fun
    Recur(N, N) -> io:format("~p~n", [N]);
    Recur(M, N) -> io:format("~p~n", [M]), Recur(M + 1, N)
  end)(1, X).

print_evens(X) when X >= 1 ->
  (fun
    Recur(N, N) -> io:format("~p~n", [N]);
    Recur(M, N) when M rem 2 =:= 0 -> io:format("~p~n", [M]), Recur(M + 1, N);
    Recur(M, N) -> Recur(M+1, N)
  end)(1, X).

filter([], _) -> [];
filter([H|T], N) when H =< N -> [H | filter(T, N)];
filter([_|T], N) -> filter(T, N).


reverse(List) ->
  (fun
    Recur([], Acc) -> Acc;
    Recur([H|T], Acc) -> Recur(T, [H|Acc])
  end)(List, []).

concatenate(List) ->
  Join = fun
    Recur([], Acc) -> Acc;
    Recur([H|T], Acc) -> Recur(T, [H|Acc])
  end,
  reverse((fun
    Recur([], Acc) -> Acc;
    Recur([H|T], Acc) -> Recur(T, Join(H, Acc))
  end)(List, [])).

flatten(ListOfLists) ->
  reverse((fun
    Recur([], Acc) -> Acc;
    Recur([H|T], Acc) when is_list(H) -> Recur(T, concatenate([Recur(H, []), Acc]));
    Recur([H|T], Acc) -> Recur(T, [H|Acc])
  end)(ListOfLists, [])).

%%% Proper tests

test() ->
  proper:module(?MODULE).

prop_sum1() ->
  ?FORALL(N, proper_types:pos_integer(), lists:sum(lists:seq(1, N)) =:= ch03:sum(N)).

prop_sum2() ->
  ?FORALL(
    {N, M},
    ?SUCHTHAT({N, M}, {proper_types:pos_integer(), proper_types:pos_integer()}, N =< M),
    lists:sum(lists:seq(N, M)) =:= ch03:sum(N, M)
  ).

prop_create() ->
  ?FORALL(N, proper_types:pos_integer(), lists:seq(1, N) =:= create(N)).

prop_reverse_create() ->
  ?FORALL(N, proper_types:pos_integer(), lists:reverse(lists:seq(1, N)) =:= reverse_create(N)).

prop_filter() ->
  ?FORALL(
    {List, N},
    ?LET(L, proper_types:non_empty(proper_types:list(proper_types:pos_integer())), {L, proper_types:elements(L)}),
    [X || X <- List, X =< N] =:= filter(List, N)
  ).

prop_reverse() ->
  ?FORALL(List, proper_types:non_empty(proper_types:list()), lists:reverse(List) =:= reverse(List)).

prop_concatenate() ->
  ?FORALL(ListOfLists, proper_types:non_empty(proper_types:list(proper_types:list())),
    proper:collect(
%%       ListOfLists,
      length(ListOfLists),
      lists:append(ListOfLists) =:= concatenate(ListOfLists))
)
.