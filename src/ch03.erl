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

