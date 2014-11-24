-module(ch03).
-author(alex_burkov).

-compile(export_all).

-include_lib("proper/include/proper.hrl").



sum(1) -> 1;
sum(N) when N > 1 -> sum(N - 1) + N.


sum(M, M) -> M;
sum(N, M) when N < M -> sum(N + 1, M) + N;
sum(N, M) when N > M -> exit(badarg).


prop_sum1() ->
  ?FORALL(X, proper_types:choose(1, 1024), lists:sum(lists:seq(1, X)) =:= ch03:sum(X)).

prop_sum2() ->
  ?FORALL({N, M}, ?SUCHTHAT({N, M}, {proper_types:choose(1, 1024), proper_types:choose(1, 1024)}, N =< M),
    lists:sum(lists:seq(N, M)) =:= ch03:sum(N, M)).



