-module(ch03).
-author(alex_burkov).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").



sum(1) -> 1;
sum(N) when N > 1 -> sum(N-1) + N.


sum(M, M) -> M;
sum(N, M) when N < M -> sum(N + 1, M) + N;
sum(N, M) when N > M -> exit(badarg).


prop_sum1() ->
  ?FORALL(X, eqc_gen:)


