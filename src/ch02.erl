-module(ch02).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

b_and(false, false) -> false;
b_and(true, true) -> true;
b_and(false, true) -> false;
b_and(true, false) -> false;
b_and(_, _) -> error(badarg).

b_or(true, false) -> true;
b_or(false, true) -> true;
b_or(true, true) -> true;
b_or(false, false) -> false;
b_or(_, _) -> error(badarg).

b_not(true) -> false;
b_not(false) -> true;
b_not(_) -> error(badarg).

b_nand(A, B) -> b_not(b_and(A, B)).


%%% tests

test() ->
  proper:module(?MODULE, [{numtests, 1024}]).

prop_b_not() ->
  ?FORALL(Bool, proper_types:bool(), proper:equals(b_not(Bool), not Bool)).

prop_b_or() ->
  ?FORALL({A, B}, {proper_types:bool(), proper_types:bool()}, proper:equals(b_or(A, B), A or B)).

prop_b_and() ->
  ?FORALL({A, B}, {proper_types:bool(), proper_types:bool()}, proper:equals(b_and(A, B), A and B)).

prop_b_nand() ->
  ?FORALL({A, B}, {proper_types:bool(), proper_types:bool()}, proper:equals(b_nand(A, B), not (A and B))).
