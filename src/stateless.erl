-module(stateless).
-author(alex_burkov).

-compile(export_all).


-include_lib("proper/include/proper.hrl").


gen_pure_fun() ->
  proper_types:function2(proper_types:int()).

%%%

gen_year() ->
  proper_types:choose(1970, 2014).

gen_month() ->
  proper_types:choose(1, 12).

-spec gen_day(fun(() -> boolean()), fun()) -> ok.
gen_day(Year, Month) ->
  proper_types:choose(1, calendar:last_day_of_the_month(Year, Month)).

gen_datetime() ->
  ?LET({Year, Month}, {gen_year(), gen_month()}, {Year, Month, gen_day(Year, Month)}).

%%%

gen_name() ->
  proper_types:oneof(["Alice", "Bob", "John", "Kate", "Mike", "Sara", "Jim", "Alex"]).

gen_gb_tree() ->
  ?SIZED(Size, do_gen_gb_tree(Size * 2)).

do_gen_gb_tree(0) -> {call, gb_trees, empty, []};
do_gen_gb_tree(Size) ->
  ?LAZY(proper_types:frequency([
    {1, {call, gb_trees, empty, []}},
    {3, {call, gb_trees, enter, [gen_name(), gen_datetime(), do_gen_gb_tree(Size - 1)]}}
  ])).

show() ->
  {ok, TreeSym} = proper_gen:pick(gen_gb_tree()),
  io:format("~p~n~n~s", [TreeSym, proper_symb:pretty_print(TreeSym)]).


%%
%% prop_nested() ->
%%   ?FORALL(X, gen_x(),
%%     ?SETUP(fun setup/0,
%%       ?WHENFAIL(fun cleanup/0,
%%         ?ALWAYS(N, ?FORALL(Y, gen_y(), prop_y(Y, X)))))).
%%
%% prop_implies() ->
%%   ?FORALL(X, gen_x(), ?IMPLIES(is_x_valid(X), prop_x())).

%%
%% gen_list() ->
%%   F = fun
%%     Recur(0, Acc) -> Acc;
%%     Recur(N, Acc) -> Recur(N - 1, [proper_types:choose(0,1024) | Acc])
%%   end,
%%   ?SIZED(Size, F(Size , [])).

prop_sorted() ->
  ?FORALL(List, gen_list(),
    begin
      is_sorted(lists:sort(List))
    end).


is_sorted([]) -> true;
is_sorted([_]) -> true;
is_sorted([H1 | [H2 | T]]) ->
  H1 =< H2 andalso is_sorted(T).


prop_appending() ->
  ?FORALL({ListA, ListB}, {gen_list(), gen_list()},
      ListA ++ ListB =:= lists:append(ListA, ListB)
  ).

prop_zipped() ->
  ?FORALL({ListA, ListB}, {gen_list(), gen_list()},
    ?IMPLIES(length(ListA) =:= length(ListB), is_zipped(ListA, ListB, lists:zip(ListA, ListB)))).


is_zipped([], [], []) -> true;
is_zipped([H1|T1], [H2|T2], [{H1, H2} | T3]) ->
  true andalso is_zipped(T1, T2, T3);
is_zipped(_, _, _) -> false.

%%%

gen_list() ->
  proper_types:non_empty(proper_types:list(proper_types:nat())).

gen_oneof([]) -> proper_types:nat();
gen_oneof(List) ->
  proper_types:frequency([
    {3, proper_types:oneof(List)},
    {10, proper_types:nat()}
  ]).

prop_delete() ->
  ?FORALL({List, OneOf}, ?LET(List, gen_list(), {List, gen_oneof(List)}),
    begin
      case lists:member(OneOf, List) of
        true ->
          count(OneOf, List) - 1 =:= count(OneOf, lists:delete(OneOf, List))
            andalso lists:sum(List) =:= lists:sum(lists:delete(OneOf, List)) + OneOf;
        false ->
          List =:= lists:delete(OneOf, List)
      end
    end).


count(_, []) -> 0;
count(E, [E|T]) -> 1 + count(E, T);
count(E, [_|T]) -> 0 + count(E, T).


-type weird_type() :: some_name | 0..42 | string() | inet:ip4_address().

typeof(X) when is_integer(X) -> '0..42';
typeof(X) when is_atom(X) -> 'some_name';
typeof(X) when is_list(X) -> 'string()';
typeof(X) when is_tuple(X) -> 'inet:ip_address()'.

prop_weird() ->
  ?FORALL(X, weird_type(), proper:collect(X, true)).


-type array_opt() :: fixed | non_neg_integer() | {size, non_neg_integer()} | {fixed, boolean()}.
-type array_opts() :: [array_opt()].

prop_array_opts() ->
  ?FORALL(Opts, array_opts(), proper:collect(Opts, array:is_array(array:new(Opts)))).


-spec divide(integer(), integer()) -> inf | float().
divide(_, 0) -> inf;
divide(A, B) -> A / B.





%%% simple compound generators
gen_lower() -> proper_types:choose($a, $z).
gen_upper() -> proper_types:choose($A, $Z).
gen_alpha() -> proper_types:oneof([gen_lower(), gen_upper()]).
gen_string() -> proper_types:non_empty(proper_types:list(gen_alpha())).
gen_int_function() -> proper_types:function2(proper_types:int()).

%% if no value is found within 100 attempts, then ?SUCHTHAT throws exception
gen_even() -> ?SUCHTHAT(X, proper_types:int(), X > 0 andalso X rem 2 =:= 0).





