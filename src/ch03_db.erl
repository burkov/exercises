-module(ch03_db).
-author(alex_burkov).

-compile(export_all).

-include_lib("proper/include/proper.hrl").


-type db() :: [].
-type key() :: any().
-type value() :: any().

-spec new() -> db().
new() ->
  [].

-spec destroy(db()) -> ok.
destroy(_) ->
  ok.

-spec write(db(), key(), value()) -> db().
write(Db, Key, Value) ->
  [{Key, Value} | filter_out(Key, Db)].

-spec delete(db(), key()) -> db().
delete(Db, Key) ->
  filter_out(Key, Db).

-spec read(db(), key()) -> value() | {error, not_found}.
read(Db, Key) ->
  (fun
    Recur(_, []) -> {error, not_found};
    Recur(K, [{K, V} | _]) -> V;
    Recur(K, [_ | T]) -> Recur(K, T)
  end)(Key, Db).

-spec match(db, value()) -> [key()].
match(Db, Value) ->
  filtermap(fun({_, V}) -> {V =:= Value, Value} end, Db).

filter_out(Key, Db) -> filtermap(fun({K, _}) -> K =/= Key end, Db).
filtermap(F, Db) ->
  (fun
    Recur(_, [], Acc) -> Acc;
    Recur(Fun, [H | T], Acc) ->
      NewAcc =
        case Fun(H) of
          false -> Acc;
          {false, _} -> Acc;
          true -> [H | Acc];
          {true, NewValue} -> [NewValue | Acc]
        end,
      Recur(Fun, T, NewAcc)
  end)(F, Db, []).


%%%%

-record(state, {
  db :: db(),
  model :: [{key(), value()}]
}).

test() ->
  proper:module(?MODULE).

gen_existing_key(List) -> proper_types:elements(proplists:get_keys(List)).
gen_nonexisting_key(List) ->
  ?LET(X, proper_types:elements(lists:seq($a, $z) -- proplists:get_keys(List)), list_to_atom([X])).
gen_existing_value(List) -> proper_types:elements([X || {_, X} <- List]).
gen_nonexisting_value(List) -> proper_types:elements(lists:seq(1, 12) -- [X || {_, X} <- List]).

gen_key([]) -> gen_nonexisting_key([]);
gen_key(List) ->
  proper_types:frequency([
    {3, gen_existing_key(List)},
    {1, gen_nonexisting_key(List)}
  ]).

gen_value([]) -> gen_nonexisting_value([]);
gen_value(List) ->
  proper_types:frequency([
    {3, gen_existing_value(List)},
    {1, gen_nonexisting_value(List)}
  ]).

command(#state{db = Db, model = M}) ->
  proper_types:oneof([
    {call, ?MODULE, write, [Db, gen_key(M), gen_value(M)]},
    {call, ?MODULE, delete, [Db, gen_key(M)]},
    {call, ?MODULE, read, [Db, gen_key(M)]},
    {call, ?MODULE, match, [Db, gen_value(M)]}
  ]).

precondition(_State, _Call) -> true.

postcondition(#state{model = M}, {call, ?MODULE, match, [_Db, V]}, Res) ->
  Res =:= [X || {_, X} <- M, X =:= V];

postcondition(_State, {call, ?MODULE, write, [_Db, K, V]}, Res) ->
  ch03_db:read(Res, K) =:= V;

postcondition(_State, {call, ?MODULE, delete, [_Db, K]}, Res) ->
  ch03_db:read(Res, K) =:= {error, not_found};

postcondition(#state{model = M}, {call, ?MODULE, read, [_Db, K]}, Res) ->
  case lists:keymember(K, 1, M) of
    true ->
      {K, V} = lists:keyfind(K, 1, M),
      Res =:= V;
    false ->
      Res =:= {error, not_found}
  end;
postcondition(_State, _Call, _Res) -> true.

next_state(State, _Res, {call, ?MODULE, match, _}) -> State;
next_state(State, _Res, {call, ?MODULE, read, _}) -> State;
next_state(S, Res, {call, ?MODULE, write, [_Db, K, V]}) ->
  S#state{db = Res, model = lists:keystore(K, 1, S#state.model, {K, V})};
next_state(S, Res, {call, ?MODULE, delete, [_Db, K]}) ->
  S#state{db = Res, model = lists:keydelete(K, 1, S#state.model)}.

prop_db_all_operations() ->
  ?FORALL(Commands,
    proper_statem:commands(?MODULE, #state{db = {var, empty_db}, model = []}),
    begin
      {_History, _State, Result} = proper_statem:run_commands(?MODULE, Commands, [{empty_db, ch03_db:new()}]),
      proper:aggregate(
        proper_statem:command_names(Commands),
%%         Commands,
        Result =:= ok
      )
    end
  ).