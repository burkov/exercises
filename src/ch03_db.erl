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
    Recur(_, []) -> [];
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
  db :: db()
}).

test() ->
  proper:module(?MODULE).

initial_state() ->
  #state{}.

gen_key() -> ?LET(X, proper_types:integer($a, $z), list_to_atom([X])).
gen_value() -> proper_types:list(proper_types:nat()).

command(#state{db = Db}) ->
  proper_types:oneof([
    {call, ?MODULE, write, [Db, gen_key(), gen_value()]},
    {call, ?MODULE, delete, [Db, gen_key()]},
    {call, ?MODULE, read, [Db, gen_key()]},
    {call, ?MODULE, match, [Db, gen_value()]}
  ]).

precondition(_State, _Call) -> true.
postcondition(_State, _Call, _Res) -> true.
next_state(State, _Res, {call, ?MODULE, match, _}) -> State;
next_state(State, _Res, {call, ?MODULE, read, _}) -> State;
next_state(State, Res, _Call) -> State#state{db = Res}.

prop_db() ->
  ?FORALL(Commands,
    proper_statem:commands(?MODULE, #state{db = {var, empty_db}}),
    begin
      {_History, _State, Result} = proper_statem:run_commands(?MODULE, Commands, [{empty_db, ch03_db:new()}]),
      proper:aggregate(
        proper_statem:command_names(Commands),
        Result =:= ok
      )
    end
  ).




