-module(roundrobin).
-author(brk).

-include_lib("proper/include/proper.hrl").

-behaviour(gen_server).

-compile(export_all).

-define(SERVER, ?MODULE).

-record(state, {ets :: ets:tid(), last = '$end_of_table', counter = 0 :: pos_integer()}).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:call(?SERVER, stop).

add(Item) -> gen_server:call(?SERVER, {add, Item}).
next() -> gen_server:call(?SERVER, next).
del(Item) -> gen_server:call(?SERVER, {del, Item}).

init([]) ->
  {ok, #state{ets = ets:new(rr, [public, named_table, ordered_set])}}.

handle_call({add, Item}, _From, #state{counter = C, ets = ETS, last = '$end_of_table'} = State) ->
%%   ets:insert(ETS, {C, Item}),
  {reply, ok, State#state{last = C, counter = C + 1}};

handle_call({add, Item}, _From, #state{ets = ETS, counter = C} = State) ->
%%   ets:insert(ETS, {C, Item}),
  {reply, ok, State#state{counter = C + 1}};

handle_call({del, Item}, _From, #state{ets = ETS, last = Item} = State) ->
%%   MaybeNext = ets:next(ETS, Item),
%%   ets:match_delete(ETS, {'_', Item}),
%%   Next = case MaybeNext of
%%     '$end_of_table' -> ets:first(ETS);
%%     _ -> MaybeNext
%%   end,
  {reply, ok, State#state{last = ok}};

handle_call({del, Item}, _From, #state{ets = ETS} = State) ->
%%   ets:match_delete(ETS, {'_', Item}),
  {reply, ok, State};

handle_call(next, _From, #state{last = '$end_of_table'} = State) ->
  {reply, '$end_of_table', State};

handle_call(next, _From, #state{ets = ETS, last = Last} = State) ->
%%   Next = cycled_next(ETS, Last),
%%   [{_,V}] = ets:lookup(ETS, Last),
  {reply, ok, State#state{last = ok}};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(Request, From, State) ->
  {stop, {unsupported_call, From, Request}, {error, unsupported}, State}.

handle_cast(Request, State) ->
  {stop, {unsupported_cast, Request}, State}.

handle_info(Info, State) ->
  {stop, {unsupported_info, Info}, State}.

terminate(Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

cycled_next(ETS, Element) ->
  case ets:next(ETS, Element) of
    '$end_of_table' -> ets:first(ETS);
    V -> V
  end.


%%%%%%%

test() -> proper:quickcheck(prop_basic()).

prop_basic() ->
  ?FORALL(Cmds, proper_statem:commands(?MODULE),
    proper:aggregate(
      proper_statem:command_names(Cmds),
      ?TRAPEXIT(
        begin
          roundrobin:start_link(),
          {H, S, Res} = proper_statem:run_commands(?MODULE, Cmds),
          ok = roundrobin:stop(),
          Res =:= ok
        end))
  ).


-record(model, {queue :: queue:queue()}).

initial_state() -> #model{queue = queue:new()}.

gen_item(#model{queue = Q}, IsNewProbability, InQueueProbability) ->
  case queue:is_empty(Q) of
    true -> proper_types:nat();
    false ->
      proper_types:frequency([
        {IsNewProbability, ?SUCHTHAT(X, proper_types:nat(), not queue:member(X, Q))},
        {InQueueProbability, proper_types:elements(queue:to_list(Q))}
      ])
  end.

precondition(_Model, {call, roundrobin, _Function, _Args}) -> true.

command(Model) ->
  Calls = [
    {call, roundrobin, next, []},
    {call, roundrobin, add, [gen_item(Model, 3, 1)]},
    {call, roundrobin, del, [gen_item(Model, 1, 5)]}
  ],
  Len = queue:len(Model#model.queue),
  Probabilities =
    if
      Len =< 3 -> [1,4,1];
      Len >= 10 -> [8,1,3];
      true -> [6,1,2]
    end,
  proper_types:frequency(lists:zip(Probabilities, Calls)).

postcondition(#model{queue = Q}, {call, roundrobin, next, []}, Res) ->
  case queue:out(Q) of
    {empty, _} -> Res =:= '$end_of_table';
    {{value, X}, _} -> Res =:= X
  end, true;
postcondition(#model{}, {call, roundrobin, add, [Item]}, _Res) -> true;
postcondition(#model{}, {call, roundrobin, del, [Item]}, _Res) -> true.

next_state(M, _Res, {call, roundrobin, next, []}) ->
  case queue:out(M#model.queue) of
    {empty, _} -> M;
    {{value, First}, Q2} -> M#model{queue = queue:in(First, Q2)}
  end;
next_state(M, _Res, {call, roundrobin, add, [Item]}) ->
  M#model{queue = queue:in(Item, M#model.queue)};
next_state(M, _Res, {call, roundrobin, del, [Item]}) ->
  M#model{queue = queue:filter(fun(X) -> X =/= Item end, M#model.queue)}.







