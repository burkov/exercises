-module(statefull).
-author(alex_burkov).


-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(state, {
  accounts = [],
  logged_in = []
}).

gen_name() ->
  eqc_gen:oneof([alex, mike, bob, alice, joe, rick, sara, kate]).

pick_random(List) ->
  lists:nth(random:uniform(length(List)), List).

initial_state() -> #state{}.

login(A) -> io:format("~p~n", [A]).
login_pre(S) -> S#state.accounts =/= [].
login_args(S) -> [pick_random(S#state.accounts)].
login_next(S, _Result, [Picked]) ->
  S#state{accounts = S#state.accounts -- [Picked], logged_in = S#state.logged_in ++ [Picked]}.

logout_pre(S) -> S#state.logged_in =/= [].
logout_args(S) -> [pick_random(S#state.logged_in)].
logout_next(S, _Result, [Picked]) ->
  S#state{accounts = S#state.accounts ++ [Picked], logged_in = S#state.logged_in -- [Picked]}.

create_acc_args(S) ->
  [{?SUCHTHAT(Name, gen_name(),
    not lists:member(Name, proplists:get_keys(S#state.accounts ++ S#state.logged_in))), qwerty123}].
create_acc_next(S, _R, NewAccount) ->
  S#state{accounts = S#state.accounts ++ [NewAccount]}.

prop_sanity() ->
  eqc_statem:show_states(
    ?FORALL(
      Commands,
      commands(?MODULE),
      begin
        {H, S, Res} = run_commands(?MODULE, Commands),
        pretty_commands(?MODULE, Commands, {H, S, Res}, aggregate(command_names(Commands), Res == ok))
      end
    )).





