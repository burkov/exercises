-module(pdict).
-behaviour(proper_statem).

-export([test/0, test/1]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
	 next_state/3]).

-include_lib("proper/include/proper.hrl").

-type key() :: 'a' | 'b' | 'c' | 'd'.
-type value() :: integer().

-define(KEYS, [a,b,c,d]).
key() -> union(?KEYS).
value() -> integer().

%% A simple statem test for the process dictionary; tests the
%% operations erlang:put/2, erlang:get/1, erlang:erase/1.

test() ->
    test(100).

test(N) ->
    proper:quickcheck(?MODULE:prop_pdict(), N).

prop_pdict() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{H,S,Res} = run_commands(?MODULE, Cmds),
		clean_up(),
		?WHENFAIL(
		   io:format("History: ~w\nState: ~w\nRes: ~w\n",
			     [H, S, Res]),
		   aggregate(command_names(Cmds), Res =:= ok))
	    end).

clean_up() ->
    lists:foreach(fun(Key) -> erlang:erase(Key) end, ?KEYS).

initial_state() -> [].

-spec command([{key(),value()}]) -> proper_types:type().
command([]) ->
    {call,erlang,put,[key(), value()]};
command(State) ->
    union([{call,erlang,put,[key(),value()]},
	   {call,erlang,get,[key(State)]},
	   {call,erlang,erase,[key(State)]}]).

key(State) ->
    ?LET({Key,_}, elements(State), Key).

precondition(_, _) ->
    true.

%% Set the right preconditions:
%% precondition(State, {call,erlang,get,[Key]}) ->
%%     proplists:is_defined(Key, State);
%% precondition(State, {call,erlang,erase,[Key]}) ->
%%     proplists:is_defined(Key, State);
%% precondition(_, {call,erlang,put,[_K, _V]}) ->
%%     true.

postcondition(State, {call,erlang,put,[Key,_]}, undefined) ->
    not proplists:is_defined(Key, State);
postcondition(State, {call,erlang,put,[Key,_]}, Old) ->
    {Key,Old} =:= proplists:lookup(Key, State);
postcondition(State, {call,erlang,get,[Key]}, Val) ->
    {Key,Val} =:= proplists:lookup(Key, State);
postcondition(State, {call,erlang,erase,[Key]}, Val) ->
    {Key,Val} =:= proplists:lookup(Key, State).

next_state(State, _Var, {call,erlang,put,[Key,Value]}) ->
    State ++ [{Key,Value}];
    %% Fix the bug in next_state:
    %% [{Key,Value}|proplists:delete(Key, State)];
next_state(State, _Var, {call,erlang,erase,[Key]}) ->
    proplists:delete(Key, State);
next_state(State, _Var, {call,erlang,get,[_]}) ->
    State.
