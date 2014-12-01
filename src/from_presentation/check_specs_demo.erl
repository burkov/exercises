-module(check_specs_demo).
-export([divide/2, filter/2, max/1]).

%% NOTE: Must compile with debug_info enabled.

%% Test each function's spec with proper:check_spec(MFA), or the whole module
%% at once with proper:check_specs(check_specs_demo).

-spec divide(integer(), integer()) -> integer().
%% Should be: divide(integer(), pos_integer() | neg_integer()) -> integer().
divide(A, B) ->
    A div B.

-spec filter(fun((T) -> term()), [T]) -> [T].
%% Should be: filter(fun((T) -> boolean()), [T]) -> [T].
filter(Pred, List) ->
    lists:filter(Pred, List).

-spec max([T]) -> T.
%% Should be: max([T,...]) -> T.
max(List) ->
    lists:max(List).
