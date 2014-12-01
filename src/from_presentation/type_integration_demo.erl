-module(type_integration_demo).

%% NOTE: Add -export_type([array_opts/0]). to array.erl and recompile it with
%% debug_info enabled before compiling this file.

%% NOTE: Compile this file with debug_info enabled.

-include_lib("proper/include/proper.hrl").

%% We want to test that array:new/0 can handle any combination of options.
%% Instead of writing a custom generator (which may rot), we can use the type
%% in that file:

%% from array.erl:
%% -type array_opt() :: 'fixed' | non_neg_integer()
%% 		   | {'default', term()} | {'fixed', boolean()}
%% 		   | {'size', non_neg_integer()}.
%% -type array_opts() :: array_opt() | [array_opt()].

prop_new_array_opts() ->
    ?FORALL(Opts, array:array_opts(), array:is_array(array:new(Opts))).
