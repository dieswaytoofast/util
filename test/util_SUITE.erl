-module(util_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("../src/defaults.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-define(CHECKSPEC(M,F,N), true = proper:check_spec({M,F,N})).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [{conversion, [],
      [t_get_string,
       t_get_binary,
       t_get_integer,
       t_get_boolean,
       t_get_base62]},


    {validation, [],
     [t_required,
      t_validate_list_of_binaries]}].

all() -> 
    [{group, conversion},
     {group, validation}].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

t_get_string(_Config) ->
    ?CHECKSPEC(util, get_string, 1).

t_get_binary(_Config) ->
    ?CHECKSPEC(util, get_binary, 1).

t_get_integer(_Config) -> 
    ?CHECKSPEC(util, get_integer, 1).

t_get_boolean(_Config) ->
    ?CHECKSPEC(util, get_boolean, 1).

t_get_base62(_Config) ->
    ?CHECKSPEC(util, get_base62, 1).

t_required(_Config) ->
    ?CHECKSPEC(util, required, 2).

t_validate_list_of_binaries(_Config) ->
    ?CHECKSPEC(util, validate_list_of_binaries, 1).
