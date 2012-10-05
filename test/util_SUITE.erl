-module(util_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("../src/defaults.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-define(CHECKSPEC(M,F,N), true = proper:check_spec({M,F,N})).
-define(PROPTEST(A), true = proper:quickcheck(A())).

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
      t_validate_list_of_binaries]},

    {json, [],
     [t_validate_boolean,
      t_validate_boolean_list,
      t_validate_boolean_list_generated,
      t_validate_integer,
      t_validate_integer_list,
      t_validate_integer_list_generated,
      t_validate_binary,
      t_validate_binary_list,
      t_validate_binary_list_generated,
      t_validate_undefined,
      t_validate_undefined_generated,
      t_validate_null,
      t_validate_null_generated,
      t_validate_ignore]}].

all() ->
    [{group, conversion},
     {group, validation},
     {group, json}].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

t_get_string(_) ->
    ?CHECKSPEC(util, get_string, 1).

t_get_binary(_) ->
    ?CHECKSPEC(util, get_binary, 1).

t_get_integer(_) ->
    ?CHECKSPEC(util, get_integer, 1).

t_get_boolean(_) ->
    ?CHECKSPEC(util, get_boolean, 1).

t_get_base62(_) ->
    ?CHECKSPEC(util, get_base62, 1).

t_required(_) ->
    ?CHECKSPEC(util, required, 2).

t_validate_list_of_binaries(_) ->
    ?CHECKSPEC(util, validate_list_of_binaries, 1).

t_validate_boolean(_) ->
    ?CHECKSPEC(json, validate_boolean, 1).

t_validate_boolean_list(_) ->
    ?CHECKSPEC(json, validate_boolean_list, 1).

t_validate_boolean_list_generated(_) ->
    ?PROPTEST(prop_validate_boolean_list_generated).

prop_validate_boolean_list_generated() ->
    ?FORALL(L, json_boolean_list(),
            case json:validate_boolean_list(L) of
                {error, _} -> false;
                _ -> true
            end).

t_validate_integer(_) ->
    ?CHECKSPEC(json, validate_integer, 1).

t_validate_integer_list(_) ->
    ?CHECKSPEC(json, validate_integer_list, 1).

t_validate_integer_list_generated(_) ->
    ?PROPTEST(prop_validate_integer_list_generated).

prop_validate_integer_list_generated() ->
    ?FORALL(L, json_integer_list(),
            case json:validate_integer_list(L) of
                {error, _} ->
                    false;
                _ ->
                    true
            end).

t_validate_binary(_) ->
    ?CHECKSPEC(json, validate_binary, 1).

t_validate_binary_list(_) ->
    ?CHECKSPEC(json, validate_binary_list, 1).

t_validate_binary_list_generated(_) ->
    ?PROPTEST(prop_validate_binary_list_generated).

prop_validate_binary_list_generated() ->
    ?FORALL(L, list(binary()),
            case json:validate_binary_list(L) of
                {error, _} ->
                    false;
                _ ->
                    true
            end).

t_validate_undefined(_) ->
    ?CHECKSPEC(json, validate_undefined, 1).

t_validate_undefined_generated(_) ->
    ?PROPTEST(prop_validate_undefined_generated).

prop_validate_undefined_generated() ->
    ?FORALL(U, any(), U =:= json:validate_undefined(U)).

t_validate_null(_) ->
    ?CHECKSPEC(json, validate_null, 1).

t_validate_null_generated(_) ->
    ?PROPTEST(prop_validate_null_generated).

prop_validate_null_generated() ->
    ?FORALL(U, weighted_union([{1,null}, {10, any()}]), ?IMPLIES(U =:= null, json:validate_null(U) =:= undefined)).

t_validate_ignore(_) ->
    ?CHECKSPEC(json, validate_ignore, 1).

%% generator for expected json boolean values
binary_boolean() ->
    ?LET(B, oneof([true_list(), false_list()]), list_to_binary(B)).
true_list() ->
    [oneof([$t, $T]),oneof([$r, $R]),oneof([$u, $U]),oneof([$e, $E])].
false_list() ->
    [oneof([$f, $F]),oneof([$a, $A]),oneof([$l, $L]),oneof([$s, $S]),oneof([$e, $E])].
json_boolean() ->
    oneof([boolean(), binary_boolean()]).
json_boolean_list() ->
    list(json_boolean()).

%% generator for expected integer
binary_integer() ->
    ?LET(B, integer(), list_to_binary(integer_to_list(B))).
json_integer() ->
    oneof([integer(), binary_integer()]).
json_integer_list() ->
    list(json_integer()).
