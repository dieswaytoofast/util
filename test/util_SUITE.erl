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
    [{ct_hooks,[cth_surefire]}, {timetrap,{minutes,1}}].

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
    [{conversion, [parallel],
      [t_get_string,
       t_get_binary,
       t_get_integer,
       t_get_boolean,
       t_get_base36,
       t_get_base62]},


    {validation, [parallel],
     [t_required,
      t_validate_list_of_binaries,
      t_validate_email_address,
      t_validate_email_address_correct]},
     
    {list, [],
     [t_get_value_3,
      t_get_value_4,
      t_get_value_4_generated]},

    {uuid, [],
     [t_create_uuid]},

    {json, [parallel],
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
      t_validate_ignore,
      t_validate_atom,
      t_validate_url,
      t_validate_tcp_port,
      t_validate_utf8,
      t_validate_utf8_spec,
      t_validate_list_with,
      t_validate_area_code]}].

all() ->
    [{group, conversion},
     {group, validation},
     {group, list},
     {group, uuid},
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

t_get_base36(_) ->
    ?CHECKSPEC(util, get_base36, 1).

t_required(_) ->
    ?CHECKSPEC(util, required, 2).

t_validate_list_of_binaries(_) ->
    ?CHECKSPEC(util, validate_list_of_binaries, 1).

t_get_value_3(_) ->
    ?CHECKSPEC(util, get_value, 3).

t_get_value_4(_) ->
    ?CHECKSPEC(util, get_value, 4).

t_create_uuid(_) ->
    ?CHECKSPEC(util, create_uuid, 0).
t_validate_email_address(_) ->
    ?CHECKSPEC(util, validate_email_address, 1).

t_validate_email_address_correct(_) ->
    ?PROPTEST(prop_email_address).

prop_email_address() ->
    ?FORALL(E, email(), E =:= util:validate_email_address(E)).

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

t_get_value_4_generated(_) ->
    ?PROPTEST(prop_get_value_4_generated).

prop_get_value_4_generated() ->
    ?FORALL(T, tuple_list(),
            case util:get_value(foo, 1, undefined, T) of
                undefined ->
                    true;
                bar ->
                    false;
                _ ->
                    false
            end).
    
%% generator for expected list of tuples
tuple_list() ->
    ?LET(T, oneof([contains_val_list(), not_contains_val_list()]), T).
contains_val_list() ->
    list({term(), term()}), {foo, bar}, list({term(), term()}).
not_contains_val_list() ->
    list({term(), term()}).

%% generator for expected integer
binary_integer() ->
    ?LET(B, integer(), list_to_binary(integer_to_list(B))).
json_integer() ->
    oneof([integer(), binary_integer()]).
json_integer_list() ->
    list(json_integer()).

t_validate_atom(_) ->
    ?CHECKSPEC(json, validate_atom, 2).

t_validate_url(_) ->
    Url = <<"http://foo.bar.com?a=b">>,
    Url = util:validate_url(Url),
    Url2 = <<"https://foo.bar.com?a=b">>,
    Url2 = util:validate_url(Url2),
    BadUrl = <<"htt://foo.bar.com?a=b">>,
    {error, {?INVALID_URL, [BadUrl]}} = util:validate_url(BadUrl).

t_validate_tcp_port(_) ->
    ?CHECKSPEC(util, validate_tcp_port, 1).

t_validate_utf8(_) ->
    ?PROPTEST(prop_validate_utf8).

prop_validate_utf8() ->
    ?FORALL(B, proper_stdgen:utf8_bin(), B =:= util:validate_utf8(B)).

t_validate_utf8_spec(_) ->
    ?CHECKSPEC(util, validate_utf8, 1).

t_validate_list_with(_) ->
    ?PROPTEST(prop_validate_list_with).

prop_validate_list_with() ->
    ?FORALL(L, list(integer()), L =:= util:validate_list_with({json, validate_integer}, L)).

t_validate_area_code(_) ->
    ?PROPTEST(prop_validate_area_code).

prop_validate_area_code() ->
    ?FORALL(L, [integer($2, $9), integer($0, $9), integer($0, $9)],
            begin
                AreaCode = list_to_binary(L),
                AreaCode =:= util:validate_area_code(AreaCode)
            end).

email_local_part() ->
    non_empty(list(oneof([integer($a, $z), integer($A, $Z), integer($0, $9), $-, $!, $#, $$, $%, $&, $', $*, $/, $=, $?, $^, $_, $`, ${, $|, $}, $~, $-]))).

label() ->
    non_empty(list(oneof([integer($a, $z), integer($A, $Z), integer($0, $9), $-]))).

email_domain() ->
  ?SUCHTHAT(Hostname,
    ?LET(Labels,
      non_empty(list(label())),
      string:join(Labels, ".")),
    length(Hostname) < 256).

email() ->
    ?SUCHTHAT(Email,
              ?LET({LocalPart, Domain}, {email_local_part(), email_domain()},
                   list_to_binary(string:join([LocalPart, Domain], "@"))),
              byte_size(Email) < 255).
