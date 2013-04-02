-module(json).

-include("defaults.hrl").

-export([validate_boolean/1,
         validate_boolean_list/1,
         validate_float/1,
         validate_float_list/1,
         validate_integer/1,
         validate_non_neg_integer/1,
         validate_integer_list/1,
         validate_non_neg_integer_list/1,
         validate_binary/1,
         validate_binary_list/1,
         validate_undefined/1,
         validate_null/1,
         validate_ignore/1,
         validate_atom/2,
         validate_atom_from_list/2]).
%% Validation

-spec validate_boolean(any()) -> boolean() | error().
validate_boolean(Bool) when is_boolean(Bool) ->
    Bool;
validate_boolean(Bin) when is_binary(Bin) ->
    case bstr:lower(Bin) of
        <<"true">> ->
            true;
        <<"false">> ->
            false;
        _ ->
            {error, {?INVALID_BOOLEAN, [Bin]}}
    end;
validate_boolean(Other) ->
    {error, {?INVALID_BOOLEAN, [Other]}}.

-spec validate_float(any()) -> float() | error().
validate_float(Float) when is_float(Float) ->
    Float;
validate_float(Bin) when is_binary(Bin) ->
    try
        bstr:to_float(Bin)
    catch
        error:badarg ->
            {error, {?INVALID_INTEGER, [Bin]}}
    end;
validate_float(Other) ->
    {error, {?INVALID_FLOAT, [Other]}}.


-spec validate_integer(any()) -> integer() | error().
validate_integer(Int) when is_integer(Int) ->
    Int;
validate_integer(Bin) when is_binary(Bin) ->
    try
        bstr:to_integer(Bin)
    catch
        error:badarg ->
            {error, {?INVALID_INTEGER, [Bin]}}
    end;
validate_integer(Other) ->
    {error, {?INVALID_INTEGER, [Other]}}.

-spec validate_non_neg_integer(any()) -> non_neg_integer() | error().
validate_non_neg_integer(Int) ->
    case validate_integer(Int) of
        {error, _} = Error ->
            Error;
        IntVal ->
            case IntVal >= 0 of
                true -> IntVal;
                false -> {error, {?INVALID_NON_NEG_INTEGER, [Int]}}
            end
    end.
    
-spec validate_binary(any()) -> binary() | error().
validate_binary(Bin) when is_binary(Bin) ->
    Bin;
validate_binary(Other) ->
    {error, {?INVALID_BINARY, [Other]}}.

-spec validate_boolean_list(any()) -> [boolean()] | error().
validate_boolean_list(L) when is_list(L) ->
    validate_list_with(fun validate_boolean/1, ?INVALID_BOOLEAN_LIST, L);
validate_boolean_list(Other) ->
    {error, {?INVALID_BOOLEAN_LIST, [Other]}}.

-spec validate_float_list(any()) -> [float()] | error().
validate_float_list(L) when is_list(L) ->
    validate_list_with(fun validate_float_list/1, ?INVALID_FLOAT_LIST, L);
validate_float_list(Other) ->
    {error, {?INVALID_FLOAT_LIST, [Other]}}.


-spec validate_integer_list(any()) -> [integer()] | error().
validate_integer_list(L) when is_list(L) ->
    validate_list_with(fun validate_integer/1, ?INVALID_INTEGER_LIST, L);
validate_integer_list(Other) ->
    {error, {?INVALID_INTEGER_LIST, [Other]}}.

-spec validate_non_neg_integer_list(any()) -> [integer()] | error().
validate_non_neg_integer_list(L) when is_list(L) ->
    validate_list_with(fun validate_non_neg_integer/1, ?INVALID_NON_NEG_INTEGER_LIST, L);
validate_non_neg_integer_list(Other) ->
    {error, {?INVALID_NON_NEG_INTEGER_LIST, [Other]}}.

-spec validate_binary_list(any()) -> [binary()] | error().
validate_binary_list(L) when is_list(L) ->
    validate_list_with(fun validate_binary/1, ?INVALID_BINARY_LIST, L);
validate_binary_list(Other) ->
    {error, {?INVALID_BINARY_LIST, [Other]}}.

-spec validate_list_with(fun((any()) -> boolean()), binary(), [any()]) -> [any()] | error().
validate_list_with(Fun, ErrorType, L) ->
    Res = lists:foldl(fun(X, {Vs, Es}) ->
                        case Fun(X) of
                            {error, E} ->
                                {Vs, [E|Es]};
                            G ->
                                {[G|Vs], Es}
                        end
                end,
                {[], []},
                L),
    case Res of
        {Valid, []} ->
            lists:reverse(Valid);
        _ ->
            {error, {ErrorType, [L]}}
    end.

-spec validate_undefined(any()) -> any().
validate_undefined(Value) ->
    Value.

-spec validate_null(any()) -> undefined | error().
validate_null(null) ->
    undefined;
validate_null(Other) ->
    {error, {?INVALID_NULL, [Other]}}.

-spec validate_ignore(any()) -> undefined.
validate_ignore(_Any) ->
    undefined.

-spec validate_atom([atom()], any()) -> atom() | error().
validate_atom(Accept, B) when is_binary(B) ->
    L = binary_to_list(B),
    validate_atom(Accept, L);
validate_atom(Accept, L) when is_list(L) ->
    try
        A = list_to_existing_atom(L),
        validate_atom(Accept, A)
    catch _:_ ->
            {error, {?INVALID_ATOM, [L]}}
    end;
validate_atom(Accept, A) when is_atom(A) ->
    case lists:member(A, Accept) of
        true ->
            A;
        false ->
            {error, {?INVALID_ATOM, [A]}}
    end;
validate_atom(_, A) ->
    {error, {?INVALID_ATOM, [A]}}.

-spec validate_atom_from_list([atom()], [any()]) -> [atom()] | error().
validate_atom_from_list(Accept, L) when is_list(Accept), is_list(L) ->
    validate_list_with(fun(X) -> validate_atom(Accept, X) end,
                       ?INVALID_ATOM_LIST, L);
validate_atom_from_list(_, L) ->
    {error, {?INVALID_ATOM_LIST, [L]}}.

