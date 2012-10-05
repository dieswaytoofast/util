-module(json).

-include("defaults.hrl").

-export([validate_boolean/1,
         validate_boolean_list/1,
         validate_integer/1,
         validate_integer_list/1,
         validate_binary/1,
         validate_binary_list/1]).
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

-spec validate_integer_list(any()) -> [integer()] | error().
validate_integer_list(L) when is_list(L) ->
    validate_list_with(fun validate_integer/1, ?INVALID_INTEGER_LIST, L);
validate_integer_list(Other) ->
    {error, {?INVALID_INTEGER_LIST, [Other]}}.

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
