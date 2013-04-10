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
         validate_integer_range/2,
         validate_binary/1,
         validate_binary_list/1,
         validate_undefined/1,
         validate_null/1,
         validate_ignore/1,
         validate_integer_in_list/2,
         validate_atom/2,
         validate_atom_from_list/2]).
-export([diff/2]).

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

-spec validate_integer_range({integer(), integer()}, any()) -> integer() | error().
validate_integer_range({Lower, Higher}, I) ->
    case validate_integer(I) of
        Int when is_integer(I) andalso
                Int >= Lower andalso
                Int =< Higher -> Int;
        Int when is_integer(Int) -> {error, ?INVALID_INTEGER};
        Error -> Error
    end.

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

-spec validate_integer_in_list([integer()], any()) -> integer() | error().
validate_integer_in_list(Accept, B) when is_binary(B) ->
    try
        I = list_to_integer(binary_to_list(B)),
        validate_integer_in_list(Accept, I)
    catch _:_ ->
            {error, {?INVALID_INTEGER, [B]}}
    end;
validate_integer_in_list(Accept, L) when is_list(L) ->
    try
        I = list_to_integer(L),
        validate_integer_in_list(Accept, I)
    catch _:_ ->
            {error, {?INVALID_INTEGER, [L]}}
    end;
validate_integer_in_list(Accept, A) when is_atom(A) ->
    try
        I = list_to_integer(atom_to_list(A)),
        validate_integer_in_list(Accept, I)
    catch _:_ ->
            {error, {?INVALID_INTEGER, [A]}}
    end;
validate_integer_in_list(Accept, I) when is_integer(I) ->
    case lists:member(I, Accept) of
        true ->
            I;
        false ->
            {error, {?INVALID_INTEGER, [I]}}
    end;
validate_integer_in_list(_, I) ->
    {error, {?INVALID_INTEGER, [I]}}.

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


%% @doc Check two parsed JSON documents in the format used by the jsx JSON
%%      parser and return the first term where they differ.
-spec diff(jsx:json_term(), jsx:json_term()) -> match | {nomatch, Path :: list(), Left :: jsx:json_term(), Right :: jsx:json_term()}.
diff(JsonTerm1, JsonTerm2) when is_list(JsonTerm1), is_list(JsonTerm2) ->
    diff([], lists:sort(JsonTerm1), lists:sort(JsonTerm2)).

diff(Path, [{Name, Value} | Tail1], [{Name, Value} | Tail2]) when not is_list(Value) ->
    %% Both elements have scalar values that match, so check the next element
    diff(Path, Tail1, Tail2);
diff(Path, [{Name, List1 = [Head | _]} | Tail1], [{Name, List2} | Tail2]) when is_tuple(Head) ->
    %% The field name matches and it holds a subdocument, so check it
    case diff([Name | Path], lists:sort(List1), lists:sort(List2)) of
        match ->
            %% If it matches, continue with the rest of the elements
            diff(Path, Tail1, Tail2);
        {nomatch, _DiffPath, _A, _B} = Result ->
            Result
    end;
diff(Path, [{Name, Array1 = [_ | _]} | Tail1], [{Name, Array2} | Tail2]) ->
    %% The field name matches and it holds an array, so check its elements
    case array_diff([Name | Path], 1, Array1, Array2) of
        match ->
            %% If it matches, continue with the rest of the elements
            diff(Path, Tail1, Tail2);
        {nomatch, _DiffPath, _A, _B} = Result ->
            Result
    end;
diff(Path, [{_Name1, _Value1} = Tuple1 | _Tail1], [{_Name2, _Value2} = Tuple2 | _Tail2]) ->
    %% The field names don't match
    {nomatch, lists:reverse(Path), Tuple1, Tuple2};
diff(Path, [{Name1, Value1} | _], []) ->
    %% The left document is longer than the right one
    {nomatch, lists:reverse([Name1 | Path]), Value1, null};
diff(Path, [], [{Name2, Value2} | _]) ->
    %% The left document is shorter than the right one
    {nomatch, lists:reverse([Name2 | Path]), null, Value2};
diff(_Path, [], []) ->
    match.


array_diff(Path, Pos, [Value1 | Tail1], [Value2 | Tail2]) when is_list(Value1); is_list(Value2) ->
    %% We're dealing with subdocuments; recurse into them
    case diff([Pos | Path], lists:sort(Value1), lists:sort(Value2)) of
        match ->
            %% Both elements match; check the next ones
            array_diff(Path, Pos + 1, Tail1, Tail2);
        {nomatch, _DiffPath, _A, _B} = Result ->
            Result
    end;
array_diff(Path, Pos, [Value | Tail1], [Value | Tail2]) ->
    %% Both elements match; check the next ones
    array_diff(Path, Pos + 1, Tail1, Tail2);
array_diff(Path, Pos, [Value1 | _Tail1], [Value2 | _Tail2]) ->
    %% The elements don't match
    {nomatch, lists:reverse([Pos | Path]), Value1, Value2};
array_diff(Path, Pos, [Value1 | _], []) ->
    %% The left array is longer than the right one
    {nomatch, lists:reverse([Pos | Path]), Value1, null};
array_diff(Path, Pos, [], [Value2 | _]) ->
    %% The left array is shorter than the right one
    {nomatch, lists:reverse([Pos | Path]), null, Value2};
array_diff(_Path, _Pos, [], []) ->
    match.
