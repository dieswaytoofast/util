%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Module serving twitterl_util functions
%%% @end
%%%-------------------------------------------------------------------
-module(util).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-include("defaults.hrl").

%% Conversions
-export([get_string/1]).
-export([get_binary/1]).
-export([get_integer/1]).
-export([get_boolean/1]).
-export([get_base62/1]).

%% Validations
-export([required/2]).
-export([validate_list_of_binaries/2]).


%% Time specific stuff
-export([datetime_to_epoch/1]).

%% String manipulation
-export([build_string/1]).

%%
%% Conversion
%% 

-spec get_integer(term()) -> integer() | error().
get_integer(Value) ->
    case bstr:is_numeric(bstr:bstr(Value)) of
        true ->
            get_integer_value(Value);
        false ->
            {error, {?INVALID_INTEGER, [Value]}}
    end.

-spec get_integer_value(term()) -> integer().
get_integer_value(Value) when is_binary(Value) ->
    get_integer_value(binary_to_list(Value));
get_integer_value(Value) when is_atom(Value) ->
    get_integer_value(atom_to_list(Value));
get_integer_value(Value) when is_list(Value) ->
    list_to_integer(Value);
get_integer_value(Value) when is_integer(Value) ->
    Value.


-spec get_string(atom() | binary() | string()) -> string().
get_string(Data) when is_integer(Data) -> integer_to_list(Data);
get_string(Data) when is_atom(Data) -> atom_to_list(Data);
get_string(Data) when is_binary(Data) -> binary_to_list(Data);
get_string(Data) when is_list(Data) -> Data;
get_string(_) -> {error, ?INVALID_STRING}.

-spec get_binary(atom() | binary() | string()) -> binary().
get_binary(Data) when is_integer(Data) -> integer_to_list(Data);
get_binary(Data) when is_atom(Data) -> atom_to_list(Data);
get_binary(Data) when is_list(Data) -> list_to_binary(Data);
get_binary(Data) when is_binary(Data) -> Data;
get_binary(_) -> {error, ?INVALID_BINARY}.

%% @doc Gets the boolean value of the provided parameter
-spec get_boolean(Value::term()) -> boolean() | error().
get_boolean(Value) when is_binary(Value) -> 
    get_boolean_lower_value(bstr:lower(Value));
get_boolean(Value) -> 
    {error, ?INVALID_BOOLEAN, [Value]}.

get_boolean_lower_value(<<"true">>) -> true;
get_boolean_lower_value(<<"false">>) -> false;
get_boolean_lower_value(Value) -> 
    {error, ?INVALID_BOOLEAN, [Value]}.

get_base62(Number) -> get_base62(Number, []).
get_base62(Number, []) when Number =:= 0 -> "0";
get_base62(Number, Acc) when Number =:= 0 -> Acc;
get_base62(Number, Acc) when Number < 0 -> get_base62(-Number, Acc); 
get_base62(Number, Acc) ->
	NumberDiv = Number div 62,
    NumberRem = Number rem 62,
	Acc1 = [get_letter(NumberRem) | Acc],
	get_base62(NumberDiv, Acc1).

get_letter(X) when X =< 9 -> $0 + X;
get_letter(X) when X =< 35 -> $A + X - 10;
get_letter(X) -> $a + X - 36.


%%
%% String Manipulation
%% 
build_string(Params) when is_list(Params) ->
    Result = 
    lists:foldr(fun(X, Acc) ->
                    SX = get_string(X),
                    [SX|Acc]
            end, [], Params),
    lists:flatten(Result).

%%
%% Validations
%% 
%% @doc Check if Value is an 'empty' parameter
-spec required(Field::term(), Value::term()) -> ok | error().
required(Field, Value) ->
    if Value =:= []
         orelse Value =:= <<>>
         orelse Value =:= undefined ->
           {error, ?EMPTY_ERROR, [Field]};
        true ->
            ok
    end.

%% @doc Validate that this is a list of binaries
-spec validate_list_of_binaries(Value::any(), ReturnVal::any()) -> ok | error().
validate_list_of_binaries([H|T], ReturnVal) ->
    if is_binary(H) =:= true ->
            validate_list_of_binaries(T, ReturnVal);
        true ->
            {error, ReturnVal, [H]}
    end;
validate_list_of_binaries([], _ReturnVal) ->
    ok.

%% @doc Convert a datetime in the format returned by the calendar:universal_time/0 function
%%      into a timestamp as a floating-point with the number of seconds since
%%      the Unix Epoch (Jan 1, 1970, 00:00:00) and a precision of microseconds.
-spec datetime_to_epoch(datetime()) -> epoch().
datetime_to_epoch({{_Year, _Month, _Day}, {_Hour, _Min, Sec}} = Datetime) when is_integer(Sec) ->
    calendar:datetime_to_gregorian_seconds(Datetime) - ?SECONDS_TO_UNIX_EPOCH;
datetime_to_epoch({{_Year, _Month, _Day} = Date, {Hour, Min, Sec}}) when is_float(Sec) ->
    TruncatedSec = trunc(Sec),
    Subsec = round((Sec - TruncatedSec) * 1000000.0) / 1000000.0,
    float(calendar:datetime_to_gregorian_seconds({Date, {Hour, Min, TruncatedSec}}) - ?SECONDS_TO_UNIX_EPOCH) + Subsec.

