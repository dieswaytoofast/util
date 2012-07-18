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
-export([twitter_time_to_epoch/1, twitter_time_to_datetime/1]).

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

%% @doc Convert a date and time as binary string in the ISO 8601 format to the
%%      number of seconds since the Unix epoch (Jan 1, 1970, 00:00:00) with
%%      millisecond precision.
-spec twitter_time_to_epoch(binary()) -> epoch().
twitter_time_to_epoch(TwitterDatetime) when is_binary(TwitterDatetime) ->
    datetime_to_epoch(twitter_time_to_datetime(TwitterDatetime));
twitter_time_to_epoch(_TwitterDatetime) ->
    null.

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


%% @doc Convert a datetime in the format used by Twitter to a date and time in the
%%      format returned by calendar:universal_time/0.
-spec twitter_time_to_datetime(binary()) -> datetime().
twitter_time_to_datetime(<<_DDD:3/binary, " ", MMM:3/binary, " ", DD:2/binary, " ",
                      Hh:2/binary, $:, Mm:2/binary, $:, Ss:2/binary, " ",
                      Sign, TzHour:2/binary, TzMin:2/binary, " ", YYYY:4/binary, _Tail/binary>>) ->
    Month = month(MMM),
    Date1 = {bstr:to_integer(YYYY), Month, bstr:to_integer(DD)},
    Hour1 = bstr:to_integer(Hh),
    Min1 = bstr:to_integer(Mm),
    Sec1 = bstr:to_integer(Ss),

    if
        TzHour =:= <<"00">> andalso TzMin =:= <<"00">> ->
            {Date1, {Hour1, Min1, Sec1}};
        true ->
            LocalSec = calendar:datetime_to_gregorian_seconds({Date1, {Hour1, Min1, Sec1}}),
            %% Convert the the seconds in the local timezone to UTC.
            UtcSec = case ((bstr:to_integer(TzHour) * 3600 + bstr:to_integer(TzMin)) * 60) of
                         Offset when Sign =:= $- -> LocalSec - Offset;
                         Offset                  -> LocalSec + Offset
                     end,
            calendar:gregorian_seconds_to_datetime(UtcSec)
    end;
twitter_time_to_datetime(_TwitterDatetime) ->
    null.


-spec month(binary()) -> integer().
month(<<"Jan">>) ->  1;
month(<<"Feb">>) ->  2;
month(<<"Mar">>) ->  3;
month(<<"Apr">>) ->  4;
month(<<"Jun">>) ->  6;
month(<<"Jul">>) ->  7;
month(<<"Aug">>) ->  8;
month(<<"Sep">>) ->  9;
month(<<"Oct">>) -> 10;
month(<<"Nov">>) -> 11;
month(<<"Dec">>) -> 12.

