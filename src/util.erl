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

-define(SERVER, ?MODULE).

-include("defaults.hrl").

-export([get_env/0, get_env/1, get_env/2]).
-export([start/0, stop/0]).
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
-export([get_epoch/0]).

%% String manipulation
-export([build_string/1]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
%%
%% Environment helper functions
%%

%% @doc Retrieve all key/value pairs in the env for the specified app.
-spec get_env() -> [{Key :: atom(), Value :: term()}].
get_env() ->
    application:get_all_env(?SERVER).

%% @doc The official way to get a value from the app's env.
%%      Will return the 'undefined' atom if that key is unset.
-spec get_env(Key :: atom()) -> term().
get_env(Key) ->
    get_env(Key, undefined).

%% @doc The official way to get a value from this application's env.
%%      Will return Default if that key is unset.
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    case application:get_env(?SERVER, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.
%%
%% Application utility functions
%%

%% @doc Start the application and all its dependencies.
-spec start() -> ok.
start() ->
    start_deps(?SERVER).

-spec start_deps(App :: atom()) -> ok.
start_deps(App) ->
    application:load(App),
    {ok, Deps} = application:get_key(App, applications),
    lists:foreach(fun start_deps/1, Deps),
    start_app(App).

-spec start_app(App :: atom()) -> ok.
start_app(App) ->
    case application:start(App) of
        {error, {already_started, _}} -> ok;
        ok                            -> ok
    end.


%% @doc Stop the application and all its dependencies.
-spec stop() -> ok.
stop() ->
    stop_deps(?SERVER).

-spec stop_deps(App :: atom()) -> ok.
stop_deps(App) ->
    stop_app(App),
    {ok, Deps} = application:get_key(App, applications),
    lists:foreach(fun stop_deps/1, lists:reverse(Deps)).

-spec stop_app(App :: atom()) -> ok.
stop_app(kernel) ->
    ok;
stop_app(stdlib) ->
    ok;
stop_app(App) ->
    case application:stop(App) of
        {error, {not_started, _}} -> ok;
        ok                        -> ok
    end.


%%
%% Conversion
%% 

-spec get_integer(term()) -> integer() | error().
get_integer(Value) when is_integer(Value) -> Value;
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
    get_integer_value(bstr:bstr(Value));
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

%% Time manipulation

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

-spec get_epoch() -> epoch().
get_epoch() ->
datetime_to_epoch(calendar:universal_time()).
