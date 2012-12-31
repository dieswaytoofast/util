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
%% We're not checking for RFC compliance
%% http://www.w3.org/TR/html5/states-of-the-type-attribute.html#valid-e-mail-address
-define(EMAIL_ADDRESS_REGEXP, "^[a-zA-Z0-9.\\\!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$").

-include("defaults.hrl").

-export([get_env/0, get_env/1, get_env/2]).
-export([start/0, stop/0]).
%% Conversions
-export([get_string/1]).
-export([get_binary/1]).
-export([get_integer/1]).
-export([get_boolean/1]).
-export([get_base62/1]).
-export([get_base36/1]).

%% Validations
-export([required/2]).
-export([validate_list_of_binaries/1]).
-export([validate_list_with/2]).
-export([validate_email_address/1]).
-export([validate_url/1]).
-export([validate_tcp_port/1]).
-export([validate_utf8/1]).
-export([validate_area_code/1]).

%% Time specific stuff
-export([datetime_to_epoch/1]).
-export([get_epoch/0]).

%% String manipulation
-export([build_string/1]).

%% List functions
-export([get_value/3, get_value/4]).

%% UUID stuff
-export([create_uuid/0]).


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

-spec get_integer(binary() | atom() | list() | char()) -> integer() | error().
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

-spec get_string(term()) -> string() | error().
get_string(Data) when is_integer(Data) -> integer_to_list(Data);
get_string(Data) when is_atom(Data) -> atom_to_list(Data);
get_string(Data) when is_binary(Data) -> binary_to_list(Data);
get_string(Data) when is_list(Data) ->
    case io_lib:printable_unicode_list(Data) of
        true ->
            Data;
        false ->
            {error, ?INVALID_STRING}
    end;
get_string(_) -> {error, ?INVALID_STRING}.

-spec get_binary(term()) -> binary() | error().
get_binary(Data) when is_integer(Data) -> get_binary(integer_to_list(Data));
get_binary(Data) when is_atom(Data) -> get_binary(atom_to_list(Data));
get_binary(Data) when is_list(Data) -> list_to_binary(Data);
get_binary(Data) when is_binary(Data) -> Data;
get_binary(_) -> {error, ?INVALID_BINARY}.

%% @doc Gets the boolean value of the provided parameter
-spec get_boolean(Value::term()) -> boolean() | error().
get_boolean(Value) when is_binary(Value) ->
    get_boolean_lower_value(bstr:lower(Value));
get_boolean(Value) ->
    {error, {?INVALID_BOOLEAN, [Value]}}.

get_boolean_lower_value(<<"true">>) -> true;
get_boolean_lower_value(<<"false">>) -> false;
get_boolean_lower_value(Value) ->
    {error, {?INVALID_BOOLEAN, [Value]}}.

-spec get_base62(integer()) -> string().
get_base62(Number) -> get_base62(Number, []).
get_base62(Number, []) when Number =:= 0 -> "0";
get_base62(Number, Acc) when Number =:= 0 -> Acc;
get_base62(Number, Acc) when Number < 0 -> get_base62(-Number, Acc);
get_base62(Number, Acc) ->
	NumberDiv = Number div 62,
    NumberRem = Number rem 62,
	Acc1 = [get_base62_letter(NumberRem) | Acc],
	get_base62(NumberDiv, Acc1).

get_base62_letter(X) when X =< 9 -> $0 + X;
get_base62_letter(X) when X =< 35 -> $A + X - 10;
get_base62_letter(X) -> $a + X - 36.

-spec get_base36(integer()) -> string().
get_base36(Number) -> get_base36(Number, []).
get_base36(Number, []) when Number =:= 0 -> "0";
get_base36(Number, Acc) when Number =:= 0 -> Acc;
get_base36(Number, Acc) when Number < 0 -> get_base36(-Number, Acc); 
get_base36(Number, Acc) ->
	NumberDiv = Number div 36,
    NumberRem = Number rem 36,
	Acc1 = [get_base36_letter(NumberRem) | Acc],
	get_base36(NumberDiv, Acc1).

get_base36_letter(X) when X =< 9 -> $0 + X;
get_base36_letter(X) -> $A + X - 10.

%%
%% Lists
%% 

%% @doc: Search the key in the list of tuples and returns the value if it exists or throws an exception if it doesn't.
-spec get_value(Key::term(), N::integer(), TupleList::[tuple()]) -> term().
get_value(_Key, _N, []) ->
    throw({empty_list, []});
get_value(Key, N, TupleList) ->
    case lists:keyfind( Key, N, TupleList ) of
        false ->
            throw({invalid_key, Key});
        Tuple ->
            element(N, Tuple)
    end.

%% @doc: Search the key in the list of tuples and returns the value if it exists or the default value if it doesn't.
-spec get_value(Key::term(), N::integer(), Default::term(), TupleList::[tuple()]) -> term().
get_value(_Key, _N, Default, []) ->
    Default;
get_value(Key, N, Default, TupleList) ->
    case lists:keyfind( Key, N, TupleList ) of
        false ->
            Default;
        Tuple ->
            element(N, Tuple)
    end.


%%
%% Validations
%%
%% @doc Check if Value is an 'empty' parameter
-spec required(Field::term(), Value::term()) -> ok | error().
required(Field, Value) ->
    if Value =:= []
         orelse Value =:= <<>>
         orelse Value =:= undefined ->
           {error, {?EMPTY_ERROR, [Field]}};
        true ->
            ok
    end.


-spec validate_list_of_binaries([term()]) -> ok | error().
validate_list_of_binaries(L) ->
    validate_list_of_binaries(L, []).

%% @doc Validate that this is a list of binaries
-spec validate_list_of_binaries([term()], [term()]) -> ok | error().
validate_list_of_binaries([H|T], ReturnVal) ->
    if is_binary(H) =:= true ->
            validate_list_of_binaries(T, ReturnVal);
        true ->
            {error, {ReturnVal, [H]}}
    end;
validate_list_of_binaries([], _ReturnVal) ->
    ok.

-spec validate_list_with({module(), atom()}, list()) -> list() | error().
validate_list_with({_M, _F}=Fun, L) ->
    validate_list_with_1(Fun, L, L, []).

validate_list_with_1(_, _, [], Acc) ->
    lists:reverse(Acc);
validate_list_with_1({M, F}=Fun, L, [H|T], Acc) ->
    case M:F(H) of
        {error, _} ->
            {error, {?INVALID_LIST, [L]}};
        H2 ->
            validate_list_with_1(Fun, L, T, [H2|Acc])
    end.

-spec validate_email_address(term()) -> binary() | error().
validate_email_address(Address) ->
    try
        case re:run(Address, ?EMAIL_ADDRESS_REGEXP, [{capture, all, binary}]) of
            nomatch ->
                {error, {?INVALID_EMAIL_ADDRESS, [Address]}};
            {match, [B]} ->
                B
        end
    catch _:_ ->
            {error, {?INVALID_EMAIL_ADDRESS, [Address]}}
    end.

-spec validate_url(binary()) -> binary() | error().
validate_url(Url) when is_binary(Url) ->
    L = binary_to_list(Url),
    case ibrowse_lib:parse_url(L) of
        {error, _} ->
            {error, {?INVALID_URL, [Url]}};
        _ ->
            Url
    end.

-spec validate_tcp_port(term()) -> 1 .. 65535 | error().
validate_tcp_port(Port) when is_integer(Port) andalso Port > 0 andalso Port =< 65535 ->
    Port;
validate_tcp_port(Port) ->
    {error, {?INVALID_TCP_PORT, [Port]}}.

-spec validate_utf8(term()) -> binary() | error().
validate_utf8(Bin) ->
    try
        Bin2 = bstr:bstr(Bin),
        case unicode:characters_to_binary(Bin2, utf8, utf8) of
            Bin2 ->
                Bin2;
            _ ->
                {error, {?INVALID_UTF8, [Bin]}}
        end
    catch _:_ ->
            {error, {?INVALID_UTF8, [Bin]}}
    end.

-spec validate_area_code(binary()) -> binary() | error().
validate_area_code(AreaCode) when is_binary(AreaCode) ->
    case bstr:is_digit(AreaCode) of
        true ->
            AreaCode;
        false ->
            {error, {?INVALID_AREA_CODE, [AreaCode]}}
    end.


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


%% UUID
%% @doc  Generate a v4 UUID as a binary (rfc4122)
-spec create_uuid() -> binary().
create_uuid() ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    create_uuid(random:uniform(1 bsl 48) - 1, random:uniform(1 bsl 12) - 1,
        random:uniform(1 bsl 32) - 1, random:uniform(1 bsl 30) - 1).
create_uuid(R1, R2, R3, R4) ->
    Uuid = <<R1:48, 4:4, R2:12, 2:2, R3:32, R4:30>>,
    list_to_binary(uuid_to_string(Uuid)).

%% @doc  Convert a UUID binary to a hex encoded string.
-spec uuid_to_string(Uuid::binary()) -> string().
uuid_to_string(<<TL:4/binary, TM:2/binary, THV:2/binary, CS:2/binary, N:6/binary>>) ->
    uuid_to_string([TL, TM, THV, CS, N], []).

uuid_to_string([Head | [_ | _] = Tail], Acc) ->
    uuid_to_string(Tail, [$- | encode_in_hex(Head, Acc)]);
uuid_to_string([Head | Tail], Acc) ->
    uuid_to_string(Tail, encode_in_hex(Head, Acc));
uuid_to_string([], Acc) ->
    lists:reverse(Acc).

encode_in_hex(<<Hi:4, Lo:4, Tail/binary>>, Acc) ->
    encode_in_hex(Tail, [char:integer_to_hex(Lo, lower), char:integer_to_hex(Hi, lower) | Acc]);
encode_in_hex(<<>>, Acc) ->
    Acc.


