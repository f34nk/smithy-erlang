-module(runtime_types).

-export([ensure_binary/1,
         url_encode/1,
         timestamp_to_binary/1,
         binary_to_timestamp/1]).

%% Convert any value to binary
-spec ensure_binary(term()) -> binary().
ensure_binary(Value) when is_binary(Value) ->
    Value;
ensure_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
ensure_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
ensure_binary(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 10}, compact]);
ensure_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
ensure_binary(true) ->
    <<"true">>;
ensure_binary(false) ->
    <<"false">>;
ensure_binary(Value) ->
    %% Fallback for complex types
    iolist_to_binary(io_lib:format("~p", [Value])).

%% URL encode a value (RFC 3986 compliant)
-spec url_encode(binary()) -> binary().
url_encode(Binary) when is_binary(Binary) ->
    url_encode(binary_to_list(Binary));
url_encode(String) when is_list(String) ->
    list_to_binary(uri_string:quote(String)).

%% Convert Erlang timestamp to ISO 8601 binary
-spec timestamp_to_binary(calendar:datetime()) -> binary().
timestamp_to_binary({{Y, M, D}, {H, Min, S}}) ->
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                      [Y, M, D, H, Min, S])
    ).

%% Parse ISO 8601 binary to Erlang timestamp
-spec binary_to_timestamp(binary()) -> calendar:datetime().
binary_to_timestamp(Binary) ->
    String = binary_to_list(Binary),
    [Date, Time] = string:split(String, "T"),
    [Y, M, D] = [list_to_integer(X) || X <- string:split(Date, "-", all)],
    [TimeStr | _] = string:split(Time, "Z"),
    [H, Min, S] = [list_to_integer(X) || X <- string:split(TimeStr, ":", all)],
    {{Y, M, D}, {H, Min, S}}.
