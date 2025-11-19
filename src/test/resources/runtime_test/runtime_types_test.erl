-module(runtime_types_test).
-include_lib("eunit/include/eunit.hrl").

ensure_binary_test_() ->
    [
        ?_assertEqual(<<"hello">>, runtime_types:ensure_binary(<<"hello">>)),
        ?_assertEqual(<<"hello">>, runtime_types:ensure_binary("hello")),
        ?_assertEqual(<<"123">>, runtime_types:ensure_binary(123)),
        ?_assertEqual(<<"123.45">>, runtime_types:ensure_binary(123.45)),
        ?_assertEqual(<<"true">>, runtime_types:ensure_binary(true)),
        ?_assertEqual(<<"false">>, runtime_types:ensure_binary(false)),
        ?_assertEqual(<<"test">>, runtime_types:ensure_binary(test))
    ].

url_encode_test_() ->
    [
        ?_assertEqual(<<"hello%20world">>, runtime_types:url_encode(<<"hello world">>)),
        ?_assertEqual(<<"test%2Fpath">>, runtime_types:url_encode(<<"test/path">>)),
        ?_assertEqual(<<"user%40example.com">>, runtime_types:url_encode(<<"user@example.com">>)),
        ?_assertEqual(<<"100%25">>, runtime_types:url_encode(<<"100%">>))
    ].

timestamp_to_binary_test_() ->
    [
        ?_assertEqual(<<"2023-11-19T10:30:00Z">>, 
                      runtime_types:timestamp_to_binary({{2023, 11, 19}, {10, 30, 0}})),
        ?_assertEqual(<<"2025-01-01T00:00:00Z">>, 
                      runtime_types:timestamp_to_binary({{2025, 1, 1}, {0, 0, 0}}))
    ].

binary_to_timestamp_test_() ->
    [
        ?_assertEqual({{2023, 11, 19}, {10, 30, 0}}, 
                      runtime_types:binary_to_timestamp(<<"2023-11-19T10:30:00Z">>)),
        ?_assertEqual({{2025, 1, 1}, {0, 0, 0}}, 
                      runtime_types:binary_to_timestamp(<<"2025-01-01T00:00:00Z">>))
    ].

roundtrip_timestamp_test() ->
    Original = {{2023, 11, 19}, {10, 30, 0}},
    Binary = runtime_types:timestamp_to_binary(Original),
    ?assertEqual(Original, runtime_types:binary_to_timestamp(Binary)).
