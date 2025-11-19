-module(runtime_http_request_test).
-include_lib("eunit/include/eunit.hrl").

build_uri_test_() ->
    [
        ?_assertEqual(
            <<"https://api.example.com/users/123">>,
            runtime_http_request:build_uri(
                <<"/users/{UserId}">>,
                [{<<"UserId">>, <<"123">>}],
                <<"https://api.example.com">>
            )
        ),
        ?_assertEqual(
            <<"https://s3.amazonaws.com/my-bucket/my%20file.txt">>,
            runtime_http_request:build_uri(
                <<"/{Bucket}/{Key}">>,
                [{<<"Bucket">>, <<"my-bucket">>}, {<<"Key">>, <<"my file.txt">>}],
                <<"https://s3.amazonaws.com">>
            )
        )
    ].

build_query_test_() ->
    [
        ?_assertEqual(
            <<"">>,
            runtime_http_request:build_query([], #{})
        ),
        ?_assertEqual(
            <<"?limit=10">>,
            runtime_http_request:build_query(
                [{<<"Limit">>, <<"limit">>}],
                #{<<"Limit">> => 10}
            )
        ),
        ?_assertEqual(
            <<"?limit=10&page=2">>,
            runtime_http_request:build_query(
                [{<<"Limit">>, <<"limit">>}, {<<"Page">>, <<"page">>}],
                #{<<"Limit">> => 10, <<"Page">> => 2}
            )
        ),
        ?_assertEqual(
            <<"">>,
            runtime_http_request:build_query(
                [{<<"OptionalParam">>, <<"param">>, false}],
                #{}
            )
        )
    ].

build_headers_test_() ->
    [
        ?_assertEqual(
            [],
            runtime_http_request:build_headers([], #{})
        ),
        ?_assertEqual(
            [{<<"X-Custom-Header">>, <<"value">>}],
            runtime_http_request:build_headers(
                [{<<"X-Custom-Header">>, <<"CustomHeader">>}],
                #{<<"CustomHeader">> => <<"value">>}
            )
        ),
        ?_assertEqual(
            [],
            runtime_http_request:build_headers(
                [{<<"X-Optional">>, <<"Optional">>, false}],
                #{}
            )
        )
    ].
