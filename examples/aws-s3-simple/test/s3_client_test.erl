-module(s3_client_test).
-include_lib("eunit/include/eunit.hrl").

%%% Test suite for S3 client with mocked HTTP responses
%%% This tests the HTTP client behavior without making actual network calls

%% Setup and teardown for each test
setup() ->
    %% Start inets application for httpc
    application:ensure_all_started(inets),
    meck:new(httpc, [unstick, passthrough]),
    ok.

teardown(_) ->
    meck:unload(httpc),
    ok.

%% Test fixture for all tests
s3_client_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun test_list_buckets_success/0,
      fun test_list_buckets_http_call/0,
      fun test_get_object_success/0,
      fun test_get_object_http_call/0,
      fun test_get_object_not_found/0,
      fun test_put_object_success/0,
      fun test_put_object_http_call/0,
      fun test_client_creation/0
     ]}.

%% Test: ListBuckets returns successful response
test_list_buckets_success() ->
    %% Mock HTTP response
    MockResponse = #{<<"Buckets">> => [
        #{<<"Name">> => <<"bucket1">>, <<"CreationDate">> => <<"2024-01-01T12:00:00Z">>},
        #{<<"Name">> => <<"bucket2">>, <<"CreationDate">> => <<"2024-01-02T12:00:00Z">>}
    ]},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(get, {_Url, _Headers}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://s3.amazonaws.com">>},
    {ok, Result} = s3_client:list_buckets(Client, #{}),
    
    %% Verify response structure
    ?assert(is_map(Result)),
    Buckets = maps:get(<<"Buckets">>, Result),
    ?assert(is_list(Buckets)),
    ?assertEqual(2, length(Buckets)),
    
    %% Verify httpc was called
    ?assert(meck:called(httpc, request, '_')).

%% Test: ListBuckets makes correct HTTP call
test_list_buckets_http_call() ->
    MockResponse = #{<<"Buckets">> => []},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(Method, {Url, Headers}, _HTTPOptions, _Options) ->
        %% Verify HTTP method
        ?assertEqual(get, Method),
        %% Verify URL contains endpoint and path
        ?assert(string:str(Url, "https://s3.amazonaws.com/") > 0),
        %% Verify headers
        ?assert(is_list(Headers)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://s3.amazonaws.com">>},
    {ok, _Result} = s3_client:list_buckets(Client, #{}),
    
    ?assert(meck:validate(httpc)).

%% Test: GetObject returns successful response
test_get_object_success() ->
    MockResponse = #{
        <<"Body">> => <<"Hello, World!">>,
        <<"ContentType">> => <<"text/plain">>,
        <<"LastModified">> => <<"2024-01-01T12:00:00Z">>
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(get, {_Url, _Headers}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://s3.amazonaws.com">>},
    {ok, Result} = s3_client:get_object(Client, #{
        <<"Bucket">> => <<"my-bucket">>,
        <<"Key">> => <<"test.txt">>
    }),
    
    %% Verify response structure
    ?assert(is_map(Result)),
    ?assertEqual(<<"Hello, World!">>, maps:get(<<"Body">>, Result)),
    ?assertEqual(<<"text/plain">>, maps:get(<<"ContentType">>, Result)).

%% Test: GetObject makes correct HTTP call with URL parameters
test_get_object_http_call() ->
    MockResponse = #{<<"Body">> => <<>>},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(Method, {Url, Headers}, _HTTPOptions, _Options) ->
        %% Verify HTTP method
        ?assertEqual(get, Method),
        %% Verify URL structure (should contain bucket and key placeholders)
        ?assert(string:str(Url, "https://s3.amazonaws.com/") > 0),
        %% In a real implementation, {Bucket}/{Key} would be replaced with actual values
        ?assert(is_list(Headers)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://s3.amazonaws.com">>},
    {ok, _Result} = s3_client:get_object(Client, #{
        <<"Bucket">> => <<"my-bucket">>,
        <<"Key">> => <<"test.txt">>
    }),
    
    ?assert(meck:validate(httpc)).

%% Test: GetObject handles 404 error
test_get_object_not_found() ->
    ErrorResponse = #{<<"message">> => <<"The specified key does not exist">>},
    ResponseBody = jsx:encode(ErrorResponse),
    
    meck:expect(httpc, request, fun(get, {_Url, _Headers}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 404, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://s3.amazonaws.com">>},
    {error, {StatusCode, ErrorData}} = s3_client:get_object(Client, #{
        <<"Bucket">> => <<"my-bucket">>,
        <<"Key">> => <<"missing.txt">>
    }),
    
    %% Verify error response
    ?assertEqual(404, StatusCode),
    ?assert(is_map(ErrorData)),
    ?assertEqual(<<"The specified key does not exist">>, maps:get(<<"message">>, ErrorData)).

%% Test: PutObject returns successful response
test_put_object_success() ->
    MockResponse = #{<<"ETag">> => <<"\"d41d8cd98f00b204e9800998ecf8427e\"">>},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(put, {_Url, _Headers, _ContentType, _Body}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://s3.amazonaws.com">>},
    {ok, Result} = s3_client:put_object(Client, #{
        <<"Bucket">> => <<"my-bucket">>,
        <<"Key">> => <<"test.txt">>,
        <<"Body">> => <<"Hello, World!">>,
        <<"ContentType">> => <<"text/plain">>
    }),
    
    %% Verify response structure
    ?assert(is_map(Result)),
    ?assertEqual(<<"\"d41d8cd98f00b204e9800998ecf8427e\"">>, maps:get(<<"ETag">>, Result)).

%% Test: PutObject makes correct HTTP call with body
test_put_object_http_call() ->
    MockResponse = #{<<"ETag">> => <<"\"abc123\"">>},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(Method, {Url, Headers, ContentType, Body}, _HTTPOptions, _Options) ->
        %% Verify HTTP method
        ?assertEqual(put, Method),
        %% Verify URL
        ?assert(string:str(Url, "https://s3.amazonaws.com/") > 0),
        %% Verify content type
        ?assertEqual("application/json", ContentType),
        %% Verify body is present
        ?assert(is_list(Body)),
        ?assert(length(Body) > 0),
        %% Verify headers
        ?assert(is_list(Headers)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://s3.amazonaws.com">>},
    {ok, _Result} = s3_client:put_object(Client, #{
        <<"Bucket">> => <<"my-bucket">>,
        <<"Key">> => <<"test.txt">>,
        <<"Body">> => <<"Hello, World!">>,
        <<"ContentType">> => <<"text/plain">>
    }),
    
    ?assert(meck:validate(httpc)).

%% Test: Client creation works correctly
test_client_creation() ->
    Config = #{endpoint => <<"https://s3.amazonaws.com">>},
    {ok, Client} = s3_client:new(Config),
    
    ?assert(is_map(Client)),
    ?assertEqual(<<"https://s3.amazonaws.com">>, maps:get(endpoint, Client)).
