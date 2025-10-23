-module(sts_client_test).
-include_lib("eunit/include/eunit.hrl").

%%% Test suite for STS client with mocked HTTP responses
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
sts_client_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun test_get_caller_identity_success/0,
      fun test_get_caller_identity_http_call/0,
      fun test_assume_role_success/0,
      fun test_assume_role_http_call/0,
      fun test_assume_role_with_policy_arns/0,
      fun test_assume_role_malformed_policy_error/0,
      fun test_assume_role_packed_policy_too_large_error/0,
      fun test_get_session_token_success/0,
      fun test_get_session_token_http_call/0,
      fun test_get_session_token_with_mfa/0,
      fun test_client_creation/0
     ]}.

%% Test: GetCallerIdentity returns successful response
test_get_caller_identity_success() ->
    %% Mock HTTP response
    MockResponse = #{
        <<"UserId">> => <<"AIDAI23HXX2LCI6BEXAMPLE">>,
        <<"Account">> => <<"123456789012">>,
        <<"Arn">> => <<"arn:aws:iam::123456789012:user/Alice">>
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(get, {_Url, _Headers}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://sts.amazonaws.com">>},
    {ok, Result} = sts_client:get_caller_identity(Client, #{}),
    
    %% Verify response structure
    ?assert(is_map(Result)),
    ?assertEqual(<<"AIDAI23HXX2LCI6BEXAMPLE">>, maps:get(<<"UserId">>, Result)),
    ?assertEqual(<<"123456789012">>, maps:get(<<"Account">>, Result)),
    ?assertEqual(<<"arn:aws:iam::123456789012:user/Alice">>, maps:get(<<"Arn">>, Result)),
    
    %% Verify httpc was called
    ?assert(meck:called(httpc, request, '_')).

%% Test: GetCallerIdentity makes correct HTTP call
test_get_caller_identity_http_call() ->
    MockResponse = #{<<"UserId">> => <<"test">>},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(Method, {Url, Headers}, _HTTPOptions, _Options) ->
        %% Verify HTTP method
        ?assertEqual(get, Method),
        %% Verify URL contains endpoint and path
        ?assert(string:str(Url, "https://sts.amazonaws.com/caller-identity") > 0),
        %% Verify headers
        ?assert(is_list(Headers)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://sts.amazonaws.com">>},
    {ok, _Result} = sts_client:get_caller_identity(Client, #{}),
    
    ?assert(meck:validate(httpc)).

%% Test: AssumeRole returns successful response
test_assume_role_success() ->
    %% Mock HTTP response
    MockResponse = #{
        <<"Credentials">> => #{
            <<"AccessKeyId">> => <<"AKIAIOSFODNN7EXAMPLE">>,
            <<"SecretAccessKey">> => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
            <<"SessionToken">> => <<"AQoDYXdzEJr...">>,
            <<"Expiration">> => <<"2024-01-01T12:00:00Z">>
        },
        <<"AssumedRoleUser">> => #{
            <<"AssumedRoleId">> => <<"AROA3XFRBF535PLBIFPI4:session-name">>,
            <<"Arn">> => <<"arn:aws:sts::123456789012:assumed-role/demo/session-name">>
        }
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(post, {_Url, _Headers, _ContentType, _Body}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://sts.amazonaws.com">>},
    {ok, Result} = sts_client:assume_role(Client, #{
        <<"RoleArn">> => <<"arn:aws:iam::123456789012:role/demo">>,
        <<"RoleSessionName">> => <<"session-name">>
    }),
    
    %% Verify response structure
    ?assert(is_map(Result)),
    Credentials = maps:get(<<"Credentials">>, Result),
    ?assert(is_map(Credentials)),
    ?assertEqual(<<"AKIAIOSFODNN7EXAMPLE">>, maps:get(<<"AccessKeyId">>, Credentials)),
    ?assertEqual(<<"AQoDYXdzEJr...">>, maps:get(<<"SessionToken">>, Credentials)),
    
    AssumedRoleUser = maps:get(<<"AssumedRoleUser">>, Result),
    ?assert(is_map(AssumedRoleUser)),
    ?assertEqual(<<"AROA3XFRBF535PLBIFPI4:session-name">>, maps:get(<<"AssumedRoleId">>, AssumedRoleUser)).

%% Test: AssumeRole makes correct HTTP call
test_assume_role_http_call() ->
    MockResponse = #{<<"Credentials">> => #{}},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(Method, {Url, Headers, ContentType, Body}, _HTTPOptions, _Options) ->
        %% Verify HTTP method
        ?assertEqual(post, Method),
        %% Verify URL
        ?assert(string:str(Url, "https://sts.amazonaws.com/assume-role") > 0),
        %% Verify content type
        ?assertEqual("application/json", ContentType),
        %% Verify body is present
        ?assert(is_list(Body)),
        ?assert(length(Body) > 0),
        %% Verify headers
        ?assert(is_list(Headers)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://sts.amazonaws.com">>},
    {ok, _Result} = sts_client:assume_role(Client, #{
        <<"RoleArn">> => <<"arn:aws:iam::123456789012:role/demo">>,
        <<"RoleSessionName">> => <<"session-name">>
    }),
    
    ?assert(meck:validate(httpc)).

%% Test: AssumeRole with PolicyArns and optional parameters
test_assume_role_with_policy_arns() ->
    MockResponse = #{
        <<"Credentials">> => #{
            <<"AccessKeyId">> => <<"AKIAIOSFODNN7EXAMPLE">>,
            <<"SecretAccessKey">> => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
            <<"SessionToken">> => <<"AQoDYXdzEJr...">>,
            <<"Expiration">> => <<"2024-01-01T12:00:00Z">>
        },
        <<"AssumedRoleUser">> => #{
            <<"AssumedRoleId">> => <<"AROA3XFRBF535PLBIFPI4:session-name">>,
            <<"Arn">> => <<"arn:aws:sts::123456789012:assumed-role/demo/session-name">>
        }
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(post, {_Url, _Headers, _ContentType, Body}, _HTTPOptions, _Options) ->
        %% Verify request body contains PolicyArns
        BodyMap = jsx:decode(list_to_binary(Body), [return_maps]),
        ?assert(maps:is_key(<<"PolicyArns">>, BodyMap)),
        ?assertEqual(3600, maps:get(<<"DurationSeconds">>, BodyMap)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://sts.amazonaws.com">>},
    {ok, Result} = sts_client:assume_role(Client, #{
        <<"RoleArn">> => <<"arn:aws:iam::123456789012:role/demo">>,
        <<"RoleSessionName">> => <<"session-name">>,
        <<"PolicyArns">> => [
            #{<<"arn">> => <<"arn:aws:iam::aws:policy/ReadOnlyAccess">>}
        ],
        <<"DurationSeconds">> => 3600,
        <<"ExternalId">> => <<"external-id-123">>
    }),
    
    ?assert(is_map(Result)).

%% Test: AssumeRole handles MalformedPolicyDocumentException
test_assume_role_malformed_policy_error() ->
    ErrorResponse = #{<<"message">> => <<"The request was rejected because the policy document was malformed">>},
    ResponseBody = jsx:encode(ErrorResponse),
    
    meck:expect(httpc, request, fun(post, {_Url, _Headers, _ContentType, _Body}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 400, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://sts.amazonaws.com">>},
    {error, {StatusCode, ErrorData}} = sts_client:assume_role(Client, #{
        <<"RoleArn">> => <<"arn:aws:iam::123456789012:role/demo">>,
        <<"RoleSessionName">> => <<"session-name">>,
        <<"Policy">> => <<"{invalid json}">>
    }),
    
    %% Verify error response
    ?assertEqual(400, StatusCode),
    ?assert(is_map(ErrorData)),
    ?assertEqual(<<"The request was rejected because the policy document was malformed">>, 
                 maps:get(<<"message">>, ErrorData)).

%% Test: AssumeRole handles PackedPolicyTooLargeException
test_assume_role_packed_policy_too_large_error() ->
    ErrorResponse = #{<<"message">> => <<"The request was rejected because the total packed size of the session policies exceeds the limit">>},
    ResponseBody = jsx:encode(ErrorResponse),
    
    meck:expect(httpc, request, fun(post, {_Url, _Headers, _ContentType, _Body}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 400, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://sts.amazonaws.com">>},
    {error, {StatusCode, ErrorData}} = sts_client:assume_role(Client, #{
        <<"RoleArn">> => <<"arn:aws:iam::123456789012:role/demo">>,
        <<"RoleSessionName">> => <<"session-name">>,
        <<"Policy">> => binary:copy(<<"x">>, 10000)  % Very large policy
    }),
    
    %% Verify error response
    ?assertEqual(400, StatusCode),
    ?assert(is_map(ErrorData)),
    ?assertEqual(<<"The request was rejected because the total packed size of the session policies exceeds the limit">>, 
                 maps:get(<<"message">>, ErrorData)).

%% Test: GetSessionToken returns successful response
test_get_session_token_success() ->
    %% Mock HTTP response
    MockResponse = #{
        <<"Credentials">> => #{
            <<"AccessKeyId">> => <<"AKIAIOSFODNN7EXAMPLE">>,
            <<"SecretAccessKey">> => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
            <<"SessionToken">> => <<"AQoDYXdzEJr...">>,
            <<"Expiration">> => <<"2024-01-01T12:00:00Z">>
        }
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(post, {_Url, _Headers, _ContentType, _Body}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://sts.amazonaws.com">>},
    {ok, Result} = sts_client:get_session_token(Client, #{}),
    
    %% Verify response structure
    ?assert(is_map(Result)),
    Credentials = maps:get(<<"Credentials">>, Result),
    ?assert(is_map(Credentials)),
    ?assertEqual(<<"AKIAIOSFODNN7EXAMPLE">>, maps:get(<<"AccessKeyId">>, Credentials)),
    ?assertEqual(<<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>, maps:get(<<"SecretAccessKey">>, Credentials)),
    ?assertEqual(<<"AQoDYXdzEJr...">>, maps:get(<<"SessionToken">>, Credentials)),
    ?assertEqual(<<"2024-01-01T12:00:00Z">>, maps:get(<<"Expiration">>, Credentials)).

%% Test: GetSessionToken makes correct HTTP call
test_get_session_token_http_call() ->
    MockResponse = #{<<"Credentials">> => #{}},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(Method, {Url, Headers, ContentType, Body}, _HTTPOptions, _Options) ->
        %% Verify HTTP method
        ?assertEqual(post, Method),
        %% Verify URL
        ?assert(string:str(Url, "https://sts.amazonaws.com/session-token") > 0),
        %% Verify content type
        ?assertEqual("application/json", ContentType),
        %% Verify body is present (even if empty map)
        ?assert(is_list(Body)),
        %% Verify headers
        ?assert(is_list(Headers)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://sts.amazonaws.com">>},
    {ok, _Result} = sts_client:get_session_token(Client, #{}),
    
    ?assert(meck:validate(httpc)).

%% Test: GetSessionToken with MFA parameters
test_get_session_token_with_mfa() ->
    MockResponse = #{
        <<"Credentials">> => #{
            <<"AccessKeyId">> => <<"AKIAIOSFODNN7EXAMPLE">>,
            <<"SecretAccessKey">> => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
            <<"SessionToken">> => <<"AQoDYXdzEJr...">>,
            <<"Expiration">> => <<"2024-01-01T12:00:00Z">>
        }
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(post, {_Url, _Headers, _ContentType, Body}, _HTTPOptions, _Options) ->
        %% Verify request body contains MFA parameters
        BodyMap = jsx:decode(list_to_binary(Body), [return_maps]),
        ?assertEqual(3600, maps:get(<<"DurationSeconds">>, BodyMap)),
        ?assertEqual(<<"arn:aws:iam::123456789012:mfa/user">>, maps:get(<<"SerialNumber">>, BodyMap)),
        ?assertEqual(<<"123456">>, maps:get(<<"TokenCode">>, BodyMap)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://sts.amazonaws.com">>},
    {ok, Result} = sts_client:get_session_token(Client, #{
        <<"DurationSeconds">> => 3600,
        <<"SerialNumber">> => <<"arn:aws:iam::123456789012:mfa/user">>,
        <<"TokenCode">> => <<"123456">>
    }),
    
    ?assert(is_map(Result)),
    ?assert(maps:is_key(<<"Credentials">>, Result)).

%% Test: Client creation works correctly
test_client_creation() ->
    Config = #{endpoint => <<"https://sts.amazonaws.com">>},
    {ok, Client} = sts_client:new(Config),
    
    ?assert(is_map(Client)),
    ?assertEqual(<<"https://sts.amazonaws.com">>, maps:get(endpoint, Client)).
