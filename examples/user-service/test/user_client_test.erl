-module(user_client_test).
-include_lib("eunit/include/eunit.hrl").

%%% Test suite for User client with mocked HTTP responses
%%% This tests the HTTP client behavior without making actual network calls

%% Setup and teardown for each test
setup() ->
    %% Start inets application for httpc
    _ = application:ensure_all_started(inets),
    meck:new(httpc, [unstick, passthrough]),
    ok.

teardown(_) ->
    meck:unload(httpc),
    ok.

%% Test fixture for all tests
user_client_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun test_get_user_success/0,
      fun test_get_user_http_call/0,
      fun test_get_user_not_found/0,
      fun test_create_user_success/0,
      fun test_create_user_http_call/0,
      fun test_create_user_with_optional_fields/0,
      fun test_create_user_invalid_data/0,
      fun test_update_user_success/0,
      fun test_update_user_http_call/0,
      fun test_update_user_partial_update/0,
      fun test_update_user_not_found/0,
      fun test_update_user_invalid_data/0,
      fun test_client_creation/0
     ]}.

%% Test: GetUser returns successful response
test_get_user_success() ->
    %% Mock HTTP response
    MockResponse = #{
        <<"user">> => #{
            <<"userId">> => <<"user-123">>,
            <<"name">> => <<"John Doe">>,
            <<"email">> => <<"john@example.com">>,
            <<"age">> => 30,
            <<"status">> => <<"active">>,
            <<"createdAt">> => <<"2024-01-01T12:00:00Z">>,
            <<"updatedAt">> => <<"2024-01-15T10:30:00Z">>
        }
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(get, {_Url, _Headers}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {ok, Result} = user_client:get_user(Client, #{
        <<"userId">> => <<"user-123">>
    }),
    
    %% Verify response structure
    ?assert(is_map(Result)),
    User = maps:get(<<"user">>, Result),
    ?assert(is_map(User)),
    ?assertEqual(<<"user-123">>, maps:get(<<"userId">>, User)),
    ?assertEqual(<<"John Doe">>, maps:get(<<"name">>, User)),
    ?assertEqual(<<"john@example.com">>, maps:get(<<"email">>, User)),
    ?assertEqual(30, maps:get(<<"age">>, User)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, User)),
    
    %% Verify httpc was called
    ?assert(meck:called(httpc, request, '_')).

%% Test: GetUser makes correct HTTP call with URL parameter
test_get_user_http_call() ->
    MockResponse = #{<<"user">> => #{}},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(Method, {Url, Headers}, _HTTPOptions, _Options) ->
        %% Verify HTTP method
        ?assertEqual(get, Method),
        %% Verify URL contains endpoint and path with userId
        ?assert(string:str(Url, "https://api.example.com/users/") > 0),
        %% Verify headers
        ?assert(is_list(Headers)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {ok, _Result} = user_client:get_user(Client, #{
        <<"userId">> => <<"user-123">>
    }),
    
    ?assert(meck:validate(httpc)).

%% Test: GetUser handles 404 UserNotFound error
test_get_user_not_found() ->
    ErrorResponse = #{
        <<"message">> => <<"User not found">>,
        <<"userId">> => <<"user-999">>
    },
    ResponseBody = jsx:encode(ErrorResponse),
    
    meck:expect(httpc, request, fun(get, {_Url, _Headers}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 404, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {error, {aws_error, StatusCode, _Code, Message}} = user_client:get_user(Client, #{
        <<"userId">> => <<"user-999">>
    }),
    
    %% Verify error response
    ?assertEqual(404, StatusCode),
    ?assertEqual(<<"User not found">>, Message).

%% Test: CreateUser returns successful response
test_create_user_success() ->
    %% Mock HTTP response
    MockResponse = #{
        <<"userId">> => <<"user-456">>,
        <<"user">> => #{
            <<"userId">> => <<"user-456">>,
            <<"name">> => <<"Jane Smith">>,
            <<"email">> => <<"jane@example.com">>,
            <<"age">> => 28,
            <<"status">> => <<"active">>,
            <<"createdAt">> => <<"2024-01-20T14:30:00Z">>,
            <<"updatedAt">> => <<"2024-01-20T14:30:00Z">>
        }
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(post, {_Url, _Headers, _ContentType, _Body}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {ok, Result} = user_client:create_user(Client, #{
        <<"name">> => <<"Jane Smith">>,
        <<"email">> => <<"jane@example.com">>,
        <<"age">> => 28,
        <<"status">> => <<"active">>
    }),
    
    %% Verify response structure
    ?assert(is_map(Result)),
    ?assertEqual(<<"user-456">>, maps:get(<<"userId">>, Result)),
    User = maps:get(<<"user">>, Result),
    ?assert(is_map(User)),
    ?assertEqual(<<"Jane Smith">>, maps:get(<<"name">>, User)),
    ?assertEqual(<<"jane@example.com">>, maps:get(<<"email">>, User)).

%% Test: CreateUser makes correct HTTP call
test_create_user_http_call() ->
    MockResponse = #{<<"userId">> => <<"user-456">>, <<"user">> => #{}},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(Method, {Url, Headers, ContentType, Body}, _HTTPOptions, _Options) ->
        %% Verify HTTP method
        ?assertEqual(post, Method),
        %% Verify URL
        ?assert(string:str(Url, "https://api.example.com/users") > 0),
        %% Verify content type
        ?assertEqual("application/json", ContentType),
        %% Verify body contains user data (may be binary or list)
        BodyBinary = if is_binary(Body) -> Body; is_list(Body) -> list_to_binary(Body) end,
        BodyMap = jsx:decode(BodyBinary, [return_maps]),
        ?assert(maps:is_key(<<"name">>, BodyMap)),
        ?assert(maps:is_key(<<"email">>, BodyMap)),
        %% Verify headers
        ?assert(is_list(Headers)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {ok, _Result} = user_client:create_user(Client, #{
        <<"name">> => <<"Jane Smith">>,
        <<"email">> => <<"jane@example.com">>
    }),
    
    ?assert(meck:validate(httpc)).

%% Test: CreateUser with optional fields
test_create_user_with_optional_fields() ->
    MockResponse = #{
        <<"userId">> => <<"user-789">>,
        <<"user">> => #{
            <<"userId">> => <<"user-789">>,
            <<"name">> => <<"Bob Wilson">>,
            <<"email">> => <<"bob@example.com">>,
            <<"status">> => <<"inactive">>
        }
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(post, {_Url, _Headers, _ContentType, Body}, _HTTPOptions, _Options) ->
        %% Verify optional fields are included
        BodyBinary = if is_binary(Body) -> Body; is_list(Body) -> list_to_binary(Body) end,
        BodyMap = jsx:decode(BodyBinary, [return_maps]),
        ?assertEqual(<<"inactive">>, maps:get(<<"status">>, BodyMap)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {ok, Result} = user_client:create_user(Client, #{
        <<"name">> => <<"Bob Wilson">>,
        <<"email">> => <<"bob@example.com">>,
        <<"status">> => <<"inactive">>
    }),
    
    ?assert(is_map(Result)),
    ?assertEqual(<<"user-789">>, maps:get(<<"userId">>, Result)).

%% Test: CreateUser handles 400 InvalidUserData error
test_create_user_invalid_data() ->
    ErrorResponse = #{
        <<"message">> => <<"Invalid email format">>,
        <<"field">> => <<"email">>
    },
    ResponseBody = jsx:encode(ErrorResponse),
    
    meck:expect(httpc, request, fun(post, {_Url, _Headers, _ContentType, _Body}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 400, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {error, {aws_error, StatusCode, _Code, Message}} = user_client:create_user(Client, #{
        <<"name">> => <<"Test User">>,
        <<"email">> => <<"invalid-email">>
    }),
    
    %% Verify error response
    ?assertEqual(400, StatusCode),
    ?assertEqual(<<"Invalid email format">>, Message).

%% Test: UpdateUser returns successful response
test_update_user_success() ->
    %% Mock HTTP response
    MockResponse = #{
        <<"user">> => #{
            <<"userId">> => <<"user-123">>,
            <<"name">> => <<"John Updated">>,
            <<"email">> => <<"john.updated@example.com">>,
            <<"age">> => 31,
            <<"status">> => <<"active">>,
            <<"createdAt">> => <<"2024-01-01T12:00:00Z">>,
            <<"updatedAt">> => <<"2024-01-25T16:45:00Z">>
        }
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(put, {_Url, _Headers, _ContentType, _Body}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {ok, Result} = user_client:update_user(Client, #{
        <<"userId">> => <<"user-123">>,
        <<"name">> => <<"John Updated">>,
        <<"email">> => <<"john.updated@example.com">>,
        <<"age">> => 31
    }),
    
    %% Verify response structure
    ?assert(is_map(Result)),
    User = maps:get(<<"user">>, Result),
    ?assert(is_map(User)),
    ?assertEqual(<<"user-123">>, maps:get(<<"userId">>, User)),
    ?assertEqual(<<"John Updated">>, maps:get(<<"name">>, User)),
    ?assertEqual(<<"john.updated@example.com">>, maps:get(<<"email">>, User)),
    ?assertEqual(31, maps:get(<<"age">>, User)).

%% Test: UpdateUser makes correct HTTP call
test_update_user_http_call() ->
    MockResponse = #{<<"user">> => #{}},
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(Method, {Url, Headers, ContentType, Body}, _HTTPOptions, _Options) ->
        %% Verify HTTP method
        ?assertEqual(put, Method),
        %% Verify URL contains userId in path (REST-style: /users/{userId})
        ?assert(string:str(Url, "https://api.example.com/users/user-123") > 0),
        %% Verify content type
        ?assertEqual("application/json", ContentType),
        %% Verify body contains update data (userId is in URL path, not body)
        BodyBinary = if is_binary(Body) -> Body; is_list(Body) -> list_to_binary(Body) end,
        BodyMap = jsx:decode(BodyBinary, [return_maps]),
        ?assert(maps:is_key(<<"name">>, BodyMap)),
        %% Verify headers
        ?assert(is_list(Headers)),
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {ok, _Result} = user_client:update_user(Client, #{
        <<"userId">> => <<"user-123">>,
        <<"name">> => <<"John Updated">>
    }),
    
    ?assert(meck:validate(httpc)).

%% Test: UpdateUser with partial update (only some fields)
test_update_user_partial_update() ->
    MockResponse = #{
        <<"user">> => #{
            <<"userId">> => <<"user-123">>,
            <<"name">> => <<"John Partial">>,
            <<"email">> => <<"john@example.com">>,
            <<"age">> => 30,
            <<"status">> => <<"active">>
        }
    },
    ResponseBody = jsx:encode(MockResponse),
    
    meck:expect(httpc, request, fun(put, {Url, _Headers, _ContentType, Body}, _HTTPOptions, _Options) ->
        %% Verify userId is in URL path (REST-style)
        ?assert(string:str(Url, "/users/user-123") > 0),
        %% Verify only updated fields are sent in body
        BodyBinary = if is_binary(Body) -> Body; is_list(Body) -> list_to_binary(Body) end,
        BodyMap = jsx:decode(BodyBinary, [return_maps]),
        ?assertEqual(<<"John Partial">>, maps:get(<<"name">>, BodyMap)),
        %% Other fields should not be in the body (or can be omitted)
        {ok, {{undefined, 200, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {ok, Result} = user_client:update_user(Client, #{
        <<"userId">> => <<"user-123">>,
        <<"name">> => <<"John Partial">>
    }),
    
    ?assert(is_map(Result)),
    User = maps:get(<<"user">>, Result),
    ?assertEqual(<<"John Partial">>, maps:get(<<"name">>, User)).

%% Test: UpdateUser handles 404 UserNotFound error
test_update_user_not_found() ->
    ErrorResponse = #{
        <<"message">> => <<"User not found">>,
        <<"userId">> => <<"user-999">>
    },
    ResponseBody = jsx:encode(ErrorResponse),
    
    meck:expect(httpc, request, fun(put, {_Url, _Headers, _ContentType, _Body}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 404, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {error, {aws_error, StatusCode, _Code, Message}} = user_client:update_user(Client, #{
        <<"userId">> => <<"user-999">>,
        <<"name">> => <<"New Name">>
    }),
    
    %% Verify error response
    ?assertEqual(404, StatusCode),
    ?assertEqual(<<"User not found">>, Message).

%% Test: UpdateUser handles 400 InvalidUserData error
test_update_user_invalid_data() ->
    ErrorResponse = #{
        <<"message">> => <<"Invalid age value">>,
        <<"field">> => <<"age">>
    },
    ResponseBody = jsx:encode(ErrorResponse),
    
    meck:expect(httpc, request, fun(put, {_Url, _Headers, _ContentType, _Body}, _HTTPOptions, _Options) ->
        {ok, {{undefined, 400, undefined}, [], ResponseBody}}
    end),
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    {error, {aws_error, StatusCode, _Code, Message}} = user_client:update_user(Client, #{
        <<"userId">> => <<"user-123">>,
        <<"age">> => -5
    }),
    
    %% Verify error response
    ?assertEqual(400, StatusCode),
    ?assertEqual(<<"Invalid age value">>, Message).

%% Test: Client creation works correctly
test_client_creation() ->
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = user_client:new(Config),
    
    ?assert(is_map(Client)),
    ?assertEqual(<<"https://api.example.com">>, maps:get(endpoint, Client)).
