-module(paginated_service_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite: Pagination Helper Functions (Comprehensive)
%%
%% This test suite uses HTTP client mocking to intercept the actual
%% httpc:request/4 calls made by the generated client code.
%%
%% HTTP Mocking Strategy:
%% - Mock httpc:request/4 to return synthetic HTTP responses
%% - Parse request URLs/bodies to determine pagination state
%% - Return appropriate paginated responses based on request tokens
%% - Supports both list_items and list_items_with_page_size operations
%%====================================================================

%%--------------------------------------------------------------------
%% Test Fixtures
%%--------------------------------------------------------------------

%% Setup and teardown for tests that use HTTP mocking
setup_http_mock() ->
    %% Start inets and ensure httpc is available
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    
    %% Clean up any existing mocks
    catch meck:unload(httpc),
    
    %% Mock httpc with passthrough for any unmocked functions
    meck:new(httpc, [passthrough, unstick]),
    ok.

cleanup_http_mock(_) ->
    %% Unload the mock
    catch meck:unload(httpc),
    ok.

%%--------------------------------------------------------------------
%% HTTP Response Helpers
%%--------------------------------------------------------------------

%% Create a successful HTTP response in the format httpc returns
http_ok_response(Body) when is_binary(Body) ->
    {ok, {{"HTTP/1.1", 200, "OK"}, [], Body}};
http_ok_response(Map) when is_map(Map) ->
    Body = jsx:encode(Map),
    {ok, {{"HTTP/1.1", 200, "OK"}, [], Body}}.

%% Create an error HTTP response
http_error_response(StatusCode, Message) when is_integer(StatusCode), is_binary(Message) ->
    Body = jsx:encode(#{<<"message">> => Message}),
    {ok, {{"HTTP/1.1", StatusCode, "Error"}, [], Body}}.

%% Extract URL from httpc request (handles both GET and POST formats)
extract_url({Url, _Headers}) when is_list(Url) ->
    Url;
extract_url({Url, _Headers, _ContentType, _Body}) when is_list(Url) ->
    Url.

%% Extract the nextToken from a URL query string
extract_token_from_url(Request) ->
    Url = extract_url(Request),
    case string:split(Url, "?", trailing) of
        [_, QueryString] ->
            Params = uri_string:dissect_query(QueryString),
            proplists:get_value("nextToken", Params);
        _ ->
            undefined
    end.

%%--------------------------------------------------------------------
%% Module Loads
%%--------------------------------------------------------------------

module_loads_test() ->
    ?assert(erlang:function_exported(paginated_service, new, 1)),
    ?assert(erlang:function_exported(paginated_service, list_items, 2)),
    ?assert(erlang:function_exported(paginated_service, list_items, 3)),
    ?assert(erlang:function_exported(paginated_service, list_items_all_pages, 2)),
    ?assert(erlang:function_exported(paginated_service, list_items_all_pages, 3)),
    ?assert(erlang:function_exported(paginated_service, list_items_with_page_size, 2)),
    ?assert(erlang:function_exported(paginated_service, list_items_with_page_size, 3)),
    ?assert(erlang:function_exported(paginated_service, list_items_with_page_size_all_pages, 2)),
    ?assert(erlang:function_exported(paginated_service, list_items_with_page_size_all_pages, 3)).

%%--------------------------------------------------------------------
%% Client Creation
%%--------------------------------------------------------------------

client_creation_test() ->
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    ?assertMatch(#{endpoint := <<"https://api.example.com">>}, Client).

%%--------------------------------------------------------------------
%% Pagination Tests with Fixtures
%%--------------------------------------------------------------------

pagination_test_() ->
    {foreach,
        fun setup_http_mock/0,
        fun cleanup_http_mock/1,
        [
            fun single_page_test/0,
            fun multi_page_test/0,
            fun empty_result_test/0,
            fun empty_token_test/0,
            fun error_on_first_page_test/0,
            fun error_on_second_page_test/0,
            fun pagination_with_options_test/0,
            fun page_size_pagination_test/0,
            fun token_propagation_test/0,
            fun large_result_set_test/0,
            fun order_preservation_test/0
        ]
    }.

%%--------------------------------------------------------------------
%% Single Page Response Tests
%%--------------------------------------------------------------------

single_page_test() ->
    %% Mock single page response (no nextToken)
    Items = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Item 1">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"Item 2">>},
        #{<<"id">> => <<"3">>, <<"name">> => <<"Item 3">>}
    ],
    
    %% Mock httpc:request/4 to return a single page of results
    meck:expect(httpc, request, 4,
        fun(_Method, _Request, _HttpOptions, _Options) ->
            http_ok_response(#{<<"items">> => Items})
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{},
    
    Result = paginated_service:list_items_all_pages(Client, Input),
    
    ?assertMatch({ok, _}, Result),
    {ok, AllItems} = Result,
    ?assertEqual(3, length(AllItems)),
    ?assertEqual(Items, AllItems).

%%--------------------------------------------------------------------
%% Multi-Page Response Tests
%%--------------------------------------------------------------------

multi_page_test() ->
    %% Mock three-page response
    Page1Items = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Item 1">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"Item 2">>}
    ],
    Page2Items = [
        #{<<"id">> => <<"3">>, <<"name">> => <<"Item 3">>},
        #{<<"id">> => <<"4">>, <<"name">> => <<"Item 4">>}
    ],
    Page3Items = [
        #{<<"id">> => <<"5">>, <<"name">> => <<"Item 5">>}
    ],
    
    %% Mock httpc:request/4 with stateful responses based on URL token
    meck:expect(httpc, request, 4,
        fun(_Method, Request, _HttpOptions, _Options) ->
            Token = extract_token_from_url(Request),
            case Token of
                undefined ->
                    %% First page
                    http_ok_response(#{<<"items">> => Page1Items, <<"nextToken">> => <<"token2">>});
                "token2" ->
                    %% Second page
                    http_ok_response(#{<<"items">> => Page2Items, <<"nextToken">> => <<"token3">>});
                "token3" ->
                    %% Third page (last)
                    http_ok_response(#{<<"items">> => Page3Items})
            end
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{},
    
    Result = paginated_service:list_items_all_pages(Client, Input),
    
    ?assertMatch({ok, _}, Result),
    {ok, AllItems} = Result,
    ?assertEqual(5, length(AllItems)),
    ?assertEqual(Page1Items ++ Page2Items ++ Page3Items, AllItems).

%%--------------------------------------------------------------------
%% Empty Result Set Test
%%--------------------------------------------------------------------

empty_result_test() ->
    %% Mock empty response
    meck:expect(httpc, request, 4,
        fun(_Method, _Request, _HttpOptions, _Options) ->
            http_ok_response(#{<<"items">> => []})
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{},
    
    Result = paginated_service:list_items_all_pages(Client, Input),
    
    ?assertMatch({ok, []}, Result).

%%--------------------------------------------------------------------
%% Empty Token Test
%%--------------------------------------------------------------------

empty_token_test() ->
    %% Mock response with empty nextToken
    Items = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Item 1">>}
    ],
    
    meck:expect(httpc, request, 4,
        fun(_Method, _Request, _HttpOptions, _Options) ->
            http_ok_response(#{<<"items">> => Items, <<"nextToken">> => <<>>})
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{},
    
    Result = paginated_service:list_items_all_pages(Client, Input),
    
    ?assertMatch({ok, [_]}, Result),
    {ok, AllItems} = Result,
    ?assertEqual(1, length(AllItems)).

%%--------------------------------------------------------------------
%% Error Handling Tests
%%--------------------------------------------------------------------

error_on_first_page_test() ->
    %% Mock error on first page
    meck:expect(httpc, request, 4,
        fun(_Method, _Request, _HttpOptions, _Options) ->
            http_error_response(500, <<"Internal Server Error">>)
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{},
    
    Result = paginated_service:list_items_all_pages(Client, Input),
    
    %% Should return error tuple
    ?assertMatch({error, _}, Result).

error_on_second_page_test() ->
    %% Mock error on second page (no partial results)
    Page1Items = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Item 1">>}
    ],
    
    meck:expect(httpc, request, 4,
        fun(_Method, Request, _HttpOptions, _Options) ->
            Token = extract_token_from_url(Request),
            case Token of
                undefined ->
                    %% First page succeeds
                    http_ok_response(#{<<"items">> => Page1Items, <<"nextToken">> => <<"token2">>});
                "token2" ->
                    %% Second page fails
                    http_error_response(503, <<"Service Unavailable">>)
            end
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{},
    
    Result = paginated_service:list_items_all_pages(Client, Input),
    
    %% Should return error, not partial results
    ?assertMatch({error, _}, Result).

%%--------------------------------------------------------------------
%% Options Support Tests
%%--------------------------------------------------------------------

pagination_with_options_test() ->
    %% Test that pagination works with custom options
    Items = [#{<<"id">> => <<"1">>, <<"name">> => <<"Item 1">>}],
    
    meck:expect(httpc, request, 4,
        fun(_Method, _Request, _HttpOptions, _Options) ->
            http_ok_response(#{<<"items">> => Items})
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{},
    Options = #{max_retries => 5, initial_backoff => 100},
    
    Result = paginated_service:list_items_all_pages(Client, Input, Options),
    
    ?assertMatch({ok, [_]}, Result).

%%--------------------------------------------------------------------
%% Page Size Tests
%%--------------------------------------------------------------------

page_size_pagination_test() ->
    %% Test list_items_with_page_size_all_pages
    Page1Items = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Item 1">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"Item 2">>}
    ],
    Page2Items = [
        #{<<"id">> => <<"3">>, <<"name">> => <<"Item 3">>}
    ],
    
    meck:expect(httpc, request, 4,
        fun(_Method, Request, _HttpOptions, _Options) ->
            Token = extract_token_from_url(Request),
            case Token of
                undefined ->
                    http_ok_response(#{<<"items">> => Page1Items, <<"nextToken">> => <<"token2">>});
                "token2" ->
                    http_ok_response(#{<<"items">> => Page2Items})
            end
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{<<"maxResults">> => 2},
    
    Result = paginated_service:list_items_with_page_size_all_pages(Client, Input),
    
    ?assertMatch({ok, _}, Result),
    {ok, AllItems} = Result,
    ?assertEqual(3, length(AllItems)).

%%--------------------------------------------------------------------
%% Token Propagation Tests
%%--------------------------------------------------------------------

token_propagation_test() ->
    %% Verify that tokens are correctly propagated
    Self = self(),
    CallCount = atomics:new(1, []),
    
    meck:expect(httpc, request, 4,
        fun(_Method, Request, _HttpOptions, _Options) ->
            Count = atomics:add_get(CallCount, 1, 1),
            Token = extract_token_from_url(Request),
            Self ! {call, Count, Token},
            
            case Count of
                1 ->
                    http_ok_response(#{<<"items">> => [#{<<"id">> => <<"1">>}], 
                                      <<"nextToken">> => <<"token_abc">>});
                2 ->
                    http_ok_response(#{<<"items">> => [#{<<"id">> => <<"2">>}], 
                                      <<"nextToken">> => <<"token_xyz">>});
                3 ->
                    http_ok_response(#{<<"items">> => [#{<<"id">> => <<"3">>}]})
            end
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{},
    
    Result = paginated_service:list_items_all_pages(Client, Input),
    
    %% Verify we got all three calls with correct tokens
    receive {call, 1, undefined} -> ok after 1000 -> ?assert(false, "First call not received") end,
    receive {call, 2, "token_abc"} -> ok after 1000 -> ?assert(false, "Second call not received") end,
    receive {call, 3, "token_xyz"} -> ok after 1000 -> ?assert(false, "Third call not received") end,
    
    ?assertMatch({ok, _}, Result),
    {ok, AllItems} = Result,
    ?assertEqual(3, length(AllItems)).

%%--------------------------------------------------------------------
%% Large Result Set Tests
%%--------------------------------------------------------------------

large_result_set_test() ->
    %% Test with many pages
    NumPages = 10,
    ItemsPerPage = 100,
    
    meck:expect(httpc, request, 4,
        fun(_Method, Request, _HttpOptions, _Options) ->
            Token = extract_token_from_url(Request),
            PageNum = case Token of
                undefined -> 0;
                T -> list_to_integer(T)
            end,
            
            Items = [#{<<"id">> => integer_to_binary(PageNum * ItemsPerPage + I),
                      <<"name">> => <<"Item ", (integer_to_binary(PageNum * ItemsPerPage + I))/binary>>}
                    || I <- lists:seq(1, ItemsPerPage)],
            
            case PageNum < NumPages - 1 of
                true ->
                    NextToken = integer_to_list(PageNum + 1),
                    http_ok_response(#{<<"items">> => Items, <<"nextToken">> => list_to_binary(NextToken)});
                false ->
                    http_ok_response(#{<<"items">> => Items})
            end
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{},
    
    Result = paginated_service:list_items_all_pages(Client, Input),
    
    ?assertMatch({ok, _}, Result),
    {ok, AllItems} = Result,
    ?assertEqual(NumPages * ItemsPerPage, length(AllItems)).

%%--------------------------------------------------------------------
%% Order Preservation Tests
%%--------------------------------------------------------------------

order_preservation_test() ->
    %% Verify items maintain order across pages
    Page1Items = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"First">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"Second">>}
    ],
    Page2Items = [
        #{<<"id">> => <<"3">>, <<"name">> => <<"Third">>},
        #{<<"id">> => <<"4">>, <<"name">> => <<"Fourth">>}
    ],
    Page3Items = [
        #{<<"id">> => <<"5">>, <<"name">> => <<"Fifth">>}
    ],
    
    meck:expect(httpc, request, 4,
        fun(_Method, Request, _HttpOptions, _Options) ->
            Token = extract_token_from_url(Request),
            case Token of
                undefined -> http_ok_response(#{<<"items">> => Page1Items, <<"nextToken">> => <<"t2">>});
                "t2" -> http_ok_response(#{<<"items">> => Page2Items, <<"nextToken">> => <<"t3">>});
                "t3" -> http_ok_response(#{<<"items">> => Page3Items})
            end
        end),
    
    Config = #{endpoint => <<"https://api.example.com">>,
               access_key_id => <<"test_key">>,
               secret_access_key => <<"test_secret">>,
               region => <<"us-east-1">>},
    {ok, Client} = paginated_service:new(Config),
    Input = #{},
    
    Result = paginated_service:list_items_all_pages(Client, Input),
    
    ?assertMatch({ok, _}, Result),
    {ok, AllItems} = Result,
    
    %% Verify exact order
    ExpectedOrder = Page1Items ++ Page2Items ++ Page3Items,
    ?assertEqual(ExpectedOrder, AllItems),
    
    %% Verify each ID is in sequence
    Ids = [maps:get(<<"id">>, Item) || Item <- AllItems],
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>], Ids).
