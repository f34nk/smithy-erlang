-module(storage_retry_example_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Example Tests for Using aws_retry with Storage Service
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Basic retry usage
%%--------------------------------------------------------------------
retry_api_call_example_test() ->
    %% Simulate an API call that might fail
    Counter = counters:new(1, []),
    
    ApiCall = fun() ->
        Count = counters:get(Counter, 1),
        counters:add(Counter, 1, 1),
        case Count of
            0 -> {error, {503, #{<<"message">> => <<"Service Unavailable">>}}};
            _ -> {ok, #{<<"locationId">> => <<"loc-123">>}}
        end
    end,
    
    %% Wrap with retry logic
    Result = aws_retry:with_retry(ApiCall, #{
        max_retries => 3,
        initial_backoff => 10
    }),
    
    ?assertMatch({ok, #{<<"locationId">> := _}}, Result).

%%--------------------------------------------------------------------
%% Test: Retry with client operation simulation
%%--------------------------------------------------------------------
retry_storage_operation_test() ->
    %% Simulate retrying a storage location lookup
    Counter = counters:new(1, []),
    
    GetStorageLocation = fun(Client, Input) ->
        Count = counters:get(Counter, 1),
        counters:add(Counter, 1, 1),
        case Count of
            0 -> {error, {500, #{<<"message">> => <<"Internal Server Error">>}}};
            1 -> {error, {503, #{<<"message">> => <<"Service Unavailable">>}}};
            _ -> {ok, #{
                <<"locationId">> => maps:get(<<"locationId">>, Input),
                <<"name">> => <<"My Storage">>,
                <<"storageType">> => #{<<"s3">> => #{}}
            }}
        end
    end,
    
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    Input = #{<<"locationId">> => <<"loc-456">>},
    
    %% Retry the operation
    Result = aws_retry:with_retry(
        fun() -> GetStorageLocation(Client, Input) end,
        #{
            max_retries => 3,
            initial_backoff => 10,
            jitter => false
        }
    ),
    
    ?assertMatch({ok, #{<<"locationId">> := <<"loc-456">>}}, Result),
    ?assertEqual(3, counters:get(Counter, 1)).

%%--------------------------------------------------------------------
%% Test: Non-retryable error returns immediately
%%--------------------------------------------------------------------
no_retry_on_client_error_test() ->
    Counter = counters:new(1, []),
    
    ApiCall = fun() ->
        counters:add(Counter, 1, 1),
        {error, {404, #{<<"message">> => <<"Location Not Found">>}}}
    end,
    
    Result = aws_retry:with_retry(ApiCall, #{max_retries => 3}),
    
    ?assertMatch({error, {404, _}}, Result),
    ?assertEqual(1, counters:get(Counter, 1)). % Only called once

%%--------------------------------------------------------------------
%% Test: Max retries exceeded
%%--------------------------------------------------------------------
max_retries_example_test() ->
    Counter = counters:new(1, []),
    
    ApiCall = fun() ->
        counters:add(Counter, 1, 1),
        {error, {500, #{<<"message">> => <<"Persistent Error">>}}}
    end,
    
    Result = aws_retry:with_retry(ApiCall, #{
        max_retries => 2,
        initial_backoff => 10
    }),
    
    ?assertMatch({error, {max_retries_exceeded, _}}, Result),
    {error, {max_retries_exceeded, Details}} = Result,
    ?assertEqual(3, maps:get(attempts, Details)),
    ?assertMatch({500, _}, maps:get(last_error, Details)).

%%--------------------------------------------------------------------
%% Test: Custom retry predicate
%%--------------------------------------------------------------------
custom_retry_predicate_test() ->
    Counter = counters:new(1, []),
    
    ApiCall = fun() ->
        Count = counters:get(Counter, 1),
        counters:add(Counter, 1, 1),
        case Count of
            0 -> {error, {storage_locked, <<"Storage is locked">>}};
            _ -> {ok, <<"success">>}
        end
    end,
    
    %% Custom predicate: retry on storage_locked errors
    CustomRetryable = fun({error, {storage_locked, _}}) -> true;
                         (_) -> false
                      end,
    
    Result = aws_retry:with_retry(ApiCall, #{
        max_retries => 2,
        initial_backoff => 10,
        retryable_errors => CustomRetryable
    }),
    
    ?assertEqual({ok, <<"success">>}, Result),
    ?assertEqual(2, counters:get(Counter, 1)).

%%--------------------------------------------------------------------
%% Test: Retry with logging
%%--------------------------------------------------------------------
retry_with_logging_test() ->
    Pid = self(),
    Counter = counters:new(1, []),
    
    ApiCall = fun() ->
        Count = counters:get(Counter, 1),
        counters:add(Counter, 1, 1),
        case Count of
            N when N < 2 -> {error, {500, #{}}};
            _ -> {ok, <<"success">>}
        end
    end,
    
    Logger = fun(LogData) ->
        Pid ! {log, LogData}
    end,
    
    Result = aws_retry:with_retry(ApiCall, #{
        max_retries => 3,
        initial_backoff => 10,
        jitter => false,
        logger => Logger
    }),
    
    ?assertEqual({ok, <<"success">>}, Result),
    
    %% Verify we received log messages
    receive {log, _} -> ok after 100 -> ?assert(false) end,
    receive {log, _} -> ok after 100 -> ?assert(false) end.

%%--------------------------------------------------------------------
%% Test: is_retryable_error utility
%%--------------------------------------------------------------------
is_retryable_check_test() ->
    %% Server errors are retryable
    ?assert(aws_retry:is_retryable_error({500, #{}})),
    ?assert(aws_retry:is_retryable_error({503, #{}})),
    ?assert(aws_retry:is_retryable_error({429, #{}})),
    
    %% Connection errors are retryable
    ?assert(aws_retry:is_retryable_error({error, timeout})),
    ?assert(aws_retry:is_retryable_error({error, econnrefused})),
    
    %% Client errors are not retryable
    ?assertNot(aws_retry:is_retryable_error({400, #{}})),
    ?assertNot(aws_retry:is_retryable_error({404, #{}})),
    ?assertNot(aws_retry:is_retryable_error({error, custom_error})).

%%--------------------------------------------------------------------
%% Test: Practical usage pattern
%%--------------------------------------------------------------------
practical_usage_pattern_test() ->
    %% Example: Wrapping a client call with retry
    Client = #{
        endpoint => <<"https://api.example.com">>,
        region => <<"us-east-1">>,
        access_key_id => <<"test_key">>,
        secret_access_key => <<"test_secret">>
    },
    
    Input = #{
        <<"name">> => <<"Test Storage">>,
        <<"storageType">> => #{<<"s3">> => #{}}
    },
    
    %% Validation happens first (no retry needed for validation errors)
    case storage_client:validate_create_storage_location_input(Input) of
        ok ->
            %% Make API call with retry
            Counter = counters:new(1, []),
            
            MockApiCall = fun() ->
                Count = counters:get(Counter, 1),
                counters:add(Counter, 1, 1),
                case Count of
                    0 -> {error, {503, #{}}};
                    _ -> {ok, #{<<"locationId">> => <<"loc-789">>}}
                end
            end,
            
            Result = aws_retry:with_retry(MockApiCall, #{
                max_retries => 3,
                initial_backoff => 100,
                max_backoff => 5000
            }),
            
            ?assertMatch({ok, #{<<"locationId">> := _}}, Result);
        
        {error, ValidationError} ->
            %% Don't retry validation errors
            ?assert(false)
    end.
