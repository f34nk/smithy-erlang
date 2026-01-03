-module(aws_demo_app).
-export([run/0]).

-define(LAMBDA_FUNCTION_NAME, <<"lambda-demo-function">>).

run() ->
    io:format("~n=== Running Lambda Client Application ===~n~n"),

    %% Create Lambda client instance with AWS credentials
    io:format("Creating Lambda client...~n"),
    Config = #{
        endpoint => unicode:characters_to_binary(os:getenv("AWS_ENDPOINT")),
        region => <<"us-east-1">>,
        service => <<"lambda">>,  %% Required for SigV4 signing with custom endpoints
        credentials => #{
            access_key_id => <<"dummy">>,
            secret_access_key => <<"dummy">>
        }
    },
    {ok, Client} = aws_lambda_client:new(Config),
    io:format("Client created successfully~n~n"),

    %% Build the function ARN for tag operations
    FunctionArn = <<"arn:aws:lambda:us-east-1:000000000000:function:", ?LAMBDA_FUNCTION_NAME/binary>>,

    %% 1. Get account settings
    io:format("--- GetAccountSettings ---~n"),
    case aws_lambda_client:get_account_settings(Client, #{}, #{enable_retry => false}) of
        {ok, SettingsOutput} ->
            io:format("SUCCESS: Account settings retrieved~n"),
            case maps:get(<<"AccountLimit">>, SettingsOutput, undefined) of
                undefined ->
                    io:format("  No account limits available~n");
                Limits ->
                    io:format("  Account Limits:~n"),
                    print_limits(Limits)
            end,
            case maps:get(<<"AccountUsage">>, SettingsOutput, undefined) of
                undefined ->
                    io:format("  No account usage available~n");
                Usage ->
                    io:format("  Account Usage:~n"),
                    TotalCodeSize = maps:get(<<"TotalCodeSize">>, Usage, 0),
                    FunctionCount = maps:get(<<"FunctionCount">>, Usage, 0),
                    io:format("    Total code size: ~p bytes~n", [TotalCodeSize]),
                    io:format("    Function count: ~p~n", [FunctionCount])
            end;
        {error, SettingsError} ->
            io:format("ERROR: ~p~n", [SettingsError])
    end,
    io:format("~n"),

    %% 2. List tags on the function (created by Terraform)
    io:format("--- ListTags ---~n"),
    ListTagsInput = #{<<"Resource">> => FunctionArn},
    case aws_lambda_client:list_tags(Client, ListTagsInput, #{enable_retry => false}) of
        {ok, ListTagsOutput} ->
            Tags = maps:get(<<"Tags">>, ListTagsOutput, #{}),
            io:format("SUCCESS: Found ~p tag(s)~n", [maps:size(Tags)]),
            maps:foreach(
                fun(Key, Value) ->
                    io:format("  ~s: ~s~n", [Key, Value])
                end,
                Tags
            );
        {error, ListTagsError} ->
            io:format("ERROR: ~p~n", [ListTagsError])
    end,
    io:format("~n"),

    %% 3. Add a new tag to the function
    io:format("--- TagResource ---~n"),
    TagInput = #{
        <<"Resource">> => FunctionArn,
        <<"Tags">> => #{
            <<"AddedBy">> => <<"smithy-erlang-demo">>,
            <<"Timestamp">> => list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second)))
        }
    },
    case aws_lambda_client:tag_resource(Client, TagInput, #{enable_retry => false}) of
        {ok, _TagOutput} ->
            io:format("SUCCESS: Tags added to function~n");
        {error, TagError} ->
            io:format("ERROR: ~p~n", [TagError])
    end,
    io:format("~n"),

    %% 4. List tags again to verify
    io:format("--- ListTags (verify) ---~n"),
    case aws_lambda_client:list_tags(Client, ListTagsInput, #{enable_retry => false}) of
        {ok, ListTagsOutput2} ->
            Tags2 = maps:get(<<"Tags">>, ListTagsOutput2, #{}),
            io:format("SUCCESS: Found ~p tag(s)~n", [maps:size(Tags2)]),
            maps:foreach(
                fun(Key, Value) ->
                    io:format("  ~s: ~s~n", [Key, Value])
                end,
                Tags2
            );
        {error, ListTagsError2} ->
            io:format("ERROR: ~p~n", [ListTagsError2])
    end,
    io:format("~n"),

    %% 5. Remove the tag we added
    io:format("--- UntagResource ---~n"),
    UntagInput = #{
        <<"Resource">> => FunctionArn,
        <<"TagKeys">> => [<<"AddedBy">>, <<"Timestamp">>]
    },
    case aws_lambda_client:untag_resource(Client, UntagInput, #{enable_retry => false}) of
        {ok, _UntagOutput} ->
            io:format("SUCCESS: Tags removed from function~n");
        {error, UntagError} ->
            io:format("ERROR: ~p~n", [UntagError])
    end,
    io:format("~n"),

    %% 6. List tags one more time to confirm removal
    io:format("--- ListTags (confirm removal) ---~n"),
    case aws_lambda_client:list_tags(Client, ListTagsInput, #{enable_retry => false}) of
        {ok, ListTagsOutput3} ->
            Tags3 = maps:get(<<"Tags">>, ListTagsOutput3, #{}),
            io:format("SUCCESS: Found ~p tag(s)~n", [maps:size(Tags3)]),
            maps:foreach(
                fun(Key, Value) ->
                    io:format("  ~s: ~s~n", [Key, Value])
                end,
                Tags3
            );
        {error, ListTagsError3} ->
            io:format("ERROR: ~p~n", [ListTagsError3])
    end,
    io:format("~n"),

    %% 7. Delete the function
    io:format("--- DeleteFunction ---~n"),
    DeleteInput = #{<<"FunctionName">> => ?LAMBDA_FUNCTION_NAME},
    case aws_lambda_client:delete_function(Client, DeleteInput, #{enable_retry => false}) of
        {ok, DeleteOutput} ->
            StatusCode = maps:get(<<"StatusCode">>, DeleteOutput, 204),
            io:format("SUCCESS: Function deleted (status: ~p)~n", [StatusCode]);
        {error, DeleteError} ->
            io:format("ERROR: ~p~n", [DeleteError])
    end,
    io:format("~n"),

    %% 8. Verify deletion by trying to list tags (should fail)
    io:format("--- ListTags (verify deletion) ---~n"),
    case aws_lambda_client:list_tags(Client, ListTagsInput, #{enable_retry => false}) of
        {ok, _} ->
            io:format("UNEXPECTED: Function still exists~n");
        {error, {aws_error, 404, _, _}} ->
            io:format("SUCCESS: Function confirmed deleted (404 Not Found)~n");
        {error, VerifyError} ->
            io:format("Result: ~p~n", [VerifyError])
    end,
    io:format("~n"),

    io:format("=== Lambda Client Application Complete ===~n"),
    ok.

%% Helper to print account limits
print_limits(Limits) when is_map(Limits) ->
    maps:foreach(
        fun(Key, Value) when is_integer(Value) ->
                io:format("    ~s: ~p~n", [Key, Value]);
           (Key, Value) ->
                io:format("    ~s: ~p~n", [Key, Value])
        end,
        Limits
    ).
