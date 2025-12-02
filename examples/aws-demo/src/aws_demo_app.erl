-module(aws_demo_app).
-export([run/0]).

run() ->
    io:format("~n=== Running S3 Client Application ===~n~n"),
    
    %% Create S3 client instance with AWS credentials
    %% Use 'moto' hostname as it's the Docker Compose service name
    io:format("Creating S3 client...~n"),
    Config = #{
        endpoint => <<"http://moto:5050">>,
        region => <<"us-east-1">>,
        credentials => #{
            access_key_id => <<"dummy">>,
            secret_access_key => <<"dummy">>
        }
    },
    {ok, Client} = s3_client:new(Config),
    io:format("Client created successfully~n~n"),
    
    %% Put an object to S3 bucket
    % io:format("Putting object to S3 bucket...~n"),
    % PutInput = #{
    %     <<"bucket">> => <<"my-test-bucket">>,
    %     <<"key">> => <<"test-file.txt">>,
    %     <<"body">> => <<"Hello from Smithy Erlang S3 Client!">>,
    %     <<"contentType">> => <<"text/plain">>
    % },
    
    % case s3_client:put_object(Client, PutInput) of
    %     {ok, PutOutput} ->
    %         io:format("Object uploaded successfully~n"),
    %         io:format("  ETag: ~p~n~n", [maps:get(<<"eTag">>, PutOutput, undefined)]);
    %     {error, PutError} ->
    %         io:format("Failed to upload object: ~p~n~n", [PutError])
    % end,
    
    % %% Get the object from S3 bucket
    % io:format("Getting object from S3 bucket...~n"),
    % GetInput = #{
    %     <<"bucket">> => <<"my-test-bucket">>,
    %     <<"key">> => <<"test-file.txt">>
    % },
    
    % case s3_client:get_object(Client, GetInput) of
    %     {ok, GetOutput} ->
    %         Body = maps:get(<<"body">>, GetOutput, <<"">>),
    %         ContentType = maps:get(<<"contentType">>, GetOutput, undefined),
    %         ETag = maps:get(<<"eTag">>, GetOutput, undefined),
    %         ContentLength = maps:get(<<"contentLength">>, GetOutput, undefined),
            
    %         io:format("Object retrieved successfully~n"),
    %         io:format("  Content-Type: ~p~n", [ContentType]),
    %         io:format("  ETag: ~p~n", [ETag]),
    %         io:format("  Content-Length: ~p~n", [ContentLength]),
    %         io:format("  Body: ~s~n~n", [Body]);
    %     {error, GetError} ->
    %         io:format("Failed to get object: ~p~n~n", [GetError])
    % end,
    
    %% List buckets (disable retry to see the actual error)
    io:format("Listing buckets...~n"),
    ListInput = #{},
    
    try s3_client:list_buckets(Client, ListInput, #{enable_retry => false}) of
        {ok, ListOutput} ->
            io:format("~nSUCCESS: ListBuckets returned successfully!~n"),
            io:format("Response: ~p~n~n", [ListOutput]),
            %% Extract buckets from S3 XML response structure
            %% Structure: ListAllMyBucketsResult -> Buckets -> Bucket (may be map or list)
            BucketsData = case ListOutput of
                #{<<"ListAllMyBucketsResult">> := #{<<"Buckets">> := #{<<"Bucket">> := B}}} -> B;
                #{<<"ListAllMyBucketsResult">> := #{<<"Buckets">> := B}} -> B;
                #{<<"Buckets">> := #{<<"Bucket">> := B}} -> B;
                #{<<"Buckets">> := B} -> B;
                _ -> []
            end,
            %% Normalize to list (single bucket comes as map, multiple as list)
            BucketList = case BucketsData of
                M when is_map(M) -> [M];
                L when is_list(L) -> L;
                _ -> []
            end,
            io:format("Found ~p bucket(s):~n", [length(BucketList)]),
            lists:foreach(fun(Bucket) ->
                Name = maps:get(<<"Name">>, Bucket, maps:get(<<"name">>, Bucket, <<"unknown">>)),
                io:format("  - ~s~n", [Name])
            end, BucketList),
            io:format("~n");
        {error, {aws_error, StatusCode, Code, Message}} ->
            io:format("AWS Error: ~p ~s - ~s~n", [StatusCode, Code, Message]);
        {error, ListError} ->
            io:format("Error from S3: ~p~n", [ListError])
    catch
        error:Reason:Stacktrace ->
            io:format("Unexpected error: ~p~n", [Reason]),
            io:format("Stacktrace: ~p~n~n", [Stacktrace])
    end,
    
    io:format("=== S3 Client Application Complete ===~n"),
    ok.
