-module(s3_client_types_test).
-include_lib("eunit/include/eunit.hrl").

%%% This test demonstrates how to work with the generated S3 client types
%%% Tests focus on input/output structure validation without making actual HTTP calls

%% Test: GetObject request structure
get_object_request_test() ->
    %% Create a GetObjectRequest map
    Request = #{
        <<"Bucket">> => <<"my-test-bucket">>,
        <<"Key">> => <<"test-file.txt">>
    },
    
    %% Verify we can encode to JSON
    Encoded = jsx:encode(Request),
    ?assert(is_binary(Encoded)),
    
    %% Verify we can decode back
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"my-test-bucket">>, maps:get(<<"Bucket">>, Decoded)),
    ?assertEqual(<<"test-file.txt">>, maps:get(<<"Key">>, Decoded)).

%% Test: GetObject output structure
get_object_output_test() ->
    %% Simulate a GetObjectOutput response
    OutputJson = <<"{\"Body\":\"SGVsbG8sIFdvcmxkIQ==\",\"ContentType\":\"text/plain\",\"LastModified\":\"2024-01-01T12:00:00Z\"}">>,
    
    Output = jsx:decode(OutputJson, [return_maps]),
    
    %% Verify fields are present
    ?assertEqual(<<"SGVsbG8sIFdvcmxkIQ==">>, maps:get(<<"Body">>, Output)),
    ?assertEqual(<<"text/plain">>, maps:get(<<"ContentType">>, Output)),
    ?assertEqual(<<"2024-01-01T12:00:00Z">>, maps:get(<<"LastModified">>, Output)).

%% Test: PutObject request structure
put_object_request_test() ->
    %% Create a PutObjectRequest map
    Request = #{
        <<"Bucket">> => <<"my-test-bucket">>,
        <<"Key">> => <<"test-file.txt">>,
        <<"Body">> => <<"Hello, World!">>,
        <<"ContentType">> => <<"text/plain">>
    },
    
    %% Verify we can encode to JSON
    Encoded = jsx:encode(Request),
    ?assert(is_binary(Encoded)),
    
    %% Verify required fields
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"my-test-bucket">>, maps:get(<<"Bucket">>, Decoded)),
    ?assertEqual(<<"test-file.txt">>, maps:get(<<"Key">>, Decoded)),
    ?assertEqual(<<"Hello, World!">>, maps:get(<<"Body">>, Decoded)).

%% Test: PutObject output structure
put_object_output_test() ->
    %% Simulate a PutObjectOutput response
    OutputJson = <<"{\"ETag\":\"\\\"d41d8cd98f00b204e9800998ecf8427e\\\"\"}">>,
    
    Output = jsx:decode(OutputJson, [return_maps]),
    
    %% Verify ETag is present
    ?assertEqual(<<"\"d41d8cd98f00b204e9800998ecf8427e\"">>, maps:get(<<"ETag">>, Output)).

%% Test: ListBuckets request structure
list_buckets_request_test() ->
    %% ListBucketsRequest has no members
    Request = #{},
    
    %% Verify we can encode empty map
    Encoded = jsx:encode(Request),
    ?assert(is_binary(Encoded)).

%% Test: ListBuckets output structure
list_buckets_output_test() ->
    %% Simulate a ListBucketsOutput response
    OutputJson = <<"{\"Buckets\":[{\"Name\":\"bucket1\",\"CreationDate\":\"2024-01-01T12:00:00Z\"},{\"Name\":\"bucket2\",\"CreationDate\":\"2024-01-02T12:00:00Z\"}]}">>,
    
    Output = jsx:decode(OutputJson, [return_maps]),
    
    %% Verify buckets list is present
    Buckets = maps:get(<<"Buckets">>, Output),
    ?assert(is_list(Buckets)),
    ?assertEqual(2, length(Buckets)),
    
    %% Verify first bucket
    [Bucket1 | _] = Buckets,
    ?assertEqual(<<"bucket1">>, maps:get(<<"Name">>, Bucket1)),
    ?assertEqual(<<"2024-01-01T12:00:00Z">>, maps:get(<<"CreationDate">>, Bucket1)).

%% Test: NoSuchKey error structure
no_such_key_error_test() ->
    %% Simulate a NoSuchKey error
    ErrorJson = <<"{\"message\":\"The specified key does not exist.\"}">>,
    
    Error = jsx:decode(ErrorJson, [return_maps]),
    
    ?assertEqual(<<"The specified key does not exist.">>, maps:get(<<"message">>, Error)).

%% Test: Client creation
client_creation_test() ->
    %% Test creating a new client
    Config = #{endpoint => <<"https://s3.amazonaws.com">>},
    {ok, Client} = s3_client:new(Config),
    ?assert(is_map(Client)),
    ?assertEqual(<<"https://s3.amazonaws.com">>, maps:get(endpoint, Client)).
