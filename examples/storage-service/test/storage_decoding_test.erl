-module(storage_decoding_test).

-include_lib("eunit/include/eunit.hrl").

%% Ignore dialyzer warnings
-dialyzer([no_return, no_match, no_contracts]).

%%% ============================================================================
%%% Union Decoding Tests
%%% ============================================================================

%%% ----------------------------------------------------------------------------
%%% Basic Decoding Tests
%%% ----------------------------------------------------------------------------

decode_s3_variant_test() ->
    %% Test decoding S3 variant from JSON
    Json = #{<<"s3">> => #{
        <<"bucket">> => <<"my-bucket">>,
        <<"region">> => <<"us-east-1">>,
        <<"prefix">> => <<"data/">>
    }},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({s3, _}, Result),
    {s3, Data} = Result,
    ?assertEqual(<<"my-bucket">>, maps:get(<<"bucket">>, Data)),
    ?assertEqual(<<"us-east-1">>, maps:get(<<"region">>, Data)),
    ?assertEqual(<<"data/">>, maps:get(<<"prefix">>, Data)).

decode_glacier_variant_test() ->
    %% Test decoding Glacier variant from JSON
    Json = #{<<"glacier">> => #{
        <<"vault">> => <<"my-vault">>,
        <<"region">> => <<"us-west-2">>,
        <<"retrievalOption">> => <<"expedited">>
    }},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({glacier, _}, Result),
    {glacier, Data} = Result,
    ?assertEqual(<<"my-vault">>, maps:get(<<"vault">>, Data)),
    ?assertEqual(<<"us-west-2">>, maps:get(<<"region">>, Data)),
    ?assertEqual(<<"expedited">>, maps:get(<<"retrievalOption">>, Data)).

decode_efs_variant_test() ->
    %% Test decoding EFS variant from JSON
    Json = #{<<"efs">> => #{
        <<"fileSystemId">> => <<"fs-12345678">>,
        <<"region">> => <<"eu-west-1">>,
        <<"mountPath">> => <<"/mnt/efs">>
    }},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({efs, _}, Result),
    {efs, Data} = Result,
    ?assertEqual(<<"fs-12345678">>, maps:get(<<"fileSystemId">>, Data)),
    ?assertEqual(<<"eu-west-1">>, maps:get(<<"region">>, Data)),
    ?assertEqual(<<"/mnt/efs">>, maps:get(<<"mountPath">>, Data)).

%%% ----------------------------------------------------------------------------
%%% Unknown Variant Tests (Forward Compatibility)
%%% ----------------------------------------------------------------------------

decode_unknown_variant_future_type_test() ->
    %% Test decoding a future storage type we don't know about yet
    Json = #{<<"redshift">> => #{
        <<"cluster">> => <<"my-cluster">>,
        <<"database">> => <<"analytics">>
    }},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({unknown, _}, Result),
    {unknown, Data} = Result,
    ?assertEqual(Json, Data).

decode_unknown_variant_multiple_keys_test() ->
    %% Test unknown variant with multiple keys (invalid union, but should handle gracefully)
    Json = #{
        <<"s3">> => #{<<"bucket">> => <<"test">>},
        <<"glacier">> => #{<<"vault">> => <<"test">>}
    },
    
    Result = storage_client:decode_storage_type(Json),
    
    %% Should match one variant (first match wins) or unknown
    ?assert(is_tuple(Result)),
    ?assertEqual(2, tuple_size(Result)).

decode_unknown_variant_empty_map_test() ->
    %% Test decoding empty map as unknown variant
    Json = #{},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({unknown, _}, Result),
    {unknown, Data} = Result,
    ?assertEqual(#{}, Data).

decode_unknown_variant_complex_data_test() ->
    %% Test unknown variant with complex nested data
    Json = #{<<"dynamodb">> => #{
        <<"table">> => <<"Users">>,
        <<"indexes">> => [
            #{<<"name">> => <<"EmailIndex">>, <<"type">> => <<"GSI">>},
            #{<<"name">> => <<"DateIndex">>, <<"type">> => <<"LSI">>}
        ],
        <<"capacity">> => #{
            <<"read">> => 10,
            <<"write">> => 5
        }
    }},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({unknown, _}, Result),
    {unknown, Data} = Result,
    ?assertEqual(Json, Data).

%%% ----------------------------------------------------------------------------
%%% Empty Data Tests
%%% ----------------------------------------------------------------------------

decode_s3_with_empty_data_test() ->
    %% Test decoding S3 variant with empty data
    Json = #{<<"s3">> => #{}},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({s3, _}, Result),
    {s3, Data} = Result,
    ?assertEqual(#{}, Data).

decode_glacier_with_minimal_data_test() ->
    %% Test decoding Glacier variant with minimal required fields
    Json = #{<<"glacier">> => #{<<"vault">> => <<"vault1">>}},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({glacier, _}, Result),
    {glacier, Data} = Result,
    ?assertEqual(<<"vault1">>, maps:get(<<"vault">>, Data)).

%%% ----------------------------------------------------------------------------
%%% Roundtrip Tests (Encode → Decode)
%%% ----------------------------------------------------------------------------

roundtrip_s3_test() ->
    %% Test encoding and then decoding S3 variant
    Original = {s3, #{
        <<"bucket">> => <<"test-bucket">>,
        <<"region">> => <<"ap-south-1">>,
        <<"prefix">> => <<"backup/">>
    }},
    
    Encoded = storage_client:encode_storage_type(Original),
    Decoded = storage_client:decode_storage_type(Encoded),
    
    ?assertEqual(Original, Decoded).

roundtrip_glacier_test() ->
    %% Test encoding and then decoding Glacier variant
    Original = {glacier, #{
        <<"vault">> => <<"archive-vault">>,
        <<"region">> => <<"us-east-1">>,
        <<"retrievalOption">> => <<"standard">>
    }},
    
    Encoded = storage_client:encode_storage_type(Original),
    Decoded = storage_client:decode_storage_type(Encoded),
    
    ?assertEqual(Original, Decoded).

roundtrip_efs_test() ->
    %% Test encoding and then decoding EFS variant
    Original = {efs, #{
        <<"fileSystemId">> => <<"fs-abcd1234">>,
        <<"region">> => <<"eu-central-1">>,
        <<"mountPath">> => <<"/data">>
    }},
    
    Encoded = storage_client:encode_storage_type(Original),
    Decoded = storage_client:decode_storage_type(Encoded),
    
    ?assertEqual(Original, Decoded).

roundtrip_unknown_test() ->
    %% Test encoding and then decoding unknown variant
    %% Note: Unknown variants are wrapped in <<"unknown">> key during encoding,
    %% so the roundtrip produces a nested structure
    Original = {unknown, #{<<"custom">> => <<"value">>}},
    
    Encoded = storage_client:encode_storage_type(Original),
    %% Encoded is: #{<<"unknown">> => #{<<"custom">> => <<"value">>}}
    
    Decoded = storage_client:decode_storage_type(Encoded),
    %% Decoded is: {unknown, #{<<"unknown">> => #{<<"custom">> => <<"value">>}}}
    
    %% Verify the structure (not identical due to wrapping)
    ?assertMatch({unknown, _}, Decoded),
    {unknown, DecodedData} = Decoded,
    ?assertEqual(Encoded, DecodedData).

%%% ----------------------------------------------------------------------------
%%% Special Cases
%%% ----------------------------------------------------------------------------

decode_with_binary_values_test() ->
    %% Test decoding with various binary value types
    Json = #{<<"s3">> => #{
        <<"bucket">> => <<"bucket-with-dashes-and-123">>,
        <<"region">> => <<"us-west-2">>,
        <<"prefix">> => <<"path/to/data/">>
    }},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({s3, _}, Result),
    {s3, Data} = Result,
    ?assert(is_binary(maps:get(<<"bucket">>, Data))),
    ?assert(is_binary(maps:get(<<"region">>, Data))),
    ?assert(is_binary(maps:get(<<"prefix">>, Data))).

decode_with_unicode_test() ->
    %% Test decoding with Unicode strings
    Json = #{<<"glacier">> => #{
        <<"vault">> => <<"vault-日本語"/utf8>>,
        <<"region">> => <<"ap-northeast-1">>,
        <<"retrievalOption">> => <<"bulk">>
    }},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({glacier, _}, Result),
    {glacier, Data} = Result,
    ?assertEqual(<<"vault-日本語"/utf8>>, maps:get(<<"vault">>, Data)).

decode_with_nested_structures_test() ->
    %% Test decoding with nested map structures (if future variants support it)
    Json = #{<<"s3">> => #{
        <<"bucket">> => <<"test">>,
        <<"region">> => <<"us-east-1">>,
        <<"config">> => #{
            <<"encryption">> => <<"AES256">>,
            <<"versioning">> => true
        }
    }},
    
    Result = storage_client:decode_storage_type(Json),
    
    ?assertMatch({s3, _}, Result),
    {s3, Data} = Result,
    Config = maps:get(<<"config">>, Data),
    ?assertEqual(<<"AES256">>, maps:get(<<"encryption">>, Config)),
    ?assertEqual(true, maps:get(<<"versioning">>, Config)).

%%% ----------------------------------------------------------------------------
%%% Pattern Matching Tests
%%% ----------------------------------------------------------------------------

decode_and_pattern_match_test() ->
    %% Test that decoded values can be pattern matched
    Json = #{<<"efs">> => #{
        <<"fileSystemId">> => <<"fs-xyz">>,
        <<"region">> => <<"us-west-1">>,
        <<"mountPath">> => <<"/mnt">>
    }},
    
    Result = storage_client:decode_storage_type(Json),
    
    %% Pattern match on the result
    {efs, EfsData} = Result,
    ?assertEqual(<<"fs-xyz">>, maps:get(<<"fileSystemId">>, EfsData)).

decode_multiple_and_distinguish_test() ->
    %% Test decoding multiple variants and distinguishing them
    S3Json = #{<<"s3">> => #{<<"bucket">> => <<"b1">>}},
    GlacierJson = #{<<"glacier">> => #{<<"vault">> => <<"v1">>}},
    EfsJson = #{<<"efs">> => #{<<"fileSystemId">> => <<"fs1">>}},
    
    S3Result = storage_client:decode_storage_type(S3Json),
    GlacierResult = storage_client:decode_storage_type(GlacierJson),
    EfsResult = storage_client:decode_storage_type(EfsJson),
    
    ?assertMatch({s3, _}, S3Result),
    ?assertMatch({glacier, _}, GlacierResult),
    ?assertMatch({efs, _}, EfsResult),
    
    %% Verify they're different
    ?assertNotEqual(S3Result, GlacierResult),
    ?assertNotEqual(GlacierResult, EfsResult),
    ?assertNotEqual(S3Result, EfsResult).
