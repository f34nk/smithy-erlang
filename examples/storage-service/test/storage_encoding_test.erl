-module(storage_encoding_test).
-include_lib("eunit/include/eunit.hrl").
-include("storage_client_types.hrl").

%%% Test union encoding functions
%%% These tests verify that tagged tuples are correctly encoded to JSON

%% Test encoding S3 storage variant
encode_s3_variant_test() ->
    S3Data = #s3storage{
        bucket = <<"my-bucket">>,
        region = <<"us-east-1">>,
        prefix = <<"data/">>
    },
    
    Variant = {s3, S3Data},
    Encoded = storage_client_types:encode_storage_type(Variant),
    
    %% Verify it's a map
    ?assert(is_map(Encoded)),
    
    %% Verify discriminator key is "s3"
    ?assert(maps:is_key(<<"s3">>, Encoded)),
    
    %% Verify data is preserved
    ?assertEqual(S3Data, maps:get(<<"s3">>, Encoded)).

%% Test encoding Glacier storage variant
encode_glacier_variant_test() ->
    GlacierData = #glacier_storage{
        vault = <<"my-vault">>,
        region = <<"eu-west-1">>,
        retrieval_option = <<"standard">>
    },
    
    Variant = {glacier, GlacierData},
    Encoded = storage_client_types:encode_storage_type(Variant),
    
    ?assert(is_map(Encoded)),
    ?assert(maps:is_key(<<"glacier">>, Encoded)),
    ?assertEqual(GlacierData, maps:get(<<"glacier">>, Encoded)).

%% Test encoding EFS storage variant
encode_efs_variant_test() ->
    EfsData = #efs_storage{
        file_system_id = <<"fs-12345678">>,
        region = <<"ap-southeast-1">>,
        mount_path = <<"/mnt/efs">>
    },
    
    Variant = {efs, EfsData},
    Encoded = storage_client_types:encode_storage_type(Variant),
    
    ?assert(is_map(Encoded)),
    ?assert(maps:is_key(<<"efs">>, Encoded)),
    ?assertEqual(EfsData, maps:get(<<"efs">>, Encoded)).

%% Test encoding unknown variant (forward compatibility)
encode_unknown_variant_test() ->
    UnknownData = #{
        <<"someField">> => <<"someValue">>,
        <<"otherField">> => 123
    },
    
    Variant = {unknown, UnknownData},
    Encoded = storage_client_types:encode_storage_type(Variant),
    
    ?assert(is_map(Encoded)),
    ?assert(maps:is_key(<<"unknown">>, Encoded)),
    ?assertEqual(UnknownData, maps:get(<<"unknown">>, Encoded)).

%% Test encoding with empty data
encode_empty_data_test() ->
    EmptyData = #{},
    
    Variant = {s3, EmptyData},
    Encoded = storage_client_types:encode_storage_type(Variant),
    
    ?assert(is_map(Encoded)),
    ?assertEqual(EmptyData, maps:get(<<"s3">>, Encoded)).

%% Test that encoded map has exactly one key (the discriminator)
encode_single_discriminator_test() ->
    S3Data = #s3storage{bucket = <<"test">>},
    Encoded = storage_client_types:encode_storage_type({s3, S3Data}),
    
    %% Encoded map should have exactly one key
    ?assertEqual(1, maps:size(Encoded)).

%% Test encoding preserves nested structure
encode_nested_structure_test() ->
    NestedData = #{
        <<"bucket">> => <<"my-bucket">>,
        <<"region">> => <<"us-east-1">>,
        <<"prefix">> => <<"data/">>,
        <<"nested">> => #{
            <<"key1">> => <<"value1">>,
            <<"key2">> => <<"value2">>
        }
    },
    
    Variant = {s3, NestedData},
    Encoded = storage_client_types:encode_storage_type(Variant),
    
    DecodedData = maps:get(<<"s3">>, Encoded),
    ?assertEqual(NestedData, DecodedData),
    
    %% Verify nested map is preserved
    NestedMap = maps:get(<<"nested">>, DecodedData),
    ?assert(is_map(NestedMap)),
    ?assertEqual(<<"value1">>, maps:get(<<"key1">>, NestedMap)).

%% Test encoding with binary values
encode_binary_values_test() ->
    Data = #{
        <<"bucket">> => <<"test-bucket-äöü"/utf8>>,
        <<"region">> => <<"üs-wëst-1"/utf8>>
    },
    
    Encoded = storage_client_types:encode_storage_type({s3, Data}),
    Result = maps:get(<<"s3">>, Encoded),
    
    ?assertEqual(<<"test-bucket-äöü"/utf8>>, maps:get(<<"bucket">>, Result)),
    ?assertEqual(<<"üs-wëst-1"/utf8>>, maps:get(<<"region">>, Result)).

%% Test encoding different variants produces different discriminators
encode_different_discriminators_test() ->
    S3Encoded = storage_client_types:encode_storage_type({s3, #{}}),
    GlacierEncoded = storage_client_types:encode_storage_type({glacier, #{}}),
    EfsEncoded = storage_client_types:encode_storage_type({efs, #{}}),
    
    %% Each should have a different discriminator
    ?assert(maps:is_key(<<"s3">>, S3Encoded)),
    ?assertNot(maps:is_key(<<"s3">>, GlacierEncoded)),
    ?assertNot(maps:is_key(<<"s3">>, EfsEncoded)),
    
    ?assertNot(maps:is_key(<<"glacier">>, S3Encoded)),
    ?assert(maps:is_key(<<"glacier">>, GlacierEncoded)),
    ?assertNot(maps:is_key(<<"glacier">>, EfsEncoded)),
    
    ?assertNot(maps:is_key(<<"efs">>, S3Encoded)),
    ?assertNot(maps:is_key(<<"efs">>, GlacierEncoded)),
    ?assert(maps:is_key(<<"efs">>, EfsEncoded)).

%% Test encoding can be used in a list of variants
encode_variant_list_test() ->
    Variants = [
        {s3, #{<<"bucket">> => <<"b1">>}},
        {glacier, #{<<"vault">> => <<"v1">>}},
        {efs, #{<<"fileSystemId">> => <<"fs1">>}}
    ],
    
    Encoded = [storage_client_types:encode_storage_type(V) || V <- Variants],
    
    ?assertEqual(3, length(Encoded)),
    
    [S3Enc, GlacierEnc, EfsEnc] = Encoded,
    ?assert(maps:is_key(<<"s3">>, S3Enc)),
    ?assert(maps:is_key(<<"glacier">>, GlacierEnc)),
    ?assert(maps:is_key(<<"efs">>, EfsEnc)).

%% Test encoding works with pattern matching
encode_with_pattern_match_test() ->
    HandleVariant = fun(Variant) ->
        case Variant of
            {s3, _} = V -> storage_client_types:encode_storage_type(V);
            {glacier, _} = V -> storage_client_types:encode_storage_type(V);
            {efs, _} = V -> storage_client_types:encode_storage_type(V)
        end
    end,
    
    S3Variant = {s3, #{<<"bucket">> => <<"test">>}},
    Encoded = HandleVariant(S3Variant),
    
    ?assert(maps:is_key(<<"s3">>, Encoded)).
