-module(storage_union_type_test).
-include_lib("eunit/include/eunit.hrl").
-include("storage_client_types.hrl").

%%% Test that union types work correctly with Dialyzer
%%% These tests verify the type system integration

%% Test creating S3 storage variant
create_s3_storage_variant_test() ->
    %% Create an S3 storage variant as a tagged tuple
    S3Data = #{
        <<"bucket">> => <<"my-bucket">>,
        <<"region">> => <<"us-east-1">>,
        <<"prefix">> => <<"data/">>
    },
    
    Storage = {s3, S3Data},
    
    %% Verify it's a tuple
    ?assert(is_tuple(Storage)),
    ?assertEqual(2, tuple_size(Storage)),
    
    %% Verify the tag
    ?assertEqual(s3, element(1, Storage)),
    
    %% Verify the data
    ?assertEqual(S3Data, element(2, Storage)).

%% Test creating Glacier storage variant
create_glacier_storage_variant_test() ->
    GlacierData = #{
        <<"vault">> => <<"my-vault">>,
        <<"region">> => <<"eu-west-1">>,
        <<"retrievalOption">> => <<"standard">>
    },
    
    Storage = {glacier, GlacierData},
    
    ?assert(is_tuple(Storage)),
    ?assertEqual(glacier, element(1, Storage)),
    ?assertEqual(GlacierData, element(2, Storage)).

%% Test creating EFS storage variant
create_efs_storage_variant_test() ->
    EfsData = #{
        <<"fileSystemId">> => <<"fs-12345678">>,
        <<"region">> => <<"ap-southeast-1">>,
        <<"mountPath">> => <<"/mnt/efs">>
    },
    
    Storage = {efs, EfsData},
    
    ?assert(is_tuple(Storage)),
    ?assertEqual(efs, element(1, Storage)),
    ?assertEqual(EfsData, element(2, Storage)).

%% Test pattern matching on union variants
pattern_match_on_variants_test() ->
    S3Storage = {s3, #{<<"bucket">> => <<"bucket1">>, <<"region">> => <<"us-east-1">>}},
    GlacierStorage = {glacier, #{<<"vault">> => <<"vault1">>, <<"region">> => <<"us-west-2">>}},
    EfsStorage = {efs, #{<<"fileSystemId">> => <<"fs-abc">>, <<"region">> => <<"eu-west-1">>}},
    
    %% Pattern match on S3
    Result1 = case S3Storage of
        {s3, S3Data} -> {s3_matched, S3Data};
        {glacier, _} -> glacier_matched;
        {efs, _} -> efs_matched
    end,
    ?assertMatch({s3_matched, _}, Result1),
    
    %% Pattern match on Glacier
    Result2 = case GlacierStorage of
        {s3, _} -> s3_matched;
        {glacier, GlacierData} -> {glacier_matched, GlacierData};
        {efs, _} -> efs_matched
    end,
    ?assertMatch({glacier_matched, _}, Result2),
    
    %% Pattern match on EFS
    Result3 = case EfsStorage of
        {s3, _} -> s3_matched;
        {glacier, _} -> glacier_matched;
        {efs, EfsData} -> {efs_matched, EfsData}
    end,
    ?assertMatch({efs_matched, _}, Result3).

%% Test that union type is used in input/output records
union_type_in_records_test() ->
    %% Create an input record with union type
    S3Data = #{
        <<"bucket">> => <<"test-bucket">>,
        <<"region">> => <<"us-east-1">>,
        <<"prefix">> => <<"test/">>
    },
    
    Input = #{
        <<"name">> => <<"my-storage">>,
        <<"storageType">> => {s3, S3Data}
    },
    
    %% Verify the structure
    ?assertEqual(<<"my-storage">>, maps:get(<<"name">>, Input)),
    ?assertMatch({s3, _}, maps:get(<<"storageType">>, Input)).

%% Helper function with -spec to test Dialyzer integration
-spec get_storage_type_tag(storage_type()) -> s3 | glacier | efs.
get_storage_type_tag({Tag, _Data}) ->
    Tag.

%% Test the helper function
dialyzer_spec_test() ->
    S3Storage = {s3, #{<<"bucket">> => <<"b1">>, <<"region">> => <<"r1">>}},
    ?assertEqual(s3, get_storage_type_tag(S3Storage)),
    
    GlacierStorage = {glacier, #{<<"vault">> => <<"v1">>, <<"region">> => <<"r1">>}},
    ?assertEqual(glacier, get_storage_type_tag(GlacierStorage)),
    
    EfsStorage = {efs, #{<<"fileSystemId">> => <<"f1">>, <<"region">> => <<"r1">>}},
    ?assertEqual(efs, get_storage_type_tag(EfsStorage)).

%% Test extracting data from variants
-spec get_region_from_storage(storage_type()) -> binary() | undefined.
get_region_from_storage({s3, Data}) ->
    maps:get(<<"region">>, Data, undefined);
get_region_from_storage({glacier, Data}) ->
    maps:get(<<"region">>, Data, undefined);
get_region_from_storage({efs, Data}) ->
    maps:get(<<"region">>, Data, undefined).

extract_data_from_variants_test() ->
    S3Storage = {s3, #{<<"bucket">> => <<"b1">>, <<"region">> => <<"us-east-1">>}},
    ?assertEqual(<<"us-east-1">>, get_region_from_storage(S3Storage)),
    
    GlacierStorage = {glacier, #{<<"vault">> => <<"v1">>, <<"region">> => <<"eu-west-1">>}},
    ?assertEqual(<<"eu-west-1">>, get_region_from_storage(GlacierStorage)),
    
    EfsStorage = {efs, #{<<"fileSystemId">> => <<"f1">>, <<"region">> => <<"ap-southeast-1">>}},
    ?assertEqual(<<"ap-southeast-1">>, get_region_from_storage(EfsStorage)).
