-module(storage_enum_encoding_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Suite for Enum Encoding Functions
%%% Note: Types and helpers are now in storage_client module
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: encode all enum values
%%--------------------------------------------------------------------
encode_expedited_test() ->
    Result = storage_client:encode_glacier_retrieval_option(expedited),
    ?assertEqual(<<"expedited">>, Result).

encode_standard_test() ->
    Result = storage_client:encode_glacier_retrieval_option(standard),
    ?assertEqual(<<"standard">>, Result).

encode_bulk_test() ->
    Result = storage_client:encode_glacier_retrieval_option(bulk),
    ?assertEqual(<<"bulk">>, Result).

%%--------------------------------------------------------------------
%% Test: return type is binary
%%--------------------------------------------------------------------
encode_returns_binary_test() ->
    Result = storage_client:encode_glacier_retrieval_option(expedited),
    ?assert(is_binary(Result)).

%%--------------------------------------------------------------------
%% Test: encoding all values returns distinct binaries
%%--------------------------------------------------------------------
encode_distinct_values_test() ->
    Expedited = storage_client:encode_glacier_retrieval_option(expedited),
    Standard = storage_client:encode_glacier_retrieval_option(standard),
    Bulk = storage_client:encode_glacier_retrieval_option(bulk),
    
    %% All values should be different
    ?assertNotEqual(Expedited, Standard),
    ?assertNotEqual(Expedited, Bulk),
    ?assertNotEqual(Standard, Bulk).

%%--------------------------------------------------------------------
%% Test: enum encoding in a structure
%%--------------------------------------------------------------------
encode_enum_in_structure_test() ->
    %% Create a Glacier storage config with retrieval option
    GlacierStorage = #{
        <<"vault">> => <<"my-vault">>,
        <<"region">> => <<"us-west-2">>,
        <<"retrievalOption">> => storage_client:encode_glacier_retrieval_option(expedited)
    },
    
    %% Verify the encoded value is present
    ?assertEqual(<<"expedited">>, maps:get(<<"retrievalOption">>, GlacierStorage)).

%%--------------------------------------------------------------------
%% Test: enum encoding in a union
%%--------------------------------------------------------------------
encode_enum_in_union_test() ->
    %% Create a Glacier storage union variant
    GlacierData = #{
        <<"vault">> => <<"my-vault">>,
        <<"region">> => <<"us-west-2">>,
        <<"retrievalOption">> => storage_client:encode_glacier_retrieval_option(standard)
    },
    StorageType = {glacier, GlacierData},
    
    %% Encode the union
    EncodedUnion = storage_client:encode_storage_type(StorageType),
    
    %% Verify the enum is encoded within the union
    GlacierInUnion = maps:get(<<"glacier">>, EncodedUnion),
    ?assertEqual(<<"standard">>, maps:get(<<"retrievalOption">>, GlacierInUnion)).

%%--------------------------------------------------------------------
%% Test: enum encoding with pattern matching
%%--------------------------------------------------------------------
encode_with_pattern_match_test() ->
    Options = [expedited, standard, bulk],
    Expected = [<<"expedited">>, <<"standard">>, <<"bulk">>],
    
    Encoded = lists:map(
        fun(Opt) -> storage_client:encode_glacier_retrieval_option(Opt) end,
        Options
    ),
    
    ?assertEqual(Expected, Encoded).

%%--------------------------------------------------------------------
%% Test: enum encoding in case expression
%%--------------------------------------------------------------------
encode_in_case_expression_test() ->
    ChosenOption = standard,
    
    Result = case ChosenOption of
        expedited -> storage_client:encode_glacier_retrieval_option(expedited);
        standard -> storage_client:encode_glacier_retrieval_option(standard);
        bulk -> storage_client:encode_glacier_retrieval_option(bulk)
    end,
    
    ?assertEqual(<<"standard">>, Result).

%%--------------------------------------------------------------------
%% Test: enum encoding in list comprehension
%%--------------------------------------------------------------------
encode_list_comprehension_test() ->
    Options = [expedited, standard, bulk, expedited, standard],
    
    EncodedOptions = [
        storage_client:encode_glacier_retrieval_option(Opt) 
        || Opt <- Options
    ],
    
    Expected = [
        <<"expedited">>, 
        <<"standard">>, 
        <<"bulk">>, 
        <<"expedited">>, 
        <<"standard">>
    ],
    
    ?assertEqual(Expected, EncodedOptions).

%%--------------------------------------------------------------------
%% Test: enum encoding with function reference
%%--------------------------------------------------------------------
encode_function_reference_test() ->
    EncodeFunc = fun storage_client:encode_glacier_retrieval_option/1,
    
    Result = EncodeFunc(bulk),
    ?assertEqual(<<"bulk">>, Result).

%%--------------------------------------------------------------------
%% Test: enum encoding is idempotent when used with binaries
%%--------------------------------------------------------------------
encode_idempotent_test() ->
    Encoded1 = storage_client:encode_glacier_retrieval_option(expedited),
    Encoded2 = storage_client:encode_glacier_retrieval_option(expedited),
    
    ?assertEqual(Encoded1, Encoded2),
    ?assert(Encoded1 =:= Encoded2).  %% Exact match

%%--------------------------------------------------------------------
%% Test: enum encoding preserves wire format case
%%--------------------------------------------------------------------
encode_preserves_case_test() ->
    %% The wire format should be lowercase, not EXPEDITED
    Result = storage_client:encode_glacier_retrieval_option(expedited),
    
    ?assertEqual(<<"expedited">>, Result),
    ?assertNotEqual(<<"EXPEDITED">>, Result),
    ?assertNotEqual(<<"Expedited">>, Result).

%%--------------------------------------------------------------------
%% Test: encoding multiple enums for JSON request
%%--------------------------------------------------------------------
encode_for_json_request_test() ->
    %% Simulate building a JSON request with enum
    Request = #{
        <<"vault">> => <<"production-vault">>,
        <<"region">> => <<"eu-west-1">>,
        <<"retrievalOption">> => storage_client:encode_glacier_retrieval_option(expedited)
    },
    
    %% Verify it can be encoded to JSON
    Json = jsx:encode(Request),
    
    %% Verify the JSON contains the enum value
    ?assert(is_binary(Json)),
    ?assert(binary:match(Json, <<"expedited">>) =/= nomatch).

%%--------------------------------------------------------------------
%% Test: enum encoding in API request simulation
%%--------------------------------------------------------------------
encode_in_api_request_test() ->
    %% Simulate creating a Glacier storage location
    CreateRequest = #{
        <<"name">> => <<"Archive Storage">>,
        <<"storageType">> => storage_client:encode_storage_type({
            glacier,
            #{
                <<"vault">> => <<"archive-vault">>,
                <<"region">> => <<"us-east-1">>,
                <<"retrievalOption">> => storage_client:encode_glacier_retrieval_option(bulk)
            }
        })
    },
    
    %% Verify the nested enum encoding
    StorageType = maps:get(<<"storageType">>, CreateRequest),
    GlacierConfig = maps:get(<<"glacier">>, StorageType),
    RetrievalOption = maps:get(<<"retrievalOption">>, GlacierConfig),
    
    ?assertEqual(<<"bulk">>, RetrievalOption).

%%--------------------------------------------------------------------
%% Test: enum encoding comparison
%%--------------------------------------------------------------------
encode_comparison_test() ->
    EncodedExpedited = storage_client:encode_glacier_retrieval_option(expedited),
    EncodedStandard = storage_client:encode_glacier_retrieval_option(standard),
    
    %% Binary comparison
    ?assert(EncodedExpedited < EncodedStandard),  %% "expedited" < "standard" lexically
    
    %% Pattern matching
    case EncodedExpedited of
        <<"expedited">> -> ok;
        _ -> ?assert(false)
    end.
