-module(storage_enum_decoding_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Suite for Enum Decoding Functions with Validation
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: decode all valid enum values
%%--------------------------------------------------------------------
decode_expedited_test() ->
    Result = storage_client:decode_glacier_retrieval_option(<<"expedited">>),
    ?assertEqual({ok, expedited}, Result).

decode_standard_test() ->
    Result = storage_client:decode_glacier_retrieval_option(<<"standard">>),
    ?assertEqual({ok, standard}, Result).

decode_bulk_test() ->
    Result = storage_client:decode_glacier_retrieval_option(<<"bulk">>),
    ?assertEqual({ok, bulk}, Result).

%%--------------------------------------------------------------------
%% Test: decode returns proper tuple format
%%--------------------------------------------------------------------
decode_returns_ok_tuple_test() ->
    {ok, Atom} = storage_client:decode_glacier_retrieval_option(<<"expedited">>),
    ?assert(is_atom(Atom)),
    ?assertEqual(expedited, Atom).

%%--------------------------------------------------------------------
%% Test: decode invalid values returns error
%%--------------------------------------------------------------------
decode_invalid_value_test() ->
    Result = storage_client:decode_glacier_retrieval_option(<<"invalid">>),
    ?assertMatch({error, {invalid_enum_value, _}}, Result).

decode_invalid_uppercase_test() ->
    %% Wire format is lowercase, not uppercase
    Result = storage_client:decode_glacier_retrieval_option(<<"EXPEDITED">>),
    ?assertMatch({error, {invalid_enum_value, <<"EXPEDITED">>}}, Result).

decode_invalid_capitalized_test() ->
    Result = storage_client:decode_glacier_retrieval_option(<<"Expedited">>),
    ?assertMatch({error, {invalid_enum_value, <<"Expedited">>}}, Result).

decode_empty_binary_test() ->
    Result = storage_client:decode_glacier_retrieval_option(<<"">>),
    ?assertMatch({error, {invalid_enum_value, <<"">>}}, Result).

decode_numeric_string_test() ->
    Result = storage_client:decode_glacier_retrieval_option(<<"123">>),
    ?assertMatch({error, {invalid_enum_value, <<"123">>}}, Result).

decode_special_chars_test() ->
    Result = storage_client:decode_glacier_retrieval_option(<<"exp@dited">>),
    ?assertMatch({error, {invalid_enum_value, <<"exp@dited">>}}, Result).

%%--------------------------------------------------------------------
%% Test: error tuple contains the invalid value
%%--------------------------------------------------------------------
decode_error_contains_value_test() ->
    InvalidValue = <<"unknown_option">>,
    {error, {invalid_enum_value, ReturnedValue}} = 
        storage_client:decode_glacier_retrieval_option(InvalidValue),
    ?assertEqual(InvalidValue, ReturnedValue).

%%--------------------------------------------------------------------
%% Test: decode all values and verify distinct atoms
%%--------------------------------------------------------------------
decode_distinct_atoms_test() ->
    {ok, Expedited} = storage_client:decode_glacier_retrieval_option(<<"expedited">>),
    {ok, Standard} = storage_client:decode_glacier_retrieval_option(<<"standard">>),
    {ok, Bulk} = storage_client:decode_glacier_retrieval_option(<<"bulk">>),
    
    %% All atoms should be different
    ?assertNotEqual(Expedited, Standard),
    ?assertNotEqual(Expedited, Bulk),
    ?assertNotEqual(Standard, Bulk).

%%--------------------------------------------------------------------
%% Test: decode in case expression with pattern matching
%%--------------------------------------------------------------------
decode_in_case_expression_test() ->
    Input = <<"standard">>,
    
    Result = case storage_client:decode_glacier_retrieval_option(Input) of
        {ok, expedited} -> fast;
        {ok, standard} -> normal;
        {ok, bulk} -> slow;
        {error, _} -> invalid
    end,
    
    ?assertEqual(normal, Result).

decode_error_in_case_expression_test() ->
    Input = <<"invalid">>,
    
    Result = case storage_client:decode_glacier_retrieval_option(Input) of
        {ok, _} -> valid;
        {error, _} -> invalid
    end,
    
    ?assertEqual(invalid, Result).

%%--------------------------------------------------------------------
%% Test: roundtrip encoding and decoding
%%--------------------------------------------------------------------
roundtrip_expedited_test() ->
    Original = expedited,
    Encoded = storage_client:encode_glacier_retrieval_option(Original),
    {ok, Decoded} = storage_client:decode_glacier_retrieval_option(Encoded),
    ?assertEqual(Original, Decoded).

roundtrip_standard_test() ->
    Original = standard,
    Encoded = storage_client:encode_glacier_retrieval_option(Original),
    {ok, Decoded} = storage_client:decode_glacier_retrieval_option(Encoded),
    ?assertEqual(Original, Decoded).

roundtrip_bulk_test() ->
    Original = bulk,
    Encoded = storage_client:encode_glacier_retrieval_option(Original),
    {ok, Decoded} = storage_client:decode_glacier_retrieval_option(Encoded),
    ?assertEqual(Original, Decoded).

roundtrip_all_values_test() ->
    AllValues = [expedited, standard, bulk],
    
    Results = lists:map(fun(Value) ->
        Encoded = storage_client:encode_glacier_retrieval_option(Value),
        {ok, Decoded} = storage_client:decode_glacier_retrieval_option(Encoded),
        Value =:= Decoded
    end, AllValues),
    
    ?assert(lists:all(fun(X) -> X end, Results)).

%%--------------------------------------------------------------------
%% Test: decode from JSON response simulation
%%--------------------------------------------------------------------
decode_from_json_response_test() ->
    %% Simulate a JSON response containing an enum
    JsonResponse = #{
        <<"vault">> => <<"production-vault">>,
        <<"region">> => <<"eu-west-1">>,
        <<"retrievalOption">> => <<"expedited">>
    },
    
    %% Extract and decode the enum
    RetrievalBinary = maps:get(<<"retrievalOption">>, JsonResponse),
    {ok, RetrievalOption} = storage_client:decode_glacier_retrieval_option(RetrievalBinary),
    
    ?assertEqual(expedited, RetrievalOption).

decode_invalid_from_json_test() ->
    %% Simulate a JSON response with invalid enum value
    JsonResponse = #{
        <<"retrievalOption">> => <<"ultra_fast">>
    },
    
    RetrievalBinary = maps:get(<<"retrievalOption">>, JsonResponse),
    Result = storage_client:decode_glacier_retrieval_option(RetrievalBinary),
    
    ?assertMatch({error, {invalid_enum_value, <<"ultra_fast">>}}, Result).

%%--------------------------------------------------------------------
%% Test: decode and use in structure
%%--------------------------------------------------------------------
decode_and_use_in_structure_test() ->
    %% Decode enum from wire format
    {ok, RetrievalOption} = storage_client:decode_glacier_retrieval_option(<<"bulk">>),
    
    %% Use in structure
    GlacierStorage = #{
        <<"vault">> => <<"archive-vault">>,
        <<"region">> => <<"us-east-1">>,
        <<"retrievalOption">> => RetrievalOption
    },
    
    %% Verify it's stored correctly
    ?assertEqual(bulk, maps:get(<<"retrievalOption">>, GlacierStorage)).

%%--------------------------------------------------------------------
%% Test: decode list of enum values
%%--------------------------------------------------------------------
decode_list_of_enums_test() ->
    Binaries = [<<"expedited">>, <<"standard">>, <<"bulk">>],
    
    Results = lists:map(
        fun(Bin) -> 
            storage_client:decode_glacier_retrieval_option(Bin) 
        end,
        Binaries
    ),
    
    Expected = [{ok, expedited}, {ok, standard}, {ok, bulk}],
    ?assertEqual(Expected, Results).

decode_list_with_invalid_test() ->
    Binaries = [<<"expedited">>, <<"invalid">>, <<"bulk">>],
    
    Results = lists:map(
        fun(Bin) -> 
            storage_client:decode_glacier_retrieval_option(Bin) 
        end,
        Binaries
    ),
    
    %% Check first and last are ok, middle is error
    ?assertMatch([{ok, expedited}, {error, _}, {ok, bulk}], Results).

%%--------------------------------------------------------------------
%% Test: decode with validation in API response handler
%%--------------------------------------------------------------------
decode_with_validation_test() ->
    %% Simulate validating enum values from API response
    ValidateEnum = fun(Bin) ->
        case storage_client:decode_glacier_retrieval_option(Bin) of
            {ok, Value} -> {valid, Value};
            {error, Reason} -> {invalid, Reason}
        end
    end,
    
    ?assertEqual({valid, expedited}, ValidateEnum(<<"expedited">>)),
    ?assertMatch({invalid, {invalid_enum_value, _}}, ValidateEnum(<<"bad_value">>)).

%%--------------------------------------------------------------------
%% Test: decode and pattern match on result
%%--------------------------------------------------------------------
decode_pattern_match_ok_test() ->
    {ok, Value} = storage_client:decode_glacier_retrieval_option(<<"standard">>),
    
    Message = case Value of
        expedited -> "Rush processing";
        standard -> "Normal processing";
        bulk -> "Economy processing"
    end,
    
    ?assertEqual("Normal processing", Message).

decode_pattern_match_error_test() ->
    {error, {invalid_enum_value, BadValue}} = 
        storage_client:decode_glacier_retrieval_option(<<"not_valid">>),
    
    ?assertEqual(<<"not_valid">>, BadValue).

%%--------------------------------------------------------------------
%% Test: decode with default fallback
%%--------------------------------------------------------------------
decode_with_default_test() ->
    GetOptionWithDefault = fun(Bin, Default) ->
        case storage_client:decode_glacier_retrieval_option(Bin) of
            {ok, Value} -> Value;
            {error, _} -> Default
        end
    end,
    
    ?assertEqual(expedited, GetOptionWithDefault(<<"expedited">>, standard)),
    ?assertEqual(standard, GetOptionWithDefault(<<"invalid">>, standard)).

%%--------------------------------------------------------------------
%% Test: decode in union context
%%--------------------------------------------------------------------
decode_enum_in_union_test() ->
    %% Simulate decoding a union with an enum inside
    UnionJson = #{<<"glacier">> => #{
        <<"vault">> => <<"my-vault">>,
        <<"region">> => <<"us-west-2">>,
        <<"retrievalOption">> => <<"expedited">>
    }},
    
    %% Decode the union
    {glacier, GlacierData} = storage_client:decode_storage_type(UnionJson),
    
    %% Extract and decode the enum
    RetrievalBinary = maps:get(<<"retrievalOption">>, GlacierData),
    {ok, RetrievalOption} = storage_client:decode_glacier_retrieval_option(RetrievalBinary),
    
    ?assertEqual(expedited, RetrievalOption).

%%--------------------------------------------------------------------
%% Test: decode is case-sensitive
%%--------------------------------------------------------------------
decode_case_sensitive_test() ->
    %% Lowercase should work
    ?assertMatch({ok, expedited}, 
        storage_client:decode_glacier_retrieval_option(<<"expedited">>)),
    
    %% Uppercase should fail
    ?assertMatch({error, {invalid_enum_value, <<"EXPEDITED">>}}, 
        storage_client:decode_glacier_retrieval_option(<<"EXPEDITED">>)),
    
    %% Mixed case should fail
    ?assertMatch({error, {invalid_enum_value, <<"Expedited">>}}, 
        storage_client:decode_glacier_retrieval_option(<<"Expedited">>)).

%%--------------------------------------------------------------------
%% Test: decode unicode/special characters fail gracefully
%%--------------------------------------------------------------------
decode_unicode_test() ->
    Result = storage_client:decode_glacier_retrieval_option(<<"expe̅dited"/utf8>>),
    ?assertMatch({error, {invalid_enum_value, _}}, Result).

decode_whitespace_test() ->
    %% Extra whitespace should not match
    ?assertMatch({error, {invalid_enum_value, <<"expedited ">>}}, 
        storage_client:decode_glacier_retrieval_option(<<"expedited ">>)),
    
    ?assertMatch({error, {invalid_enum_value, <<" expedited">>}}, 
        storage_client:decode_glacier_retrieval_option(<<" expedited">>)).

%%--------------------------------------------------------------------
%% Test: collect valid enums from list
%%--------------------------------------------------------------------
decode_collect_valid_test() ->
    Binaries = [<<"expedited">>, <<"invalid1">>, <<"standard">>, <<"invalid2">>, <<"bulk">>],
    
    ValidEnums = lists:filtermap(
        fun(Bin) ->
            case storage_client:decode_glacier_retrieval_option(Bin) of
                {ok, Value} -> {true, Value};
                {error, _} -> false
            end
        end,
        Binaries
    ),
    
    ?assertEqual([expedited, standard, bulk], ValidEnums).

%%--------------------------------------------------------------------
%% Test: performance - decode many values
%%--------------------------------------------------------------------
decode_performance_test() ->
    Binaries = lists:duplicate(1000, <<"expedited">>),
    
    Results = [storage_client:decode_glacier_retrieval_option(B) || B <- Binaries],
    
    %% All should succeed
    ?assert(lists:all(fun(R) -> R =:= {ok, expedited} end, Results)).

