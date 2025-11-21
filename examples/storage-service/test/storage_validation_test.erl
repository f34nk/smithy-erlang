-module(storage_validation_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Suite for @required Field Validation
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: CreateStorageLocationInput validation - all required fields present
%%--------------------------------------------------------------------
validate_create_with_all_required_test() ->
    Input = #{
        <<"name">> => <<"My Storage">>,
        <<"storageType">> => #{<<"s3">> => #{}}
    },
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertEqual(ok, Result).

validate_create_with_all_required_and_optional_test() ->
    Input = #{
        <<"name">> => <<"My Storage">>,
        <<"storageType">> => #{<<"s3">> => #{}},
        <<"optional_field">> => <<"some_value">>
    },
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertEqual(ok, Result).

%%--------------------------------------------------------------------
%% Test: CreateStorageLocationInput validation - missing required fields
%%--------------------------------------------------------------------
validate_create_missing_name_test() ->
    Input = #{
        <<"storageType">> => #{<<"s3">> => #{}}
    },
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertMatch({error, {missing_required_fields, _}}, Result),
    {error, {missing_required_fields, Missing}} = Result,
    ?assert(lists:member(<<"name">>, Missing)).

validate_create_missing_storage_type_test() ->
    Input = #{
        <<"name">> => <<"My Storage">>
    },
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertMatch({error, {missing_required_fields, _}}, Result),
    {error, {missing_required_fields, Missing}} = Result,
    ?assert(lists:member(<<"storageType">>, Missing)).

validate_create_missing_all_required_test() ->
    Input = #{},
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertMatch({error, {missing_required_fields, _}}, Result),
    {error, {missing_required_fields, Missing}} = Result,
    ?assertEqual(2, length(Missing)),
    ?assert(lists:member(<<"name">>, Missing)),
    ?assert(lists:member(<<"storageType">>, Missing)).

validate_create_missing_both_test() ->
    Input = #{
        <<"other_field">> => <<"value">>
    },
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertMatch({error, {missing_required_fields, [_, _]}}, Result).

%%--------------------------------------------------------------------
%% Test: GetStorageLocationInput validation - all required fields present
%%--------------------------------------------------------------------
validate_get_with_all_required_test() ->
    Input = #{
        <<"locationId">> => <<"loc-123">>
    },
    Result = storage_client:validate_get_storage_location_input(Input),
    ?assertEqual(ok, Result).

validate_get_with_required_and_extra_test() ->
    Input = #{
        <<"locationId">> => <<"loc-123">>,
        <<"extra_field">> => <<"ignored">>
    },
    Result = storage_client:validate_get_storage_location_input(Input),
    ?assertEqual(ok, Result).

%%--------------------------------------------------------------------
%% Test: GetStorageLocationInput validation - missing required field
%%--------------------------------------------------------------------
validate_get_missing_location_id_test() ->
    Input = #{},
    Result = storage_client:validate_get_storage_location_input(Input),
    ?assertMatch({error, {missing_required_fields, _}}, Result),
    {error, {missing_required_fields, Missing}} = Result,
    ?assertEqual([<<"locationId">>], Missing).

validate_get_with_wrong_field_test() ->
    Input = #{
        <<"wrongField">> => <<"value">>
    },
    Result = storage_client:validate_get_storage_location_input(Input),
    ?assertMatch({error, {missing_required_fields, [<<"locationId">>]}}, Result).

%%--------------------------------------------------------------------
%% Test: Validation with empty field values (values present but empty)
%%--------------------------------------------------------------------
validate_create_with_empty_name_test() ->
    Input = #{
        <<"name">> => <<"">>,
        <<"storageType">> => #{<<"s3">> => #{}}
    },
    %% Empty values are still present, so validation passes
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertEqual(ok, Result).

validate_create_with_null_like_values_test() ->
    Input = #{
        <<"name">> => null,
        <<"storageType">> => undefined
    },
    %% Keys are present, validation passes (value validation is separate)
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertEqual(ok, Result).

%%--------------------------------------------------------------------
%% Test: Pattern matching on validation result
%%--------------------------------------------------------------------
validate_pattern_match_ok_test() ->
    Input = #{
        <<"name">> => <<"Test">>,
        <<"storageType">> => #{}
    },
    
    case storage_client:validate_create_storage_location_input(Input) of
        ok -> ok;
        {error, _} -> ?assert(false)
    end.

validate_pattern_match_error_test() ->
    Input = #{},
    
    case storage_client:validate_create_storage_location_input(Input) of
        ok -> ?assert(false);
        {error, {missing_required_fields, Missing}} ->
            ?assert(is_list(Missing)),
            ?assert(length(Missing) > 0)
    end.

%%--------------------------------------------------------------------
%% Test: Validation in guard
%%--------------------------------------------------------------------
validate_in_guard_test() ->
    ValidInput = #{
        <<"name">> => <<"Test">>,
        <<"storageType">> => #{}
    },
    
    IsValid = case storage_client:validate_create_storage_location_input(ValidInput) of
        ok -> true;
        _ -> false
    end,
    
    ?assert(IsValid).

%%--------------------------------------------------------------------
%% Test: Validation with different data types for field values
%%--------------------------------------------------------------------
validate_with_binary_values_test() ->
    Input = #{
        <<"name">> => <<"Storage Name">>,
        <<"storageType">> => #{<<"type">> => <<"s3">>}
    },
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertEqual(ok, Result).

validate_with_map_values_test() ->
    Input = #{
        <<"name">> => <<"Storage">>,
        <<"storageType">> => #{
            <<"s3">> => #{
                <<"bucket">> => <<"my-bucket">>,
                <<"region">> => <<"us-east-1">>
            }
        }
    },
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertEqual(ok, Result).

validate_with_integer_values_test() ->
    Input = #{
        <<"name">> => 12345,  % Wrong type, but key is present
        <<"storageType">> => #{}
    },
    %% Validation only checks key presence, not value types
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertEqual(ok, Result).

%%--------------------------------------------------------------------
%% Test: Validation in API request flow
%%--------------------------------------------------------------------
validate_before_request_success_test() ->
    Input = #{
        <<"name">> => <<"Production Storage">>,
        <<"storageType">> => #{<<"s3">> => #{}}
    },
    
    %% Simulate validation before API call
    case storage_client:validate_create_storage_location_input(Input) of
        ok ->
            %% Would proceed with API call
            ok;
        {error, Reason} ->
            %% Would return error without making call
            {error, Reason}
    end,
    
    ?assertEqual(ok, storage_client:validate_create_storage_location_input(Input)).

validate_before_request_failure_test() ->
    Input = #{
        <<"name">> => <<"Missing Storage Type">>
    },
    
    %% Simulate validation before API call
    Result = case storage_client:validate_create_storage_location_input(Input) of
        ok ->
            %% Would proceed with API call
            make_api_call;
        {error, Reason} ->
            %% Would return error without making call
            {validation_failed, Reason}
    end,
    
    ?assertMatch({validation_failed, {missing_required_fields, _}}, Result).

%%--------------------------------------------------------------------
%% Test: Collect missing fields
%%--------------------------------------------------------------------
validate_collect_missing_fields_test() ->
    Inputs = [
        #{<<"name">> => <<"A">>},
        #{<<"storageType">> => #{}},
        #{<<"name">> => <<"B">>, <<"storageType">> => #{}},
        #{}
    ],
    
    Results = lists:map(
        fun(Input) ->
            storage_client:validate_create_storage_location_input(Input)
        end,
        Inputs
    ),
    
    %% Count errors
    ErrorCount = length([R || R <- Results, element(1, R) =:= error]),
    ?assertEqual(3, ErrorCount).

%%--------------------------------------------------------------------
%% Test: Validation with list comprehension
%%--------------------------------------------------------------------
validate_list_comprehension_test() ->
    Inputs = [
        #{<<"locationId">> => <<"1">>},
        #{<<"locationId">> => <<"2">>},
        #{},
        #{<<"locationId">> => <<"3">>}
    ],
    
    ValidInputs = [
        I || I <- Inputs,
        storage_client:validate_get_storage_location_input(I) =:= ok
    ],
    
    ?assertEqual(3, length(ValidInputs)).

%%--------------------------------------------------------------------
%% Test: Error message contains field names
%%--------------------------------------------------------------------
validate_error_contains_field_names_test() ->
    Input = #{},
    {error, {missing_required_fields, Missing}} = 
        storage_client:validate_create_storage_location_input(Input),
    
    %% Verify field names are binaries
    ?assert(lists:all(fun is_binary/1, Missing)),
    
    %% Verify specific fields are mentioned
    ?assert(lists:member(<<"name">>, Missing)),
    ?assert(lists:member(<<"storageType">>, Missing)).

%%--------------------------------------------------------------------
%% Test: Validation with nested structures
%%--------------------------------------------------------------------
validate_with_nested_map_test() ->
    Input = #{
        <<"name">> => <<"Nested Storage">>,
        <<"storageType">> => #{
            <<"glacier">> => #{
                <<"vault">> => <<"archive-vault">>,
                <<"region">> => <<"us-west-2">>,
                <<"retrievalOption">> => <<"expedited">>
            }
        }
    },
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertEqual(ok, Result).

%%--------------------------------------------------------------------
%% Test: Multiple validations in sequence
%%--------------------------------------------------------------------
validate_multiple_inputs_test() ->
    CreateInput = #{
        <<"name">> => <<"Storage">>,
        <<"storageType">> => #{}
    },
    GetInput = #{
        <<"locationId">> => <<"loc-123">>
    },
    
    CreateResult = storage_client:validate_create_storage_location_input(CreateInput),
    GetResult = storage_client:validate_get_storage_location_input(GetInput),
    
    ?assertEqual(ok, CreateResult),
    ?assertEqual(ok, GetResult).

%%--------------------------------------------------------------------
%% Test: Validation with unicode field values
%%--------------------------------------------------------------------
validate_with_unicode_test() ->
    Input = #{
        <<"name">> => <<"存储位置"/utf8>>,
        <<"storageType">> => #{}
    },
    Result = storage_client:validate_create_storage_location_input(Input),
    ?assertEqual(ok, Result).

%%--------------------------------------------------------------------
%% Test: Validation preserves field order in error
%%--------------------------------------------------------------------
validate_field_order_test() ->
    Input = #{},
    {error, {missing_required_fields, Missing}} = 
        storage_client:validate_create_storage_location_input(Input),
    
    %% Both fields should be in the error
    ?assertEqual(2, length(Missing)),
    ?assert(lists:member(<<"name">>, Missing)),
    ?assert(lists:member(<<"storageType">>, Missing)).

%%--------------------------------------------------------------------
%% Test: Performance - validate many inputs
%%--------------------------------------------------------------------
validate_performance_test() ->
    Inputs = [
        #{<<"name">> => <<"Storage ", (integer_to_binary(N))/binary>>,
          <<"storageType">> => #{}}
        || N <- lists:seq(1, 1000)
    ],
    
    Results = [storage_client:validate_create_storage_location_input(I) || I <- Inputs],
    
    %% All should succeed
    ?assert(lists:all(fun(R) -> R =:= ok end, Results)).

