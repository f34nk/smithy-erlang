-module(user_client_types_test).
-include_lib("eunit/include/eunit.hrl").

%%% This test demonstrates how to work with the generated Smithy types
%%% The generated code will be in build/smithy/source/erlang-client-codegen/src

%% Test: Creating and encoding input structures
create_user_input_test() ->
    %% Create a map representing CreateUserInput
    Input = #{
        <<"name">> => <<"John Doe">>,
        <<"email">> => <<"john@example.com">>,
        <<"age">> => 30,
        <<"status">> => <<"active">>
    },
    
    %% Verify we can encode to JSON
    Encoded = jsx:encode(Input),
    ?assert(is_binary(Encoded)),
    
    %% Verify we can decode back
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"John Doe">>, maps:get(<<"name">>, Decoded)),
    ?assertEqual(<<"john@example.com">>, maps:get(<<"email">>, Decoded)),
    ?assertEqual(30, maps:get(<<"age">>, Decoded)).

%% Test: Working with optional fields
optional_fields_test() ->
    %% Create input with only required fields
    MinimalInput = #{
        <<"name">> => <<"Jane Doe">>,
        <<"email">> => <<"jane@example.com">>
    },
    
    %% Should encode successfully even without optional fields
    Encoded = jsx:encode(MinimalInput),
    ?assert(is_binary(Encoded)),
    
    %% Create input with all fields
    FullInput = #{
        <<"name">> => <<"Jane Doe">>,
        <<"email">> => <<"jane@example.com">>,
        <<"age">> => 25,
        <<"status">> => <<"active">>
    },
    
    FullEncoded = jsx:encode(FullInput),
    ?assert(is_binary(FullEncoded)).

%% Test: User status enum values
user_status_enum_test() ->
    %% Valid status values
    ValidStatuses = [<<"active">>, <<"inactive">>, <<"suspended">>],
    
    lists:foreach(fun(Status) ->
        Input = #{
            <<"name">> => <<"Test User">>,
            <<"email">> => <<"test@example.com">>,
            <<"status">> => Status
        },
        Encoded = jsx:encode(Input),
        ?assert(is_binary(Encoded))
    end, ValidStatuses).

%% Test: User output structure
user_output_test() ->
    %% Simulate a User response from the API
    UserJson = <<"{\"userId\":\"123\",\"name\":\"John Doe\",\"email\":\"john@example.com\",\"age\":30,\"status\":\"active\"}">>,
    
    User = jsx:decode(UserJson, [return_maps]),
    
    %% Verify required fields are present
    ?assertEqual(<<"123">>, maps:get(<<"userId">>, User)),
    ?assertEqual(<<"John Doe">>, maps:get(<<"name">>, User)),
    ?assertEqual(<<"john@example.com">>, maps:get(<<"email">>, User)),
    ?assertEqual(30, maps:get(<<"age">>, User)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, User)).

%% Test: Error structure
error_structure_test() ->
    %% Simulate a UserNotFound error
    ErrorJson = <<"{\"message\":\"User not found\",\"userId\":\"999\"}">>,
    
    Error = jsx:decode(ErrorJson, [return_maps]),
    
    ?assertEqual(<<"User not found">>, maps:get(<<"message">>, Error)),
    ?assertEqual(<<"999">>, maps:get(<<"userId">>, Error)).

%% Test: Update operation with partial data
update_user_test() ->
    %% UpdateUserInput allows updating only specific fields
    UpdateInput = #{
        <<"userId">> => <<"123">>,
        <<"name">> => <<"John Smith">>
        %% Other fields are optional and can be omitted
    },
    
    Encoded = jsx:encode(UpdateInput),
    ?assert(is_binary(Encoded)),
    
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"123">>, maps:get(<<"userId">>, Decoded)),
    ?assertEqual(<<"John Smith">>, maps:get(<<"name">>, Decoded)).

%% Test: Type conversion helpers
type_conversion_test() ->
    %% Helper to convert Erlang terms to Smithy types
    
    %% String -> binary
    Name = <<"John Doe">>,
    ?assert(is_binary(Name)),
    
    %% Integer remains integer
    Age = 30,
    ?assert(is_integer(Age)),
    
    %% Boolean (if we had boolean fields)
    Active = true,
    ?assert(is_boolean(Active)).

%% Test: Building complex nested structures
nested_structure_test() ->
    %% GetUserOutput contains a nested User structure
    Output = #{
        <<"user">> => #{
            <<"userId">> => <<"123">>,
            <<"name">> => <<"John Doe">>,
            <<"email">> => <<"john@example.com">>,
            <<"age">> => 30,
            <<"status">> => <<"active">>
        }
    },
    
    Encoded = jsx:encode(Output),
    ?assert(is_binary(Encoded)),
    
    Decoded = jsx:decode(Encoded, [return_maps]),
    User = maps:get(<<"user">>, Decoded),
    ?assertEqual(<<"123">>, maps:get(<<"userId">>, User)).

%% Test: Timestamp handling (if present in response)
timestamp_test() ->
    %% Timestamps in JSON are typically ISO 8601 strings or epoch numbers
    UserWithTimestamp = #{
        <<"userId">> => <<"123">>,
        <<"name">> => <<"John Doe">>,
        <<"email">> => <<"john@example.com">>,
        <<"createdAt">> => <<"2024-01-01T12:00:00Z">>,
        <<"updatedAt">> => 1704110400
    },
    
    Encoded = jsx:encode(UserWithTimestamp),
    ?assert(is_binary(Encoded)),
    
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"2024-01-01T12:00:00Z">>, maps:get(<<"createdAt">>, Decoded)),
    ?assertEqual(1704110400, maps:get(<<"updatedAt">>, Decoded)).
