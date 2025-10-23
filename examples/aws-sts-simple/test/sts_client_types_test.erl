-module(sts_client_types_test).
-include_lib("eunit/include/eunit.hrl").

%%% This test demonstrates how to work with the generated STS client types
%%% Tests focus on input/output structure validation without making actual HTTP calls

%% Test: GetCallerIdentity request structure
get_caller_identity_request_test() ->
    %% GetCallerIdentityRequest has no members
    Request = #{},
    
    %% Verify we can encode empty map
    Encoded = jsx:encode(Request),
    ?assert(is_binary(Encoded)).

%% Test: GetCallerIdentity response structure
get_caller_identity_response_test() ->
    %% Simulate a GetCallerIdentityResponse
    ResponseJson = <<"{\"UserId\":\"AIDAI23HXX2LCI6BEXAMPLE\",\"Account\":\"123456789012\",\"Arn\":\"arn:aws:iam::123456789012:user/Alice\"}">>,
    
    Response = jsx:decode(ResponseJson, [return_maps]),
    
    %% Verify fields are present
    ?assertEqual(<<"AIDAI23HXX2LCI6BEXAMPLE">>, maps:get(<<"UserId">>, Response)),
    ?assertEqual(<<"123456789012">>, maps:get(<<"Account">>, Response)),
    ?assertEqual(<<"arn:aws:iam::123456789012:user/Alice">>, maps:get(<<"Arn">>, Response)).

%% Test: AssumeRole request with required fields only
assume_role_request_minimal_test() ->
    %% Create an AssumeRoleRequest with only required fields
    Request = #{
        <<"RoleArn">> => <<"arn:aws:iam::123456789012:role/demo">>,
        <<"RoleSessionName">> => <<"session-name">>
    },
    
    %% Verify we can encode to JSON
    Encoded = jsx:encode(Request),
    ?assert(is_binary(Encoded)),
    
    %% Verify required fields
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"arn:aws:iam::123456789012:role/demo">>, maps:get(<<"RoleArn">>, Decoded)),
    ?assertEqual(<<"session-name">>, maps:get(<<"RoleSessionName">>, Decoded)).

%% Test: AssumeRole request with all fields
assume_role_request_full_test() ->
    %% Create an AssumeRoleRequest with all fields
    Request = #{
        <<"RoleArn">> => <<"arn:aws:iam::123456789012:role/demo">>,
        <<"RoleSessionName">> => <<"session-name">>,
        <<"PolicyArns">> => [
            #{<<"arn">> => <<"arn:aws:iam::aws:policy/ReadOnlyAccess">>}
        ],
        <<"Policy">> => <<"{\"Version\":\"2012-10-17\"}">>,
        <<"DurationSeconds">> => 3600,
        <<"ExternalId">> => <<"external-id-123">>
    },
    
    %% Verify we can encode to JSON
    Encoded = jsx:encode(Request),
    ?assert(is_binary(Encoded)),
    
    %% Verify all fields
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"arn:aws:iam::123456789012:role/demo">>, maps:get(<<"RoleArn">>, Decoded)),
    ?assertEqual(<<"session-name">>, maps:get(<<"RoleSessionName">>, Decoded)),
    ?assertEqual(3600, maps:get(<<"DurationSeconds">>, Decoded)),
    ?assertEqual(<<"external-id-123">>, maps:get(<<"ExternalId">>, Decoded)).

%% Test: AssumeRole response structure
assume_role_response_test() ->
    %% Simulate an AssumeRoleResponse
    ResponseJson = <<"{\"Credentials\":{\"AccessKeyId\":\"AKIAIOSFODNN7EXAMPLE\",\"SecretAccessKey\":\"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY\",\"SessionToken\":\"AQoDYXdzEJr...\",\"Expiration\":\"2024-01-01T12:00:00Z\"},\"AssumedRoleUser\":{\"AssumedRoleId\":\"AROA3XFRBF535PLBIFPI4:session-name\",\"Arn\":\"arn:aws:sts::123456789012:assumed-role/demo/session-name\"}}">>,
    
    Response = jsx:decode(ResponseJson, [return_maps]),
    
    %% Verify Credentials structure
    Credentials = maps:get(<<"Credentials">>, Response),
    ?assert(is_map(Credentials)),
    ?assertEqual(<<"AKIAIOSFODNN7EXAMPLE">>, maps:get(<<"AccessKeyId">>, Credentials)),
    ?assertEqual(<<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>, maps:get(<<"SecretAccessKey">>, Credentials)),
    ?assertEqual(<<"AQoDYXdzEJr...">>, maps:get(<<"SessionToken">>, Credentials)),
    ?assertEqual(<<"2024-01-01T12:00:00Z">>, maps:get(<<"Expiration">>, Credentials)),
    
    %% Verify AssumedRoleUser structure
    AssumedRoleUser = maps:get(<<"AssumedRoleUser">>, Response),
    ?assert(is_map(AssumedRoleUser)),
    ?assertEqual(<<"AROA3XFRBF535PLBIFPI4:session-name">>, maps:get(<<"AssumedRoleId">>, AssumedRoleUser)),
    ?assertEqual(<<"arn:aws:sts::123456789012:assumed-role/demo/session-name">>, maps:get(<<"Arn">>, AssumedRoleUser)).

%% Test: GetSessionToken request with no fields
get_session_token_request_empty_test() ->
    %% GetSessionTokenRequest with no members
    Request = #{},
    
    %% Verify we can encode empty map
    Encoded = jsx:encode(Request),
    ?assert(is_binary(Encoded)).

%% Test: GetSessionToken request with optional fields
get_session_token_request_with_mfa_test() ->
    %% Create a GetSessionTokenRequest with MFA
    Request = #{
        <<"DurationSeconds">> => 3600,
        <<"SerialNumber">> => <<"arn:aws:iam::123456789012:mfa/user">>,
        <<"TokenCode">> => <<"123456">>
    },
    
    %% Verify we can encode to JSON
    Encoded = jsx:encode(Request),
    ?assert(is_binary(Encoded)),
    
    %% Verify fields
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(3600, maps:get(<<"DurationSeconds">>, Decoded)),
    ?assertEqual(<<"arn:aws:iam::123456789012:mfa/user">>, maps:get(<<"SerialNumber">>, Decoded)),
    ?assertEqual(<<"123456">>, maps:get(<<"TokenCode">>, Decoded)).

%% Test: GetSessionToken response structure
get_session_token_response_test() ->
    %% Simulate a GetSessionTokenResponse
    ResponseJson = <<"{\"Credentials\":{\"AccessKeyId\":\"AKIAIOSFODNN7EXAMPLE\",\"SecretAccessKey\":\"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY\",\"SessionToken\":\"AQoDYXdzEJr...\",\"Expiration\":\"2024-01-01T12:00:00Z\"}}">>,
    
    Response = jsx:decode(ResponseJson, [return_maps]),
    
    %% Verify Credentials structure
    Credentials = maps:get(<<"Credentials">>, Response),
    ?assert(is_map(Credentials)),
    ?assertEqual(<<"AKIAIOSFODNN7EXAMPLE">>, maps:get(<<"AccessKeyId">>, Credentials)),
    ?assertEqual(<<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>, maps:get(<<"SecretAccessKey">>, Credentials)),
    ?assertEqual(<<"AQoDYXdzEJr...">>, maps:get(<<"SessionToken">>, Credentials)),
    ?assertEqual(<<"2024-01-01T12:00:00Z">>, maps:get(<<"Expiration">>, Credentials)).

%% Test: Credentials structure with all required fields
credentials_structure_test() ->
    %% Create a Credentials map
    Credentials = #{
        <<"AccessKeyId">> => <<"AKIAIOSFODNN7EXAMPLE">>,
        <<"SecretAccessKey">> => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
        <<"SessionToken">> => <<"AQoDYXdzEJr...">>,
        <<"Expiration">> => <<"2024-01-01T12:00:00Z">>
    },
    
    %% Verify we can encode to JSON
    Encoded = jsx:encode(Credentials),
    ?assert(is_binary(Encoded)),
    
    %% Verify all required fields
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"AKIAIOSFODNN7EXAMPLE">>, maps:get(<<"AccessKeyId">>, Decoded)),
    ?assertEqual(<<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>, maps:get(<<"SecretAccessKey">>, Decoded)),
    ?assertEqual(<<"AQoDYXdzEJr...">>, maps:get(<<"SessionToken">>, Decoded)),
    ?assertEqual(<<"2024-01-01T12:00:00Z">>, maps:get(<<"Expiration">>, Decoded)).

%% Test: AssumedRoleUser structure
assumed_role_user_structure_test() ->
    %% Create an AssumedRoleUser map
    AssumedRoleUser = #{
        <<"AssumedRoleId">> => <<"AROA3XFRBF535PLBIFPI4:session-name">>,
        <<"Arn">> => <<"arn:aws:sts::123456789012:assumed-role/demo/session-name">>
    },
    
    %% Verify we can encode to JSON
    Encoded = jsx:encode(AssumedRoleUser),
    ?assert(is_binary(Encoded)),
    
    %% Verify required fields
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"AROA3XFRBF535PLBIFPI4:session-name">>, maps:get(<<"AssumedRoleId">>, Decoded)),
    ?assertEqual(<<"arn:aws:sts::123456789012:assumed-role/demo/session-name">>, maps:get(<<"Arn">>, Decoded)).

%% Test: PolicyDescriptorList with multiple policies
policy_descriptor_list_test() ->
    %% Create a PolicyDescriptorList
    PolicyList = [
        #{<<"arn">> => <<"arn:aws:iam::aws:policy/ReadOnlyAccess">>},
        #{<<"arn">> => <<"arn:aws:iam::aws:policy/PowerUserAccess">>}
    ],
    
    %% Verify we can encode to JSON
    Encoded = jsx:encode(PolicyList),
    ?assert(is_binary(Encoded)),
    
    %% Verify list structure
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assert(is_list(Decoded)),
    ?assertEqual(2, length(Decoded)),
    
    %% Verify first policy
    [Policy1 | _] = Decoded,
    ?assertEqual(<<"arn:aws:iam::aws:policy/ReadOnlyAccess">>, maps:get(<<"arn">>, Policy1)).

%% Test: PolicyDescriptorType structure
policy_descriptor_type_test() ->
    %% Create a PolicyDescriptorType
    PolicyDescriptor = #{
        <<"arn">> => <<"arn:aws:iam::aws:policy/ReadOnlyAccess">>
    },
    
    %% Verify we can encode to JSON
    Encoded = jsx:encode(PolicyDescriptor),
    ?assert(is_binary(Encoded)),
    
    %% Verify field
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"arn:aws:iam::aws:policy/ReadOnlyAccess">>, maps:get(<<"arn">>, Decoded)).

%% Test: MalformedPolicyDocumentException error structure
malformed_policy_document_exception_test() ->
    %% Simulate a MalformedPolicyDocumentException
    ErrorJson = <<"{\"message\":\"The request was rejected because the policy document was malformed\"}">>,
    
    Error = jsx:decode(ErrorJson, [return_maps]),
    
    ?assertEqual(<<"The request was rejected because the policy document was malformed">>, maps:get(<<"message">>, Error)).

%% Test: PackedPolicyTooLargeException error structure
packed_policy_too_large_exception_test() ->
    %% Simulate a PackedPolicyTooLargeException
    ErrorJson = <<"{\"message\":\"The request was rejected because the total packed size of the session policies exceeds the limit\"}">>,
    
    Error = jsx:decode(ErrorJson, [return_maps]),
    
    ?assertEqual(<<"The request was rejected because the total packed size of the session policies exceeds the limit">>, maps:get(<<"message">>, Error)).

%% Test: Client creation
client_creation_test() ->
    %% Test creating a new client
    Config = #{endpoint => <<"https://sts.amazonaws.com">>},
    {ok, Client} = sts_client:new(Config),
    ?assert(is_map(Client)),
    ?assertEqual(<<"https://sts.amazonaws.com">>, maps:get(endpoint, Client)).
