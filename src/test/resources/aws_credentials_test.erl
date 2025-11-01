-module(aws_credentials_test).

-include_lib("eunit/include/eunit.hrl").

%%% Tests for AWS credentials provider module
%%% Tests environment variable credential loading

%%====================================================================
%% from_environment/0 Tests
%%====================================================================

%% Test loading credentials with both required variables set
from_environment_with_credentials_test() ->
    %% Set environment variables
    os:putenv("AWS_ACCESS_KEY_ID", "AKIAIOSFODNN7EXAMPLE"),
    os:putenv("AWS_SECRET_ACCESS_KEY", "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"),
    
    %% Ensure no session token
    os:unsetenv("AWS_SESSION_TOKEN"),
    
    %% Load credentials
    Result = aws_credentials:from_environment(),
    
    %% Verify success
    ?assertMatch({ok, _}, Result),
    
    %% Extract and verify credentials
    {ok, Credentials} = Result,
    ?assertEqual(<<"AKIAIOSFODNN7EXAMPLE">>, maps:get(access_key_id, Credentials)),
    ?assertEqual(<<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>, maps:get(secret_access_key, Credentials)),
    
    %% Should not have session token
    ?assertEqual(false, maps:is_key(session_token, Credentials)),
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY").

%% Test loading credentials with session token (temporary credentials)
from_environment_with_session_token_test() ->
    %% Set environment variables including session token
    os:putenv("AWS_ACCESS_KEY_ID", "ASIATEMP123EXAMPLE"),
    os:putenv("AWS_SECRET_ACCESS_KEY", "tempSecret/key/example"),
    os:putenv("AWS_SESSION_TOKEN", "FQoGZXIvYXdzEBoaDNt5ZW1zY2..."),
    
    %% Load credentials
    Result = aws_credentials:from_environment(),
    
    %% Verify success
    ?assertMatch({ok, _}, Result),
    
    %% Extract and verify credentials
    {ok, Credentials} = Result,
    ?assertEqual(<<"ASIATEMP123EXAMPLE">>, maps:get(access_key_id, Credentials)),
    ?assertEqual(<<"tempSecret/key/example">>, maps:get(secret_access_key, Credentials)),
    ?assertEqual(<<"FQoGZXIvYXdzEBoaDNt5ZW1zY2...">>, maps:get(session_token, Credentials)),
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY"),
    os:unsetenv("AWS_SESSION_TOKEN").

%% Test missing AWS_ACCESS_KEY_ID
from_environment_missing_access_key_test() ->
    %% Unset AWS_ACCESS_KEY_ID
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    
    %% Set only secret key
    os:putenv("AWS_SECRET_ACCESS_KEY", "secret"),
    
    %% Load credentials
    Result = aws_credentials:from_environment(),
    
    %% Should return error
    ?assertEqual({error, no_access_key}, Result),
    
    %% Cleanup
    os:unsetenv("AWS_SECRET_ACCESS_KEY").

%% Test missing AWS_SECRET_ACCESS_KEY
from_environment_missing_secret_key_test() ->
    %% Set only access key
    os:putenv("AWS_ACCESS_KEY_ID", "AKIAIOSFODNN7EXAMPLE"),
    
    %% Unset AWS_SECRET_ACCESS_KEY
    os:unsetenv("AWS_SECRET_ACCESS_KEY"),
    
    %% Load credentials
    Result = aws_credentials:from_environment(),
    
    %% Should return error
    ?assertEqual({error, no_secret_key}, Result),
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID").

%% Test with both credentials missing
from_environment_missing_both_test() ->
    %% Unset all credentials
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY"),
    os:unsetenv("AWS_SESSION_TOKEN"),
    
    %% Load credentials
    Result = aws_credentials:from_environment(),
    
    %% Should return error (no_access_key checked first)
    ?assertEqual({error, no_access_key}, Result).

%% Test with empty string values
from_environment_empty_string_test() ->
    %% Set empty strings (should work - os:getenv returns list)
    os:putenv("AWS_ACCESS_KEY_ID", ""),
    os:putenv("AWS_SECRET_ACCESS_KEY", ""),
    
    %% Load credentials
    Result = aws_credentials:from_environment(),
    
    %% Should succeed (empty strings are valid, though not useful)
    ?assertMatch({ok, _}, Result),
    
    {ok, Credentials} = Result,
    ?assertEqual(<<>>, maps:get(access_key_id, Credentials)),
    ?assertEqual(<<>>, maps:get(secret_access_key, Credentials)),
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY").

%% Test with special characters in credentials
from_environment_special_chars_test() ->
    %% Set credentials with special characters
    os:putenv("AWS_ACCESS_KEY_ID", "AKIA+/=EXAMPLE"),
    os:putenv("AWS_SECRET_ACCESS_KEY", "secret+/=with+special/chars=="),
    
    %% Load credentials
    Result = aws_credentials:from_environment(),
    
    %% Should succeed
    ?assertMatch({ok, _}, Result),
    
    {ok, Credentials} = Result,
    ?assertEqual(<<"AKIA+/=EXAMPLE">>, maps:get(access_key_id, Credentials)),
    ?assertEqual(<<"secret+/=with+special/chars==">>, maps:get(secret_access_key, Credentials)),
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY").

%%====================================================================
%% get_credentials/0 Tests
%%====================================================================

%% Test get_credentials with environment variables set
get_credentials_from_environment_test() ->
    %% Set environment variables
    os:putenv("AWS_ACCESS_KEY_ID", "AKIAIOSFODNN7EXAMPLE"),
    os:putenv("AWS_SECRET_ACCESS_KEY", "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"),
    
    %% Get credentials using default chain
    Result = aws_credentials:get_credentials(),
    
    %% Should succeed
    ?assertMatch({ok, _}, Result),
    
    {ok, Credentials} = Result,
    ?assertEqual(<<"AKIAIOSFODNN7EXAMPLE">>, maps:get(access_key_id, Credentials)),
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY").

%% Test get_credentials with no credentials available
get_credentials_no_credentials_test() ->
    %% Unset all environment variables
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY"),
    os:unsetenv("AWS_SESSION_TOKEN"),
    
    %% Get credentials
    Result = aws_credentials:get_credentials(),
    
    %% Should return error (no credentials found in chain)
    ?assertEqual({error, no_credentials}, Result).

%% Test get_credentials returns first available provider
get_credentials_first_provider_test() ->
    %% Set environment variables (first provider)
    os:putenv("AWS_ACCESS_KEY_ID", "AKIAENV123"),
    os:putenv("AWS_SECRET_ACCESS_KEY", "envSecret"),
    
    %% Get credentials
    Result = aws_credentials:get_credentials(),
    
    %% Should return environment credentials (first provider)
    ?assertMatch({ok, _}, Result),
    {ok, Credentials} = Result,
    ?assertEqual(<<"AKIAENV123">>, maps:get(access_key_id, Credentials)),
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY").

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test complete workflow with environment credentials
integration_environment_workflow_test() ->
    %% Setup: Set environment variables
    os:putenv("AWS_ACCESS_KEY_ID", "AKIAIOSFODNN7EXAMPLE"),
    os:putenv("AWS_SECRET_ACCESS_KEY", "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"),
    
    %% Step 1: Get credentials
    {ok, Credentials} = aws_credentials:get_credentials(),
    
    %% Step 2: Verify credentials structure is correct for use with client
    ?assert(maps:is_key(access_key_id, Credentials)),
    ?assert(maps:is_key(secret_access_key, Credentials)),
    ?assert(is_binary(maps:get(access_key_id, Credentials))),
    ?assert(is_binary(maps:get(secret_access_key, Credentials))),
    
    %% Step 3: Credentials should be ready to use with aws_sigv4
    %% (Simulate client creation)
    Client = #{
        endpoint => <<"https://s3.amazonaws.com">>,
        region => <<"us-east-1">>,
        credentials => Credentials
    },
    
    ?assertEqual(<<"https://s3.amazonaws.com">>, maps:get(endpoint, Client)),
    ?assert(maps:is_key(credentials, Client)),
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY").

%% Test workflow with temporary credentials
integration_temporary_credentials_test() ->
    %% Setup: Set temporary credentials
    os:putenv("AWS_ACCESS_KEY_ID", "ASIATEMP123"),
    os:putenv("AWS_SECRET_ACCESS_KEY", "tempSecret"),
    os:putenv("AWS_SESSION_TOKEN", "FQoGZXIvYXdzEBoa..."),
    
    %% Get credentials
    {ok, Credentials} = aws_credentials:from_environment(),
    
    %% Verify session token present
    ?assert(maps:is_key(session_token, Credentials)),
    ?assertEqual(<<"FQoGZXIvYXdzEBoa...">>, maps:get(session_token, Credentials)),
    
    %% Credentials ready for SigV4 (will include X-Amz-Security-Token)
    ?assert(is_map(Credentials)),
    ?assertEqual(3, maps:size(Credentials)),  % access_key_id + secret_access_key + session_token
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY"),
    os:unsetenv("AWS_SESSION_TOKEN").

%%====================================================================
%% Data Type Tests
%%====================================================================

%% Test that all values are binary
credentials_binary_type_test() ->
    os:putenv("AWS_ACCESS_KEY_ID", "test123"),
    os:putenv("AWS_SECRET_ACCESS_KEY", "secret456"),
    
    {ok, Credentials} = aws_credentials:from_environment(),
    
    %% All values should be binary
    lists:foreach(
        fun(Key) ->
            Value = maps:get(Key, Credentials),
            ?assert(is_binary(Value))
        end,
        maps:keys(Credentials)
    ),
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY").

%% Test credentials map structure
credentials_map_structure_test() ->
    os:putenv("AWS_ACCESS_KEY_ID", "test"),
    os:putenv("AWS_SECRET_ACCESS_KEY", "secret"),
    
    {ok, Credentials} = aws_credentials:from_environment(),
    
    %% Should be a map
    ?assert(is_map(Credentials)),
    
    %% Should have required keys
    ?assert(maps:is_key(access_key_id, Credentials)),
    ?assert(maps:is_key(secret_access_key, Credentials)),
    
    %% Should be exactly 2 keys (no session token)
    ?assertEqual(2, maps:size(Credentials)),
    
    %% Cleanup
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY").
