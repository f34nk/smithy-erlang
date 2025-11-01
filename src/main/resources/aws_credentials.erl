-module(aws_credentials).

%% AWS Credentials Provider
%% Loads AWS credentials from various sources following the AWS credentials chain
%% Reference: https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html

%% Public API
-export([
    get_credentials/0,
    from_environment/0
]).

%% @doc Get AWS credentials from the default credential chain
%%
%% Tries credential providers in the following order:
%% 1. Environment variables (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY)
%% 2. Shared credentials file (~/.aws/credentials) - TODO: Not yet implemented
%% 3. IAM role (EC2 instance metadata) - TODO: Not yet implemented
%%
%% Returns the first successfully loaded credentials, or an error if none are found.
%%
%% @returns {ok, Credentials :: map()} | {error, Reason :: atom()}
%%
%% Example:
%% ```
%% case aws_credentials:get_credentials() of
%%     {ok, Credentials} ->
%%         Client = #{
%%             endpoint => <<"https://s3.amazonaws.com">>,
%%             region => <<"us-east-1">>,
%%             credentials => Credentials
%%         };
%%     {error, Reason} ->
%%         io:format("Failed to load credentials: ~p~n", [Reason])
%% end.
%% ```
-spec get_credentials() -> {ok, map()} | {error, atom()}.
get_credentials() ->
    %% Try providers in order
    Providers = [
        fun from_environment/0
        %% TODO: Add from_credentials_file/0
        %% TODO: Add from_iam_role/0
    ],
    try_providers(Providers).

%% @doc Load AWS credentials from environment variables
%%
%% Reads credentials from the following environment variables:
%% - AWS_ACCESS_KEY_ID (required)
%% - AWS_SECRET_ACCESS_KEY (required)
%% - AWS_SESSION_TOKEN (optional, for temporary credentials)
%%
%% This is the standard AWS SDK environment variable naming convention.
%% These environment variables take precedence over other credential sources
%% in the AWS credentials chain.
%%
%% @returns {ok, Credentials :: map()} | {error, Reason :: atom()}
%%
%% Example:
%% ```
%% %% In shell:
%% %% export AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE
%% %% export AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
%%
%% case aws_credentials:from_environment() of
%%     {ok, Credentials} ->
%%         %% Credentials = #{
%%         %%     access_key_id => <<"AKIAIOSFODNN7EXAMPLE">>,
%%         %%     secret_access_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>
%%         %% }
%%         {ok, Credentials};
%%     {error, no_access_key} ->
%%         io:format("AWS_ACCESS_KEY_ID not set~n");
%%     {error, no_secret_key} ->
%%         io:format("AWS_SECRET_ACCESS_KEY not set~n")
%% end.
%% ```
-spec from_environment() -> {ok, map()} | {error, atom()}.
from_environment() ->
    %% Read required environment variables
    case {os:getenv("AWS_ACCESS_KEY_ID"), os:getenv("AWS_SECRET_ACCESS_KEY")} of
        {false, _} ->
            %% AWS_ACCESS_KEY_ID not set
            {error, no_access_key};
        
        {_, false} ->
            %% AWS_SECRET_ACCESS_KEY not set
            {error, no_secret_key};
        
        {AccessKeyId, SecretAccessKey} when is_list(AccessKeyId), is_list(SecretAccessKey) ->
            %% Both required credentials present
            %% Convert from list (os:getenv returns list) to binary
            Credentials = #{
                access_key_id => list_to_binary(AccessKeyId),
                secret_access_key => list_to_binary(SecretAccessKey)
            },
            
            %% Check for optional session token (for temporary credentials from STS)
            case os:getenv("AWS_SESSION_TOKEN") of
                false ->
                    %% No session token - permanent credentials (IAM user)
                    {ok, Credentials};
                
                Token when is_list(Token) ->
                    %% Session token present - temporary credentials (STS)
                    {ok, Credentials#{session_token => list_to_binary(Token)}}
            end
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Try each credential provider in order until one succeeds
-spec try_providers([fun(() -> {ok, map()} | {error, atom()})]) -> {ok, map()} | {error, no_credentials}.
try_providers([]) ->
    %% No providers succeeded
    {error, no_credentials};

try_providers([Provider | Rest]) ->
    case Provider() of
        {ok, Credentials} ->
            %% Provider succeeded - return credentials
            {ok, Credentials};
        
        {error, _Reason} ->
            %% Provider failed - try next provider
            try_providers(Rest)
    end.
