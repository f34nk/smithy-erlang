-module(aws_sigv4).

%% AWS Signature Version 4 signing implementation
%% Reference: https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html

-export([sign_request/5]).

%% @doc Sign an HTTP request using AWS Signature Version 4
%%
%% This function adds the Authorization header and other required headers
%% to authenticate requests to AWS services.
%%
%% @param Method HTTP method (GET, POST, PUT, DELETE, etc.) as binary
%% @param Url Complete URL including protocol, host, path, and query string
%% @param Headers List of header tuples [{Name, Value}] where both are binary
%% @param Body Request body as binary
%% @param Credentials Map containing:
%%   - access_key_id: AWS access key ID (binary)
%%   - secret_access_key: AWS secret access key (binary)
%%   - region: AWS region (binary, e.g., <<"us-east-1">>)
%%   - service: AWS service name (binary, e.g., <<"s3">>)
%%   - session_token (optional): Temporary session token (binary)
%%
%% @returns Updated headers list with Authorization and other AWS headers added
-spec sign_request(
    Method :: binary(),
    Url :: binary(),
    Headers :: [{binary(), binary()}],
    Body :: binary(),
    Credentials :: map()
) -> [{binary(), binary()}].
sign_request(_Method, _Url, Headers, _Body, _Credentials) ->
    %% TODO: Implement AWS SigV4 signing algorithm
    %% 
    %% Steps to implement (Phase 3.2-3.5):
    %% 1. Create canonical request (Method, URI, Query, Headers, Payload)
    %% 2. Create string to sign (Algorithm, DateTime, Scope, HashedRequest)
    %% 3. Calculate signature (HMAC-SHA256 with derived key)
    %% 4. Format Authorization header
    %% 5. Add required headers (X-Amz-Date, X-Amz-Security-Token if present)
    %%
    %% For now, return headers unchanged (pass-through)
    Headers.
