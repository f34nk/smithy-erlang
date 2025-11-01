-module(aws_sigv4).

%% AWS Signature Version 4 signing implementation
%% Reference: https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html

-export([sign_request/5]).

%% Internal functions (exported for testing)
-export([
    create_canonical_request/4,
    canonicalize_query_string/1,
    canonicalize_headers/1,
    signed_header_list/1,
    hash_sha256/1
]).

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
    %% TODO: Implement full AWS SigV4 signing algorithm
    %% 
    %% Steps remaining (Phase 3.3-3.5):
    %% 2. Create string to sign (Algorithm, DateTime, Scope, HashedRequest)
    %% 3. Calculate signature (HMAC-SHA256 with derived key)
    %% 4. Format Authorization header
    %% 5. Add required headers (X-Amz-Date, X-Amz-Security-Token if present)
    %%
    %% For now, return headers unchanged (pass-through)
    %% Step 3.2 (Canonical Request Generation) is implemented below
    Headers.

%%====================================================================
%% Canonical Request Generation (Step 3.2)
%%====================================================================

%% @doc Create canonical request per AWS SigV4 specification
%%
%% The canonical request consists of:
%% 1. HTTP method
%% 2. Canonical URI (path component)
%% 3. Canonical query string (sorted, encoded)
%% 4. Canonical headers (lowercase, sorted, trimmed)
%% 5. Signed headers (list of header names)
%% 6. Hashed payload (SHA256 hex)
%%
%% @param Method HTTP method as binary (e.g., <<"GET">>)
%% @param Uri Complete URI including path and query
%% @param Headers List of header tuples [{Name, Value}]
%% @param Body Request body as binary
%% @returns Canonical request as binary
-spec create_canonical_request(
    Method :: binary(),
    Uri :: binary(),
    Headers :: [{binary(), binary()}],
    Body :: binary()
) -> binary().
create_canonical_request(Method, Uri, Headers, Body) ->
    %% 1. HTTP method (already canonical - uppercase)
    CanonicalMethod = Method,
    
    %% 2. Canonical URI (path component, URL-encoded except /)
    ParsedUri = uri_string:parse(Uri),
    CanonicalUri = case maps:get(path, ParsedUri, <<>>) of
        <<>> -> <<"/">>;  %% Empty path becomes /
        Path -> Path      %% Path is already URL-encoded
    end,
    
    %% 3. Canonical query string (sorted by key, URL-encoded)
    CanonicalQuery = case maps:get(query, ParsedUri, undefined) of
        undefined -> <<>>;
        Query -> canonicalize_query_string(Query)
    end,
    
    %% 4. Canonical headers (lowercase, sorted, trimmed)
    CanonicalHeaders = canonicalize_headers(Headers),
    
    %% 5. Signed headers (semicolon-separated list of header names)
    SignedHeaders = signed_header_list(Headers),
    
    %% 6. Hashed payload (SHA256 in lowercase hex)
    HashedPayload = hash_sha256(Body),
    
    %% Build canonical request with newlines between components
    <<
        CanonicalMethod/binary, "\n",
        CanonicalUri/binary, "\n",
        CanonicalQuery/binary, "\n",
        CanonicalHeaders/binary, "\n",
        SignedHeaders/binary, "\n",
        HashedPayload/binary
    >>.

%% @doc Canonicalize query string per AWS SigV4 specification
%%
%% Process:
%% 1. Parse query parameters
%% 2. Sort by parameter name
%% 3. URL-encode names and values
%% 4. Join with &
%%
%% @param Query Query string (may include leading ?)
%% @returns Canonical query string
-spec canonicalize_query_string(binary() | undefined) -> binary().
canonicalize_query_string(undefined) ->
    <<>>;
canonicalize_query_string(<<>>) ->
    <<>>;
canonicalize_query_string(<<"?", Query/binary>>) ->
    canonicalize_query_string(Query);
canonicalize_query_string(Query) when is_binary(Query) ->
    %% Parse query string into list of {Key, Value} pairs
    Params = uri_string:dissect_query(Query),
    
    %% Sort by key
    SortedParams = lists:sort(fun({K1, _}, {K2, _}) -> K1 =< K2 end, Params),
    
    %% URL-encode and format as key=value
    EncodedParams = lists:map(
        fun({Key, Value}) ->
            %% AWS requires specific encoding: space as %20, not +
            %% uri_string:quote encodes properly for AWS
            EncodedKey = uri_string:quote(Key),
            EncodedValue = uri_string:quote(Value),
            <<EncodedKey/binary, "=", EncodedValue/binary>>
        end,
        SortedParams
    ),
    
    %% Join with &
    join_binaries(EncodedParams, <<"&">>).

%% @doc Canonicalize headers per AWS SigV4 specification
%%
%% Process:
%% 1. Convert header names to lowercase
%% 2. Trim whitespace from values
%% 3. Sort by header name
%% 4. Format as name:value\n
%%
%% @param Headers List of header tuples
%% @returns Canonical headers string with trailing newline
-spec canonicalize_headers([{binary(), binary()}]) -> binary().
canonicalize_headers(Headers) ->
    %% Convert to lowercase and trim
    Normalized = lists:map(
        fun({Name, Value}) ->
            LowerName = string:lowercase(Name),
            TrimmedValue = string:trim(Value, both),
            {LowerName, TrimmedValue}
        end,
        Headers
    ),
    
    %% Sort by header name
    Sorted = lists:sort(fun({N1, _}, {N2, _}) -> N1 =< N2 end, Normalized),
    
    %% Format as name:value\n for each header
    Formatted = lists:map(
        fun({Name, Value}) ->
            <<Name/binary, ":", Value/binary, "\n">>
        end,
        Sorted
    ),
    
    %% Join all headers
    iolist_to_binary(Formatted).

%% @doc Create signed headers list per AWS SigV4 specification
%%
%% Creates a semicolon-separated list of lowercase header names, sorted.
%%
%% @param Headers List of header tuples
%% @returns Signed headers string (e.g., <<"host;x-amz-date">>)
-spec signed_header_list([{binary(), binary()}]) -> binary().
signed_header_list(Headers) ->
    %% Extract and lowercase header names
    Names = lists:map(
        fun({Name, _Value}) -> string:lowercase(Name) end,
        Headers
    ),
    
    %% Sort and remove duplicates
    Sorted = lists:sort(lists:usort(Names)),
    
    %% Join with semicolon
    join_binaries(Sorted, <<";">>).

%% @doc Calculate SHA256 hash and return as lowercase hex string
%%
%% @param Data Binary data to hash
%% @returns SHA256 hash as lowercase hex binary
-spec hash_sha256(binary()) -> binary().
hash_sha256(Data) ->
    Hash = crypto:hash(sha256, Data),
    %% Convert to lowercase hex string
    binary:encode_hex(Hash, lowercase).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Join list of binaries with separator
-spec join_binaries([binary()], binary()) -> binary().
join_binaries([], _Sep) ->
    <<>>;
join_binaries([H], _Sep) ->
    H;
join_binaries([H | T], Sep) ->
    iolist_to_binary([H, [[Sep, X] || X <- T]]).
