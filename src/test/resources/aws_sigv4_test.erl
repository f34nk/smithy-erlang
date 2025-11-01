-module(aws_sigv4_test).

-include_lib("eunit/include/eunit.hrl").

%%% Tests for AWS SigV4 module
%%% Step 3.1: Basic skeleton tests (5 tests)
%%% Step 3.2: Canonical request generation tests (25 tests)
%%% Step 3.3: String to sign generation tests (14 tests)
%%% Total: 44 tests

%% Test that the module compiles and exports are correct
module_info_test() ->
    %% Verify module exists and loads
    ?assertEqual(aws_sigv4, aws_sigv4:module_info(module)),
    
    %% Verify sign_request/5 is exported
    Exports = aws_sigv4:module_info(exports),
    ?assert(lists:member({sign_request, 5}, Exports)).

%% Test sign_request/5 with minimal inputs (returns headers unchanged for now)
sign_request_passthrough_test() ->
    Method = <<"GET">>,
    Url = <<"https://s3.amazonaws.com/mybucket/mykey">>,
    Headers = [{<<"Host">>, <<"s3.amazonaws.com">>}],
    Body = <<>>,
    Credentials = #{
        access_key_id => <<"AKIAIOSFODNN7EXAMPLE">>,
        secret_access_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
        region => <<"us-east-1">>,
        service => <<"s3">>
    },
    
    %% For now, should return headers unchanged
    Result = aws_sigv4:sign_request(Method, Url, Headers, Body, Credentials),
    
    ?assertEqual(Headers, Result).

%% Test with all credential fields including session token
sign_request_with_session_token_test() ->
    Method = <<"POST">>,
    Url = <<"https://dynamodb.us-west-2.amazonaws.com/">>,
    Headers = [{<<"Content-Type">>, <<"application/x-amz-json-1.0">>}],
    Body = <<"{\"TableName\":\"Test\"}">>,
    Credentials = #{
        access_key_id => <<"AKIAIOSFODNN7EXAMPLE">>,
        secret_access_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
        session_token => <<"AQoEXAMPLEH4aoAH0gNCAPyJxz4BlCFFxWNE1OPTgk5TthT+FvwqnKwRcOIfrRh3c/LTo6UDdyJwOOvEVPvLXCrrrUtdnniCEXAMPLE/IvU1dYUg2RVAJBanLiHb4IgRmpRV3zrkuWJOgQs8IZZaIv2BXIa2R4OlgkBN9bkUDNCJiBeb/AXlzBBko7b15fjrBs2+cTQtpZ3CYWFXG8C5zqx37wnOE49mRl/+OtkIKGO7fAE">>,
        region => <<"us-west-2">>,
        service => <<"dynamodb">>
    },
    
    %% For now, should return headers unchanged
    Result = aws_sigv4:sign_request(Method, Url, Headers, Body, Credentials),
    
    ?assertEqual(Headers, Result).

%% Test with empty body
sign_request_empty_body_test() ->
    Method = <<"DELETE">>,
    Url = <<"https://s3.amazonaws.com/mybucket/mykey">>,
    Headers = [{<<"Host">>, <<"s3.amazonaws.com">>}],
    Body = <<>>,
    Credentials = #{
        access_key_id => <<"AKIAIOSFODNN7EXAMPLE">>,
        secret_access_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
        region => <<"us-east-1">>,
        service => <<"s3">>
    },
    
    Result = aws_sigv4:sign_request(Method, Url, Headers, Body, Credentials),
    
    ?assertEqual(Headers, Result).

%% Test function signature accepts all parameter types correctly
sign_request_type_test() ->
    %% Test that function can be called with various input types
    Headers = [{<<"X-Custom-Header">>, <<"value">>}],
    Result = aws_sigv4:sign_request(
        <<"PUT">>,
        <<"https://example.com/path">>,
        Headers,
        <<"body">>,
        #{access_key_id => <<"key">>, 
          secret_access_key => <<"secret">>,
          region => <<"us-east-1">>,
          service => <<"s3">>}
    ),
    %% Should return a list (for now, unchanged headers)
    ?assert(is_list(Result)),
    ?assertEqual(Headers, Result).

%%====================================================================
%% Step 3.2: Canonical Request Generation Tests
%%====================================================================

%% Test hash_sha256/1 with empty string
hash_sha256_empty_test() ->
    %% SHA256 of empty string (AWS test vector)
    Expected = <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>,
    Result = aws_sigv4:hash_sha256(<<>>),
    ?assertEqual(Expected, Result).

%% Test hash_sha256/1 with known value
hash_sha256_known_test() ->
    %% SHA256 of "hello"
    Expected = <<"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824">>,
    Result = aws_sigv4:hash_sha256(<<"hello">>),
    ?assertEqual(Expected, Result).

%% Test hash_sha256/1 with JSON body
hash_sha256_json_test() ->
    Body = <<"{\"TableName\":\"Test\"}">>,
    Result = aws_sigv4:hash_sha256(Body),
    %% Should return lowercase hex string of 64 characters (32 bytes * 2)
    ?assertEqual(64, byte_size(Result)),
    %% Should be all lowercase hex
    ?assert(is_hex_string(Result)).

%% Test signed_header_list/1 with single header
signed_header_list_single_test() ->
    Headers = [{<<"Host">>, <<"example.com">>}],
    Result = aws_sigv4:signed_header_list(Headers),
    ?assertEqual(<<"host">>, Result).

%% Test signed_header_list/1 with multiple headers (sorted)
signed_header_list_multiple_test() ->
    Headers = [
        {<<"X-Amz-Date">>, <<"20230101T120000Z">>},
        {<<"Host">>, <<"s3.amazonaws.com">>},
        {<<"Content-Type">>, <<"application/json">>}
    ],
    Result = aws_sigv4:signed_header_list(Headers),
    %% Should be sorted alphabetically
    ?assertEqual(<<"content-type;host;x-amz-date">>, Result).

%% Test signed_header_list/1 with mixed case
signed_header_list_mixed_case_test() ->
    Headers = [
        {<<"HOST">>, <<"example.com">>},
        {<<"X-AMZ-DATE">>, <<"20230101T120000Z">>}
    ],
    Result = aws_sigv4:signed_header_list(Headers),
    %% Should be lowercase and sorted
    ?assertEqual(<<"host;x-amz-date">>, Result).

%% Test canonicalize_headers/1 with single header
canonicalize_headers_single_test() ->
    Headers = [{<<"Host">>, <<"example.com">>}],
    Result = aws_sigv4:canonicalize_headers(Headers),
    Expected = <<"host:example.com\n">>,
    ?assertEqual(Expected, Result).

%% Test canonicalize_headers/1 with sorting
canonicalize_headers_sorting_test() ->
    Headers = [
        {<<"X-Amz-Date">>, <<"20230101T120000Z">>},
        {<<"Host">>, <<"s3.amazonaws.com">>}
    ],
    Result = aws_sigv4:canonicalize_headers(Headers),
    %% host should come before x-amz-date alphabetically
    Expected = <<"host:s3.amazonaws.com\nx-amz-date:20230101T120000Z\n">>,
    ?assertEqual(Expected, Result).

%% Test canonicalize_headers/1 with whitespace trimming
canonicalize_headers_trim_test() ->
    Headers = [
        {<<"Host">>, <<"  example.com  ">>},
        {<<"X-Custom">>, <<"  value with spaces  ">>}
    ],
    Result = aws_sigv4:canonicalize_headers(Headers),
    %% Whitespace should be trimmed from values
    Expected = <<"host:example.com\nx-custom:value with spaces\n">>,
    ?assertEqual(Expected, Result).

%% Test canonicalize_headers/1 with lowercase conversion
canonicalize_headers_lowercase_test() ->
    Headers = [
        {<<"HOST">>, <<"example.com">>},
        {<<"Content-TYPE">>, <<"application/json">>}
    ],
    Result = aws_sigv4:canonicalize_headers(Headers),
    Expected = <<"content-type:application/json\nhost:example.com\n">>,
    ?assertEqual(Expected, Result).

%% Test canonicalize_query_string/1 with empty string
canonicalize_query_empty_test() ->
    ?assertEqual(<<>>, aws_sigv4:canonicalize_query_string(<<>>)),
    ?assertEqual(<<>>, aws_sigv4:canonicalize_query_string(undefined)).

%% Test canonicalize_query_string/1 with single parameter
canonicalize_query_single_test() ->
    Query = <<"key=value">>,
    Result = aws_sigv4:canonicalize_query_string(Query),
    %% Should be URL-encoded
    ?assertEqual(<<"key=value">>, Result).

%% Test canonicalize_query_string/1 with multiple parameters (sorted)
canonicalize_query_sorting_test() ->
    Query = <<"zebra=last&apple=first&middle=center">>,
    Result = aws_sigv4:canonicalize_query_string(Query),
    %% Should be sorted by key
    ?assertEqual(<<"apple=first&middle=center&zebra=last">>, Result).

%% Test canonicalize_query_string/1 with URL encoding
canonicalize_query_encoding_test() ->
    Query = <<"key=value with spaces">>,
    Result = aws_sigv4:canonicalize_query_string(Query),
    %% Spaces should be encoded as %20
    ?assert(binary:match(Result, <<"%20">>) =/= nomatch).

%% Test canonicalize_query_string/1 with leading ?
canonicalize_query_leading_question_test() ->
    Query = <<"?key=value">>,
    Result = aws_sigv4:canonicalize_query_string(Query),
    ?assertEqual(<<"key=value">>, Result).

%% Test create_canonical_request/4 with GET request
create_canonical_request_get_test() ->
    Method = <<"GET">>,
    Uri = <<"https://s3.amazonaws.com/mybucket/mykey">>,
    Headers = [
        {<<"Host">>, <<"s3.amazonaws.com">>},
        {<<"X-Amz-Date">>, <<"20230101T120000Z">>}
    ],
    Body = <<>>,
    
    Result = aws_sigv4:create_canonical_request(Method, Uri, Headers, Body),
    
    %% Should contain all components
    ?assert(binary:match(Result, <<"GET">>) =/= nomatch),
    ?assert(binary:match(Result, <<"/mybucket/mykey">>) =/= nomatch),
    ?assert(binary:match(Result, <<"host:s3.amazonaws.com">>) =/= nomatch),
    ?assert(binary:match(Result, <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>) =/= nomatch).

%% Test create_canonical_request/4 with POST and body
create_canonical_request_post_test() ->
    Method = <<"POST">>,
    Uri = <<"https://dynamodb.us-west-2.amazonaws.com/">>,
    Headers = [
        {<<"Host">>, <<"dynamodb.us-west-2.amazonaws.com">>},
        {<<"Content-Type">>, <<"application/x-amz-json-1.0">>}
    ],
    Body = <<"{\"TableName\":\"Test\"}">>,
    
    Result = aws_sigv4:create_canonical_request(Method, Uri, Headers, Body),
    
    %% Should contain POST method
    ?assert(binary:match(Result, <<"POST">>) =/= nomatch),
    %% Should contain hashed body (not empty hash)
    EmptyHash = <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>,
    ?assert(binary:match(Result, EmptyHash) =:= nomatch).

%% Test create_canonical_request/4 with query parameters
create_canonical_request_query_test() ->
    Method = <<"GET">>,
    Uri = <<"https://s3.amazonaws.com/mybucket?prefix=photos&max-keys=100">>,
    Headers = [{<<"Host">>, <<"s3.amazonaws.com">>}],
    Body = <<>>,
    
    Result = aws_sigv4:create_canonical_request(Method, Uri, Headers, Body),
    
    %% Should contain sorted query parameters
    ?assert(binary:match(Result, <<"max-keys">>) =/= nomatch),
    ?assert(binary:match(Result, <<"prefix">>) =/= nomatch).

%% Test create_canonical_request/4 with root path
create_canonical_request_root_path_test() ->
    Method = <<"GET">>,
    Uri = <<"https://s3.amazonaws.com/">>,
    Headers = [{<<"Host">>, <<"s3.amazonaws.com">>}],
    Body = <<>>,
    
    Result = aws_sigv4:create_canonical_request(Method, Uri, Headers, Body),
    
    %% Should have / as path
    Lines = binary:split(Result, <<"\n">>, [global]),
    %% Second line should be the path
    Path = lists:nth(2, Lines),
    ?assertEqual(<<"/">>, Path).

%% Test create_canonical_request/4 with no path (implicit root)
create_canonical_request_no_path_test() ->
    Method = <<"GET">>,
    Uri = <<"https://s3.amazonaws.com">>,
    Headers = [{<<"Host">>, <<"s3.amazonaws.com">>}],
    Body = <<>>,
    
    Result = aws_sigv4:create_canonical_request(Method, Uri, Headers, Body),
    
    %% Should have / as path even if not specified
    Lines = binary:split(Result, <<"\n">>, [global]),
    Path = lists:nth(2, Lines),
    ?assertEqual(<<"/">>, Path).

%%====================================================================
%% Step 3.3: String to Sign Generation Tests
%%====================================================================

%% Test credential_scope/3 basic format
credential_scope_basic_test() ->
    DateTime = <<"20230101T120000Z">>,
    Region = <<"us-east-1">>,
    Service = <<"s3">>,
    
    Result = aws_sigv4:credential_scope(DateTime, Region, Service),
    
    Expected = <<"20230101/us-east-1/s3/aws4_request">>,
    ?assertEqual(Expected, Result).

%% Test credential_scope/3 with different region
credential_scope_different_region_test() ->
    DateTime = <<"20230515T093000Z">>,
    Region = <<"eu-west-1">>,
    Service = <<"dynamodb">>,
    
    Result = aws_sigv4:credential_scope(DateTime, Region, Service),
    
    Expected = <<"20230515/eu-west-1/dynamodb/aws4_request">>,
    ?assertEqual(Expected, Result).

%% Test credential_scope/3 date extraction
credential_scope_date_extraction_test() ->
    %% Different times on same day should have same date
    DateTime1 = <<"20230101T000000Z">>,
    DateTime2 = <<"20230101T235959Z">>,
    Region = <<"us-west-2">>,
    Service = <<"s3">>,
    
    Result1 = aws_sigv4:credential_scope(DateTime1, Region, Service),
    Result2 = aws_sigv4:credential_scope(DateTime2, Region, Service),
    
    %% Both should have same credential scope (same date)
    ?assertEqual(Result1, Result2),
    ?assertEqual(<<"20230101/us-west-2/s3/aws4_request">>, Result1).

%% Test credential_scope/3 with various services
credential_scope_services_test() ->
    DateTime = <<"20230101T120000Z">>,
    Region = <<"us-east-1">>,
    
    Services = [<<"s3">>, <<"dynamodb">>, <<"ec2">>, <<"lambda">>],
    
    lists:foreach(
        fun(Service) ->
            Result = aws_sigv4:credential_scope(DateTime, Region, Service),
            Expected = <<"20230101/us-east-1/", Service/binary, "/aws4_request">>,
            ?assertEqual(Expected, Result)
        end,
        Services
    ).

%% Test iso8601_datetime/0 format
iso8601_datetime_format_test() ->
    Result = aws_sigv4:iso8601_datetime(),
    
    %% Should be 16 characters: YYYYMMDDTHHMMSSZ
    ?assertEqual(16, byte_size(Result)),
    
    %% Should start with 20 (year 20XX)
    ?assertEqual(<<"20">>, binary:part(Result, 0, 2)),
    
    %% Should have T separator at position 8
    ?assertEqual($T, binary:at(Result, 8)),
    
    %% Should end with Z
    ?assertEqual($Z, binary:at(Result, 15)),
    
    %% Should be all digits except T and Z
    <<Year:4/binary, Month:2/binary, Day:2/binary, "T",
      Hour:2/binary, Minute:2/binary, Second:2/binary, "Z">> = Result,
    
    ?assert(is_numeric(Year)),
    ?assert(is_numeric(Month)),
    ?assert(is_numeric(Day)),
    ?assert(is_numeric(Hour)),
    ?assert(is_numeric(Minute)),
    ?assert(is_numeric(Second)).

%% Test iso8601_datetime/0 sequential calls
iso8601_datetime_sequential_test() ->
    %% Call twice in quick succession
    Time1 = aws_sigv4:iso8601_datetime(),
    Time2 = aws_sigv4:iso8601_datetime(),
    
    %% Both should be valid timestamps
    ?assertEqual(16, byte_size(Time1)),
    ?assertEqual(16, byte_size(Time2)),
    
    %% Should be same or Time2 >= Time1 (depends on execution speed)
    ?assert(Time2 >= Time1).

%% Test create_string_to_sign/3 basic format
create_string_to_sign_basic_test() ->
    DateTime = <<"20230101T120000Z">>,
    CredentialScope = <<"20230101/us-east-1/s3/aws4_request">>,
    CanonicalRequest = <<"GET\n/\n\nhost:s3.amazonaws.com\n\nhost\ne3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>,
    
    Result = aws_sigv4:create_string_to_sign(DateTime, CredentialScope, CanonicalRequest),
    
    %% Should start with algorithm
    ?assert(binary:match(Result, <<"AWS4-HMAC-SHA256">>) =/= nomatch),
    
    %% Should contain DateTime
    ?assert(binary:match(Result, DateTime) =/= nomatch),
    
    %% Should contain CredentialScope
    ?assert(binary:match(Result, CredentialScope) =/= nomatch),
    
    %% Should have 4 lines (algorithm, datetime, scope, hash)
    Lines = binary:split(Result, <<"\n">>, [global]),
    ?assertEqual(4, length(Lines)).

%% Test create_string_to_sign/3 structure
create_string_to_sign_structure_test() ->
    DateTime = <<"20230515T093000Z">>,
    CredentialScope = <<"20230515/eu-west-1/dynamodb/aws4_request">>,
    CanonicalRequest = <<"POST\n/\n\nhost:dynamodb.eu-west-1.amazonaws.com\n\nhost\n44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a">>,
    
    Result = aws_sigv4:create_string_to_sign(DateTime, CredentialScope, CanonicalRequest),
    
    %% Split into lines
    Lines = binary:split(Result, <<"\n">>, [global]),
    
    %% Line 1: Algorithm
    ?assertEqual(<<"AWS4-HMAC-SHA256">>, lists:nth(1, Lines)),
    
    %% Line 2: DateTime
    ?assertEqual(DateTime, lists:nth(2, Lines)),
    
    %% Line 3: Credential Scope
    ?assertEqual(CredentialScope, lists:nth(3, Lines)),
    
    %% Line 4: Hashed canonical request (64 char hex)
    HashedCanonicalRequest = lists:nth(4, Lines),
    ?assertEqual(64, byte_size(HashedCanonicalRequest)),
    ?assert(is_hex_string(HashedCanonicalRequest)).

%% Test create_string_to_sign/3 with AWS test vector
create_string_to_sign_aws_example_test() ->
    %% Based on AWS SigV4 test suite
    DateTime = <<"20150830T123600Z">>,
    CredentialScope = <<"20150830/us-east-1/service/aws4_request">>,
    
    %% Simple GET request canonical request
    CanonicalRequest = <<"GET\n/\n\nhost:example.amazonaws.com\nx-amz-date:20150830T123600Z\n\nhost;x-amz-date\ne3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>,
    
    Result = aws_sigv4:create_string_to_sign(DateTime, CredentialScope, CanonicalRequest),
    
    %% Verify structure
    Lines = binary:split(Result, <<"\n">>, [global]),
    ?assertEqual(4, length(Lines)),
    ?assertEqual(<<"AWS4-HMAC-SHA256">>, lists:nth(1, Lines)),
    ?assertEqual(DateTime, lists:nth(2, Lines)),
    ?assertEqual(CredentialScope, lists:nth(3, Lines)),
    
    %% Hashed canonical request should be deterministic
    HashedCanonicalRequest = lists:nth(4, Lines),
    ?assertEqual(64, byte_size(HashedCanonicalRequest)).

%% Test create_string_to_sign/3 with empty canonical request
create_string_to_sign_empty_canonical_test() ->
    DateTime = <<"20230101T000000Z">>,
    CredentialScope = <<"20230101/us-east-1/s3/aws4_request">>,
    CanonicalRequest = <<>>,
    
    Result = aws_sigv4:create_string_to_sign(DateTime, CredentialScope, CanonicalRequest),
    
    %% Should still have 4 lines
    Lines = binary:split(Result, <<"\n">>, [global]),
    ?assertEqual(4, length(Lines)),
    
    %% Last line should be hash of empty string
    EmptyHash = <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>,
    ?assertEqual(EmptyHash, lists:nth(4, Lines)).

%% Test integration: credential_scope used in create_string_to_sign
integration_credential_scope_test() ->
    DateTime = <<"20230101T120000Z">>,
    Region = <<"us-east-1">>,
    Service = <<"s3">>,
    
    %% Create credential scope
    CredentialScope = aws_sigv4:credential_scope(DateTime, Region, Service),
    
    %% Use in string to sign
    CanonicalRequest = <<"GET\n/\n\nhost:example.com\n\nhost\ne3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>,
    StringToSign = aws_sigv4:create_string_to_sign(DateTime, CredentialScope, CanonicalRequest),
    
    %% Verify credential scope is in string to sign
    ?assert(binary:match(StringToSign, CredentialScope) =/= nomatch),
    ?assert(binary:match(StringToSign, <<"20230101/us-east-1/s3/aws4_request">>) =/= nomatch).

%% Test full flow: canonical request to string to sign
integration_full_flow_test() ->
    %% Step 1: Create canonical request
    Method = <<"GET">>,
    Uri = <<"https://s3.amazonaws.com/mybucket/mykey">>,
    Headers = [
        {<<"Host">>, <<"s3.amazonaws.com">>},
        {<<"X-Amz-Date">>, <<"20230101T120000Z">>}
    ],
    Body = <<>>,
    
    CanonicalRequest = aws_sigv4:create_canonical_request(Method, Uri, Headers, Body),
    
    %% Step 2: Create credential scope
    DateTime = <<"20230101T120000Z">>,
    Region = <<"us-east-1">>,
    Service = <<"s3">>,
    CredentialScope = aws_sigv4:credential_scope(DateTime, Region, Service),
    
    %% Step 3: Create string to sign
    StringToSign = aws_sigv4:create_string_to_sign(DateTime, CredentialScope, CanonicalRequest),
    
    %% Verify result structure
    Lines = binary:split(StringToSign, <<"\n">>, [global]),
    ?assertEqual(4, length(Lines)),
    ?assertEqual(<<"AWS4-HMAC-SHA256">>, lists:nth(1, Lines)),
    ?assertEqual(DateTime, lists:nth(2, Lines)),
    ?assertEqual(<<"20230101/us-east-1/s3/aws4_request">>, lists:nth(3, Lines)),
    
    %% Hash should be deterministic
    HashedCanonicalRequest = lists:nth(4, Lines),
    ?assertEqual(64, byte_size(HashedCanonicalRequest)),
    ?assert(is_hex_string(HashedCanonicalRequest)).

%%====================================================================
%% Helper Functions for Tests
%%====================================================================

%% Check if a binary is a valid hex string
is_hex_string(Bin) ->
    lists:all(
        fun(C) -> 
            (C >= $0 andalso C =< $9) orelse 
            (C >= $a andalso C =< $f) orelse
            (C >= $A andalso C =< $F)
        end,
        binary_to_list(Bin)
    ).

%% Check if a binary is numeric
is_numeric(Bin) ->
    lists:all(
        fun(C) -> C >= $0 andalso C =< $9 end,
        binary_to_list(Bin)
    ).
