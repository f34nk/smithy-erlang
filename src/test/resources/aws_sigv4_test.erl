-module(aws_sigv4_test).

-include_lib("eunit/include/eunit.hrl").

%%% Basic tests for AWS SigV4 module skeleton
%%% More comprehensive tests will be added as signing is implemented

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
