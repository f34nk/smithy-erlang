-module(runtime_http_response_test).
-include_lib("eunit/include/eunit.hrl").

parse_response_success_json_test() ->
    StatusCode = 200,
    ResponseHeaders = [{"content-type", "application/json"}],
    ResponseBody = <<"{\"id\": 123, \"name\": \"test\"}">>,
    ExpectedStatus = 200,
    HeaderMapping = [{<<"content-type">>, <<"ContentType">>}],
    BodyType = json,
    
    {ok, Output} = runtime_http_response:parse_response(
        StatusCode, ResponseHeaders, ResponseBody, 
        ExpectedStatus, HeaderMapping, BodyType
    ),
    
    ?assertEqual(123, maps:get(<<"id">>, Output)),
    ?assertEqual(<<"test">>, maps:get(<<"name">>, Output)),
    ?assertEqual(<<"application/json">>, maps:get(<<"ContentType">>, Output)).

parse_response_success_blob_test() ->
    StatusCode = 200,
    ResponseHeaders = [],
    ResponseBody = <<"binary data here">>,
    ExpectedStatus = 200,
    HeaderMapping = [],
    BodyType = blob,
    
    {ok, Output} = runtime_http_response:parse_response(
        StatusCode, ResponseHeaders, ResponseBody, 
        ExpectedStatus, HeaderMapping, BodyType
    ),
    
    ?assertEqual(<<"binary data here">>, maps:get(<<"Body">>, Output)).

parse_response_success_text_test() ->
    StatusCode = 200,
    ResponseHeaders = [],
    ResponseBody = <<"text data">>,
    ExpectedStatus = 200,
    HeaderMapping = [],
    BodyType = text,
    
    {ok, Output} = runtime_http_response:parse_response(
        StatusCode, ResponseHeaders, ResponseBody, 
        ExpectedStatus, HeaderMapping, BodyType
    ),
    
    ?assertEqual(<<"text data">>, maps:get(<<"Body">>, Output)).

parse_response_success_none_test() ->
    StatusCode = 204,
    ResponseHeaders = [],
    ResponseBody = <<>>,
    ExpectedStatus = 204,
    HeaderMapping = [],
    BodyType = none,
    
    {ok, Output} = runtime_http_response:parse_response(
        StatusCode, ResponseHeaders, ResponseBody, 
        ExpectedStatus, HeaderMapping, BodyType
    ),
    
    ?assertEqual(#{}, Output).

parse_response_error_test() ->
    StatusCode = 404,
    ResponseHeaders = [],
    ResponseBody = <<"{\"error\": \"Not found\"}">>,
    ExpectedStatus = 200,
    HeaderMapping = [],
    BodyType = json,
    
    {error, {404, ErrorData}} = runtime_http_response:parse_response(
        StatusCode, ResponseHeaders, ResponseBody, 
        ExpectedStatus, HeaderMapping, BodyType
    ),
    
    ?assertEqual(<<"Not found">>, maps:get(<<"error">>, ErrorData)).

parse_response_error_empty_body_test() ->
    StatusCode = 500,
    ResponseHeaders = [],
    ResponseBody = <<>>,
    ExpectedStatus = 200,
    HeaderMapping = [],
    BodyType = json,
    
    {error, {500, ErrorData}} = runtime_http_response:parse_response(
        StatusCode, ResponseHeaders, ResponseBody, 
        ExpectedStatus, HeaderMapping, BodyType
    ),
    
    ?assertEqual(#{}, ErrorData).

parse_response_error_invalid_json_test() ->
    StatusCode = 400,
    ResponseHeaders = [],
    ResponseBody = <<"not json">>,
    ExpectedStatus = 200,
    HeaderMapping = [],
    BodyType = json,
    
    {error, {400, ErrorData}} = runtime_http_response:parse_response(
        StatusCode, ResponseHeaders, ResponseBody, 
        ExpectedStatus, HeaderMapping, BodyType
    ),
    
    ?assertEqual(<<"not json">>, maps:get(<<"message">>, ErrorData)).

extract_headers_test() ->
    HeaderMapping = [
        {<<"content-type">>, <<"ContentType">>},
        {<<"etag">>, <<"ETag">>}
    ],
    ResponseHeaders = [
        {"content-type", "application/json"},
        {"etag", "\"abc123\""}
    ],
    Output = #{},
    
    Result = runtime_http_response:extract_headers(HeaderMapping, ResponseHeaders, Output),
    
    ?assertEqual(<<"application/json">>, maps:get(<<"ContentType">>, Result)),
    ?assertEqual(<<"\"abc123\"">>, maps:get(<<"ETag">>, Result)).

extract_headers_case_insensitive_test() ->
    HeaderMapping = [{<<"Content-Type">>, <<"ContentType">>}],
    ResponseHeaders = [{"content-type", "application/json"}],
    Output = #{},
    
    Result = runtime_http_response:extract_headers(HeaderMapping, ResponseHeaders, Output),
    
    ?assertEqual(<<"application/json">>, maps:get(<<"ContentType">>, Result)).

extract_headers_missing_test() ->
    HeaderMapping = [{<<"x-missing">>, <<"Missing">>}],
    ResponseHeaders = [],
    Output = #{},
    
    Result = runtime_http_response:extract_headers(HeaderMapping, ResponseHeaders, Output),
    
    ?assertEqual(#{}, Result).

extract_headers_with_existing_data_test() ->
    HeaderMapping = [{<<"content-type">>, <<"ContentType">>}],
    ResponseHeaders = [{"content-type", "application/json"}],
    Output = #{<<"existing">> => <<"data">>},
    
    Result = runtime_http_response:extract_headers(HeaderMapping, ResponseHeaders, Output),
    
    ?assertEqual(<<"data">>, maps:get(<<"existing">>, Result)),
    ?assertEqual(<<"application/json">>, maps:get(<<"ContentType">>, Result)).
