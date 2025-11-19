-module(runtime_http_response).

-export([parse_response/6,
         extract_headers/3]).

%% Parse response based on expected status code
-spec parse_response(integer(), [{string(), string()}], binary(), 
                     integer(), [{binary(), binary()}], atom()) ->
    {ok, map()} | {error, term()}.
parse_response(StatusCode, ResponseHeaders, ResponseBody, ExpectedStatus, HeaderMapping, BodyType) ->
    case StatusCode of
        ExpectedStatus ->
            %% Success - parse response
            Output = parse_body(ResponseBody, BodyType),
            OutputWithHeaders = extract_headers(HeaderMapping, ResponseHeaders, Output),
            {ok, OutputWithHeaders};
        _ ->
            %% Error response
            ErrorData = case byte_size(ResponseBody) of
                0 -> #{};
                _ -> 
                    try
                        jsx:decode(ResponseBody, [return_maps])
                    catch
                        _:_ -> #{<<"message">> => ResponseBody}
                    end
            end,
            {error, {StatusCode, ErrorData}}
    end.

%% Parse response body based on type
-spec parse_body(binary(), atom()) -> map().
parse_body(Body, json) ->
    case byte_size(Body) of
        0 -> #{};
        _ -> jsx:decode(Body, [return_maps])
    end;
parse_body(Body, blob) ->
    #{<<"Body">> => Body};
parse_body(Body, text) ->
    #{<<"Body">> => Body};
parse_body(_, none) ->
    #{}.

%% Extract headers from response
-spec extract_headers([{binary(), binary()}], [{string(), string()}], map()) -> map().
extract_headers(HeaderMapping, ResponseHeaders, Output) ->
    lists:foldl(
        fun({HeaderName, OutputKey}, Acc) ->
            HeaderNameStr = string:lowercase(binary_to_list(HeaderName)),
            case lists:keyfind(HeaderNameStr, 1, ResponseHeaders) of
                {_, Value} ->
                    maps:put(OutputKey, list_to_binary(Value), Acc);
                false ->
                    Acc
            end
        end,
        Output,
        HeaderMapping
    ).
