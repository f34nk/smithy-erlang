-module(runtime_http_request).

-export([build_uri/3,
         build_query/2,
         build_headers/2,
         execute_request/5]).

%% Build URI with path parameter substitution
-spec build_uri(binary(), [{binary(), term()}], binary()) -> binary().
build_uri(UriPattern, PathParams, Endpoint) ->
    Uri = lists:foldl(
        fun({Name, Value}, Acc) ->
            Placeholder = <<"{", Name/binary, "}">>,
            EncodedValue = runtime_types:url_encode(runtime_types:ensure_binary(Value)),
            binary:replace(Acc, Placeholder, EncodedValue)
        end,
        UriPattern,
        PathParams
    ),
    <<Endpoint/binary, Uri/binary>>.

%% Build query string from parameters
-spec build_query([{binary(), binary()} | {binary(), binary(), boolean()}], map()) -> binary().
build_query(QueryParams, Input) ->
    Pairs = lists:foldl(
        fun
            ({InputKey, QueryKey}, Acc) ->
                case maps:get(InputKey, Input, undefined) of
                    undefined -> Acc;
                    Value -> [{QueryKey, runtime_types:ensure_binary(Value)} | Acc]
                end;
            ({InputKey, QueryKey, true}, Acc) ->
                %% Required parameter
                Value = maps:get(InputKey, Input),
                [{QueryKey, runtime_types:ensure_binary(Value)} | Acc];
            ({InputKey, QueryKey, false}, Acc) ->
                %% Optional parameter
                case maps:get(InputKey, Input, undefined) of
                    undefined -> Acc;
                    Value -> [{QueryKey, runtime_types:ensure_binary(Value)} | Acc]
                end
        end,
        [],
        QueryParams
    ),
    case Pairs of
        [] -> <<"">>;
        _ ->
            Encoded = uri_string:compose_query(lists:reverse(Pairs)),
            <<$?, Encoded/binary>>
    end.

%% Build headers from mapping
-spec build_headers([{binary(), binary()} | {binary(), binary(), boolean()}], map()) -> [{binary(), binary()}].
build_headers(HeaderMapping, Input) ->
    lists:foldl(
        fun
            ({HeaderName, InputKey}, Acc) ->
                case maps:get(InputKey, Input, undefined) of
                    undefined -> Acc;
                    Value -> [{HeaderName, runtime_types:ensure_binary(Value)} | Acc]
                end;
            ({HeaderName, InputKey, true}, Acc) ->
                %% Required header
                Value = maps:get(InputKey, Input),
                [{HeaderName, runtime_types:ensure_binary(Value)} | Acc];
            ({HeaderName, InputKey, false}, Acc) ->
                %% Optional header
                case maps:get(InputKey, Input, undefined) of
                    undefined -> Acc;
                    Value -> [{HeaderName, runtime_types:ensure_binary(Value)} | Acc]
                end
        end,
        [],
        HeaderMapping
    ).

%% Execute HTTP request with signing
-spec execute_request(map(), binary(), binary(), [{binary(), binary()}], binary()) -> 
    {ok, {integer(), [{string(), string()}], binary()}} | {error, term()}.
execute_request(Client, Method, Url, Headers, Body) ->
    %% Sign request if credentials present
    SignedHeaders = case maps:get(credentials, Client, undefined) of
        undefined ->
            Headers;
        Credentials ->
            Region = maps:get(region, Client, <<"us-east-1">>),
            Service = maps:get(service, Client, <<"smithy">>),
            CredentialsWithRegion = Credentials#{region => Region, service => Service},
            aws_sigv4:sign_request(Method, Url, Headers, Body, CredentialsWithRegion)
    end,
    
    %% Build httpc request
    HttpcHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- SignedHeaders],
    Request = case Method of
        <<"GET">> -> 
            {binary_to_list(Url), HttpcHeaders};
        <<"HEAD">> -> 
            {binary_to_list(Url), HttpcHeaders};
        <<"DELETE">> -> 
            {binary_to_list(Url), HttpcHeaders};
        _ -> 
            {binary_to_list(Url), HttpcHeaders, "application/json", binary_to_list(Body)}
    end,
    
    %% Execute request
    MethodAtom = binary_to_atom(string:lowercase(Method), utf8),
    case httpc:request(MethodAtom, Request, [], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, ResponseHeaders, ResponseBody}} ->
            {ok, {StatusCode, ResponseHeaders, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.
