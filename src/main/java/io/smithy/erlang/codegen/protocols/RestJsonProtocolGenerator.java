package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.MemberShape;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.Shape;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.shapes.StructureShape;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.RequiredTrait;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Protocol generator for REST-JSON protocol.
 * 
 * <p>This generator handles code generation for services using the REST-JSON
 * protocol, which is used by API Gateway, Lambda, and other JSON-based 
 * RESTful AWS services.
 * 
 * <h2>Protocol Characteristics</h2>
 * <ul>
 *   <li>HTTP Method: From {@code @http} trait (GET, PUT, POST, DELETE)</li>
 *   <li>URI Path: From {@code @http} trait with {@code @httpLabel} substitution</li>
 *   <li>Query Parameters: From {@code @httpQuery} members</li>
 *   <li>Headers: From {@code @httpHeader} members</li>
 *   <li>Content-Type: {@code application/json}</li>
 *   <li>Request Body: JSON encoded with {@code jsx:encode/1}</li>
 *   <li>Response Body: JSON decoded with {@code jsx:decode/2}</li>
 *   <li>Authentication: AWS SigV4</li>
 * </ul>
 * 
 * <h2>Error Response Format</h2>
 * <pre>
 * {
 *   "message": "Error message",
 *   "__type": "ErrorCode"
 * }
 * </pre>
 * <p>or</p>
 * <pre>
 * {
 *   "Message": "Error message",
 *   "code": "ErrorCode"
 * }
 * </pre>
 * 
 * <h2>Services Using REST-JSON</h2>
 * <ul>
 *   <li>Amazon API Gateway</li>
 *   <li>AWS Lambda</li>
 *   <li>Amazon Cognito</li>
 *   <li>AWS AppSync</li>
 * </ul>
 * 
 * @see ProtocolGenerator
 */
public class RestJsonProtocolGenerator implements ProtocolGenerator {
    
    /** Protocol trait ID for REST-JSON 1 */
    public static final ShapeId REST_JSON_1 = ShapeId.from("aws.protocols#restJson1");
    
    /**
     * Creates a REST-JSON protocol generator.
     */
    public RestJsonProtocolGenerator() {
    }
    
    @Override
    public ShapeId getProtocol() {
        return REST_JSON_1;
    }
    
    @Override
    public String getName() {
        return "restJson1";
    }
    
    @Override
    public String getDefaultMethod() {
        return null; // REST protocols use @http trait for method
    }
    
    @Override
    public String getDefaultUri() {
        return null; // REST protocols use @http trait for URI
    }
    
    @Override
    public String getContentType(ServiceShape service) {
        return "application/json";
    }
    
    @Override
    public void generateOperation(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        Model model = context.model();
        String opName = EnhancedErlangSymbolProvider.toErlangName(operation.getId().getName());
        
        // Determine input and output types
        String inputType = "map()";
        String outputType = "map()";
        
        if (operation.getInput().isPresent()) {
            ShapeId inputId = operation.getInput().get();
            String inputRecordName = EnhancedErlangSymbolProvider.toErlangName(inputId.getName());
            inputType = inputRecordName + "()";
        }
        
        if (operation.getOutput().isPresent()) {
            ShapeId outputId = operation.getOutput().get();
            String outputRecordName = EnhancedErlangSymbolProvider.toErlangName(outputId.getName());
            outputType = outputRecordName + "()";
        }
        
        // Generate 2-arity wrapper
        writer.writeComment("Calls the " + operation.getId().getName() + " operation");
        writer.writeSpec(opName, "Client :: map(), Input :: " + inputType, "{ok, " + outputType + "} | {error, term()}");
        writer.write("$L(Client, Input) ->", opName);
        writer.indent();
        writer.write("$L(Client, Input, #{}).", opName);
        writer.dedent();
        writer.write("");
        
        // Generate 3-arity version with retry support
        writer.writeComment("Calls the " + operation.getId().getName() + " operation with options");
        writer.writeSpec(opName, "Client :: map(), Input :: " + inputType + ", Options :: map()", "{ok, " + outputType + "} | {error, term()}");
        writer.write("$L(Client, Input, Options) when is_map(Input), is_map(Options) ->", opName);
        writer.indent();
        
        String internalFunctionName = "make_" + opName + "_request";
        writer.write("RequestFun = fun() -> $L(Client, Input) end,", internalFunctionName);
        writer.write("");
        writer.write("case maps:get(enable_retry, Options, true) of");
        writer.indent();
        writer.write("true -> aws_retry:with_retry(RequestFun, Options);");
        writer.write("false -> RequestFun()");
        writer.dedent();
        writer.write("end.");
        writer.dedent();
        writer.write("");
        
        // Generate internal request function
        generateInternalRequestFunction(operation, writer, context, opName, inputType, outputType);
    }
    
    /**
     * Generates the internal request function.
     */
    private void generateInternalRequestFunction(OperationShape operation, ErlangWriter writer,
            ErlangContext context, String opName, String inputType, String outputType) {
        
        Model model = context.model();
        String internalFunctionName = "make_" + opName + "_request";
        
        writer.writeComment("Internal function to make the " + operation.getId().getName() + " request");
        writer.writeSpec(internalFunctionName, "Client :: map(), Input :: " + inputType, "{ok, " + outputType + "} | {error, term()}");
        writer.write("$L(Client, Input) when is_map(Input) ->", internalFunctionName);
        writer.indent();
        
        // Get HTTP method and URI from @http trait
        String method = ProtocolUtils.getHttpMethod(operation, "POST");
        String uri = ProtocolUtils.getHttpUri(operation, "/");
        
        // Get HTTP binding members
        Optional<StructureShape> inputShape = ProtocolUtils.getInputShape(operation, model);
        List<MemberShape> httpLabelMembers = inputShape
            .map(ProtocolUtils::getLabelMembers)
            .orElse(Collections.emptyList());
        List<MemberShape> httpQueryMembers = inputShape
            .map(ProtocolUtils::getQueryMembers)
            .orElse(Collections.emptyList());
        List<MemberShape> httpHeaderInputMembers = inputShape
            .map(ProtocolUtils::getHeaderMembers)
            .orElse(Collections.emptyList());
        
        writer.write("Method = <<\"$L\">>,", method);
        writer.write("Endpoint = maps:get(endpoint, Client),");
        writer.write("");
        
        // Build query string
        generateQueryString(httpQueryMembers, writer);
        writer.write("");
        
        // Build URL
        generateUrl(uri, httpLabelMembers, writer);
        writer.write("");
        
        // Generate request body
        String contentType = generateRequestBody(operation, model, inputShape, writer);
        writer.write("");
        
        // Generate headers
        generateRequestHeaders(httpHeaderInputMembers, contentType, writer);
        writer.write("");
        
        // Make the request with SigV4 signing
        generateHttpRequest(operation, model, contentType, writer);
        
        writer.dedent();
        writer.write("");
    }
    
    private void generateQueryString(List<MemberShape> httpQueryMembers, ErlangWriter writer) {
        if (httpQueryMembers.isEmpty()) {
            writer.write("%% No query parameters");
            writer.write("QueryString = <<\"\">>,");
        } else {
            writer.write("%% Build query string from @httpQuery parameters");
            writer.write("QueryPairs0 = [],");
            
            for (int i = 0; i < httpQueryMembers.size(); i++) {
                MemberShape member = httpQueryMembers.get(i);
                String memberName = member.getMemberName();
                String erlangFieldName = EnhancedErlangSymbolProvider.toErlangName(memberName);
                String queryName = member.expectTrait(software.amazon.smithy.model.traits.HttpQueryTrait.class).getValue();
                if (queryName == null || queryName.isEmpty()) {
                    queryName = memberName;
                }
                
                String currentPairsVar = "QueryPairs" + i;
                String nextPairsVar = "QueryPairs" + (i + 1);
                
                writer.write("$L = case maps:get(<<\"$L\">>, Input, undefined) of", nextPairsVar, memberName);
                writer.indent();
                writer.write("undefined -> $L;", currentPairsVar);
                writer.write("$LVal -> [{<<\"$L\">>, ensure_binary($LVal)} | $L]",
                        capitalize(erlangFieldName), queryName, capitalize(erlangFieldName), currentPairsVar);
                writer.dedent();
                writer.write("end,");
            }
            
            String finalPairsVar = "QueryPairs" + httpQueryMembers.size();
            writer.write("QueryString = case $L of", finalPairsVar);
            writer.indent();
            writer.write("[] -> <<\"\">>;");
            writer.write("Pairs -> Encoded = uri_string:compose_query(Pairs), <<\"?\", Encoded/binary>>");
            writer.dedent();
            writer.write("end,");
        }
    }
    
    private void generateUrl(String uri, List<MemberShape> httpLabelMembers, ErlangWriter writer) {
        if (httpLabelMembers.isEmpty()) {
            writer.write("%% No path parameters");
            writer.write("Uri = <<\"$L\">>,", uri);
        } else {
            writer.write("%% Build URL with path parameters");
            writer.write("Uri0 = <<\"$L\">>,", uri);
            
            for (int i = 0; i < httpLabelMembers.size(); i++) {
                MemberShape member = httpLabelMembers.get(i);
                String memberName = member.getMemberName();
                String erlangFieldName = EnhancedErlangSymbolProvider.toErlangName(memberName);
                String currentUriVar = "Uri" + i;
                String nextUriVar = "Uri" + (i + 1);
                
                writer.write("$LValue = maps:get(<<\"$L\">>, Input),", capitalize(erlangFieldName), memberName);
                writer.write("$LEncoded = url_encode(ensure_binary($LValue)),", capitalize(erlangFieldName), capitalize(erlangFieldName));
                writer.write("$L = binary:replace($L, <<\"{$L}\">>, $LEncoded),",
                        nextUriVar, currentUriVar, memberName, capitalize(erlangFieldName));
            }
        }
        
        String uriVar = httpLabelMembers.isEmpty() ? "Uri" : "Uri" + httpLabelMembers.size();
        writer.write("Url = <<Endpoint/binary, $L/binary, QueryString/binary>>,", uriVar);
    }
    
    private String generateRequestBody(OperationShape operation, Model model,
            Optional<StructureShape> inputShape, ErlangWriter writer) {
        
        Optional<MemberShape> payloadMember = inputShape.flatMap(ProtocolUtils::getPayloadMember);
        
        if (!payloadMember.isPresent()) {
            if (!inputShape.isPresent()) {
                writer.write("%% No input - empty request body");
                writer.write("Body = <<\"{}\">>,");
                return "application/json";
            }
            
            List<MemberShape> bodyMembers = ProtocolUtils.getBodyMembers(inputShape.get());
            if (bodyMembers.isEmpty()) {
                writer.write("%% No body members - empty request body");
                writer.write("Body = <<\"{}\">>,");
                return "application/json";
            }
            
            writer.write("%% Encode body members as JSON");
            writer.write("BodyMap0 = #{},");
            for (int i = 0; i < bodyMembers.size(); i++) {
                MemberShape member = bodyMembers.get(i);
                String memberName = member.getMemberName();
                String currentMapVar = "BodyMap" + i;
                String nextMapVar = "BodyMap" + (i + 1);
                String valVar = "BVal" + i;
                writer.write("$L = case maps:get(<<\"$L\">>, Input, undefined) of", nextMapVar, memberName);
                writer.indent();
                writer.write("undefined -> $L;", currentMapVar);
                writer.write("$L -> $L#{<<\"$L\">> => $L}", valVar, currentMapVar, memberName, valVar);
                writer.dedent();
                writer.write("end,");
            }
            writer.write("Body = jsx:encode($L),", "BodyMap" + bodyMembers.size());
            return "application/json";
        }
        
        // @httpPayload present
        MemberShape payload = payloadMember.get();
        String memberName = payload.getMemberName();
        Shape targetShape = model.expectShape(payload.getTarget());
        
        writer.write("%% @httpPayload - extract payload member");
        if (targetShape.isBlobShape()) {
            writer.write("Body = maps:get(<<\"$L\">>, Input),", memberName);
            return "application/octet-stream";
        } else if (targetShape.isStringShape()) {
            writer.write("Body = ensure_binary(maps:get(<<\"$L\">>, Input)),", memberName);
            return "text/plain";
        } else {
            writer.write("Body = jsx:encode(maps:get(<<\"$L\">>, Input)),", memberName);
            return "application/json";
        }
    }
    
    private void generateRequestHeaders(List<MemberShape> httpHeaderMembers, String contentType, ErlangWriter writer) {
        if (httpHeaderMembers.isEmpty()) {
            writer.write("%% Headers");
            writer.write("Headers = [{<<\"Content-Type\">>, <<\"$L\">>}],", contentType);
        } else {
            writer.write("%% Build headers with @httpHeader members");
            writer.write("Headers0 = [{<<\"Content-Type\">>, <<\"$L\">>}],", contentType);
            
            for (int i = 0; i < httpHeaderMembers.size(); i++) {
                MemberShape member = httpHeaderMembers.get(i);
                String memberName = member.getMemberName();
                String erlangFieldName = EnhancedErlangSymbolProvider.toErlangName(memberName);
                HttpHeaderTrait headerTrait = member.expectTrait(HttpHeaderTrait.class);
                String headerName = headerTrait.getValue();
                if (headerName == null || headerName.isEmpty()) {
                    headerName = memberName;
                }
                boolean isRequired = member.hasTrait(RequiredTrait.class);
                String currentHeadersVar = "Headers" + i;
                String nextHeadersVar = "Headers" + (i + 1);
                
                if (isRequired) {
                    writer.write("$LValue = ensure_binary(maps:get(<<\"$L\">>, Input)),",
                            capitalize(erlangFieldName), memberName);
                    writer.write("$L = [{<<\"$L\">>, $LValue} | $L],",
                            nextHeadersVar, headerName, capitalize(erlangFieldName), currentHeadersVar);
                } else {
                    writer.write("$L = case maps:get(<<\"$L\">>, Input, undefined) of", nextHeadersVar, memberName);
                    writer.indent();
                    writer.write("undefined -> $L;", currentHeadersVar);
                    writer.write("$LValue -> [{<<\"$L\">>, ensure_binary($LValue)} | $L]",
                            capitalize(erlangFieldName), headerName, capitalize(erlangFieldName), currentHeadersVar);
                    writer.dedent();
                    writer.write("end,");
                }
            }
            
            writer.write("Headers = $L,", "Headers" + httpHeaderMembers.size());
        }
    }
    
    private void generateHttpRequest(OperationShape operation, Model model, String contentType, ErlangWriter writer) {
        writer.write("%% Sign and send request");
        writer.write("case aws_sigv4:sign_request(Method, Url, Headers, Body, Client) of");
        writer.indent();
        writer.write("{ok, SignedHeaders} ->");
        writer.indent();
        writer.write("ContentType = \"$L\",", contentType);
        writer.write("StringHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- SignedHeaders],");
        writer.write("Request = case Method of");
        writer.indent();
        writer.write("<<\"GET\">> -> {binary_to_list(Url), StringHeaders};");
        writer.write("<<\"DELETE\">> -> {binary_to_list(Url), StringHeaders};");
        writer.write("<<\"HEAD\">> -> {binary_to_list(Url), StringHeaders};");
        writer.write("_ -> {binary_to_list(Url), StringHeaders, ContentType, Body}");
        writer.dedent();
        writer.write("end,");
        writer.write("");
        writer.write("case httpc:request(binary_to_atom(string:lowercase(Method), utf8), Request, [], [{body_format, binary}]) of");
        writer.indent();
        writer.write("{ok, {{_, StatusCode, _}, _RespHeaders, ResponseBody}} when StatusCode >= 200, StatusCode < 300 ->");
        writer.indent();
        generateResponseDeserializer(operation, writer, null);
        writer.dedent();
        writer.write("{ok, {{_, StatusCode, _}, _RespHeaders, ErrorBody}} ->");
        writer.indent();
        generateErrorParser(operation, writer, null);
        writer.dedent();
        writer.write("{error, Reason} ->");
        writer.indent();
        writer.write("{error, {http_error, Reason}}");
        writer.dedent();
        writer.dedent();
        writer.write("end;");
        writer.dedent();
        writer.write("{error, SignError} ->");
        writer.indent();
        writer.write("{error, {signing_error, SignError}}");
        writer.dedent();
        writer.dedent();
        writer.write("end.");
    }
    
    @Override
    public void generateHeaders(OperationShape operation, ServiceShape service,
            ErlangWriter writer, ErlangContext context) {
        // Headers are generated inline in generateOperation()
        writer.writeComment("REST-JSON headers (generated in operation)");
    }
    
    @Override
    public void generateRequestSerializer(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        writer.writeComment("REST-JSON request serialization uses jsx:encode/1");
        writer.write("Body = jsx:encode(Input),");
    }
    
    @Override
    public void generateResponseDeserializer(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        writer.write("%% Decode JSON response");
        writer.write("case ResponseBody of");
        writer.indent();
        writer.write("<<>> -> {ok, #{}};");
        writer.write("<<\"{}\">> -> {ok, #{}};");
        writer.write("_ ->");
        writer.indent();
        writer.write("try jsx:decode(ResponseBody, [return_maps]) of");
        writer.indent();
        writer.write("DecodedBody -> {ok, DecodedBody}");
        writer.dedent();
        writer.write("catch");
        writer.indent();
        writer.write("_:DecodeError -> {error, {json_decode_error, DecodeError}}");
        writer.dedent();
        writer.write("end");
        writer.dedent();
        writer.dedent();
        writer.write("end;");
    }
    
    @Override
    public void generateErrorParser(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        writer.write("%% Parse REST-JSON error response");
        writer.write("case ErrorBody of");
        writer.indent();
        writer.write("<<>> -> {error, {http_error, StatusCode, <<\"No error body\">>}};");
        writer.write("_ ->");
        writer.indent();
        writer.write("try jsx:decode(ErrorBody, [return_maps]) of");
        writer.indent();
        writer.write("ErrorMap ->");
        writer.indent();
        writer.write("Message = case maps:get(<<\"message\">>, ErrorMap, undefined) of");
        writer.indent();
        writer.write("undefined -> maps:get(<<\"Message\">>, ErrorMap, <<\"Unknown error\">>);");
        writer.write("Msg -> Msg");
        writer.dedent();
        writer.write("end,");
        writer.write("Code = case maps:get(<<\"__type\">>, ErrorMap, undefined) of");
        writer.indent();
        writer.write("undefined -> case maps:get(<<\"code\">>, ErrorMap, undefined) of");
        writer.indent();
        writer.write("undefined -> maps:get(<<\"Code\">>, ErrorMap, <<\"Unknown\">>);");
        writer.write("C -> C");
        writer.dedent();
        writer.write("end;");
        writer.write("T -> T");
        writer.dedent();
        writer.write("end,");
        writer.write("{error, {aws_error, StatusCode, Code, Message}}");
        writer.dedent();
        writer.dedent();
        writer.write("catch");
        writer.indent();
        writer.write("_:_ -> {error, {http_error, StatusCode, ErrorBody}}");
        writer.dedent();
        writer.write("end");
        writer.dedent();
        writer.dedent();
        writer.write("end;");
    }
    
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
}
