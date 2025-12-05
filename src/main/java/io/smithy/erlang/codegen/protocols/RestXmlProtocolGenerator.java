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
 * Protocol generator for REST-XML protocol.
 * 
 * <p>This generator handles code generation for services using the REST-XML
 * protocol, which is used by S3, CloudFront, Route 53, SES, and other XML-based
 * AWS services.
 * 
 * <h2>Protocol Characteristics</h2>
 * <ul>
 *   <li>HTTP Method: From {@code @http} trait (GET, PUT, POST, DELETE)</li>
 *   <li>URI Path: From {@code @http} trait with {@code @httpLabel} substitution</li>
 *   <li>Query Parameters: From {@code @httpQuery} members</li>
 *   <li>Headers: From {@code @httpHeader} members</li>
 *   <li>Content-Type: {@code application/xml}</li>
 *   <li>Request Body: XML encoded with {@code aws_xml:encode/1}</li>
 *   <li>Response Body: XML decoded with {@code aws_xml:decode/1}</li>
 *   <li>Authentication: AWS SigV4</li>
 * </ul>
 * 
 * <h2>S3 Special Handling</h2>
 * <p>For S3 services, this generator uses {@code aws_s3:build_url/4} to handle
 * virtual-hosted-style vs path-style URL routing based on the endpoint.
 * 
 * <h2>Services Using REST-XML</h2>
 * <ul>
 *   <li>Amazon S3</li>
 *   <li>Amazon CloudFront</li>
 *   <li>Amazon Route 53</li>
 *   <li>Amazon SES</li>
 * </ul>
 * 
 * @see ProtocolGenerator
 */
public class RestXmlProtocolGenerator implements ProtocolGenerator {
    
    /** Protocol trait ID for REST-XML */
    public static final ShapeId REST_XML = ShapeId.from("aws.protocols#restXml");
    
    /**
     * Creates a REST-XML protocol generator.
     */
    public RestXmlProtocolGenerator() {
    }
    
    @Override
    public ShapeId getProtocol() {
        return REST_XML;
    }
    
    @Override
    public String getName() {
        return "restXml";
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
        return "application/xml";
    }
    
    @Override
    public void generateOperation(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        Model model = context.model();
        ServiceShape service = context.serviceShape();
        
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
        writer.write("-spec $L(Client :: map(), Input :: $L) -> {ok, $L} | {error, term()}.",
                opName, inputType, outputType);
        writer.write("$L(Client, Input) ->", opName);
        writer.indent();
        writer.write("$L(Client, Input, #{}).", opName);
        writer.dedent();
        writer.write("");
        
        // Generate 3-arity version with retry support
        writer.writeComment("Calls the " + operation.getId().getName() + " operation with options");
        writer.write("-spec $L(Client :: map(), Input :: $L, Options :: map()) -> {ok, $L} | {error, term()}.",
                opName, inputType, outputType);
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
        ServiceShape service = context.serviceShape();
        String internalFunctionName = "make_" + opName + "_request";
        
        writer.writeComment("Internal function to make the " + operation.getId().getName() + " request");
        writer.write("-spec $L(Client :: map(), Input :: $L) -> {ok, $L} | {error, term()}.",
                internalFunctionName, inputType, outputType);
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
        
        // Check if this is an S3 service
        boolean isS3Service = isS3Service(service);
        boolean useS3Url = isS3Service && hasS3BucketParameter(httpLabelMembers);
        
        writer.write("Method = <<\"$L\">>,", method);
        if (!useS3Url) {
            writer.write("Endpoint = maps:get(endpoint, Client),");
        }
        writer.write("");
        
        // Build query string
        generateQueryString(httpQueryMembers, writer);
        writer.write("");
        
        // Generate URL
        if (useS3Url) {
            generateS3Url(httpLabelMembers, writer);
        } else {
            generateGenericUrl(uri, httpLabelMembers, writer);
        }
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
    
    private void generateS3Url(List<MemberShape> httpLabelMembers, ErlangWriter writer) {
        writer.write("%% S3-specific URL building with bucket routing");
        
        Optional<MemberShape> bucketMember = httpLabelMembers.stream()
            .filter(m -> m.getMemberName().equalsIgnoreCase("Bucket"))
            .findFirst();
        Optional<MemberShape> keyMember = httpLabelMembers.stream()
            .filter(m -> m.getMemberName().equalsIgnoreCase("Key"))
            .findFirst();
        
        if (bucketMember.isPresent()) {
            writer.write("Bucket = maps:get(<<\"$L\">>, Input, <<>>),", bucketMember.get().getMemberName());
        } else {
            writer.write("Bucket = <<>>,");
        }
        
        if (keyMember.isPresent()) {
            writer.write("Key = maps:get(<<\"$L\">>, Input, <<>>),", keyMember.get().getMemberName());
        } else {
            writer.write("Key = <<>>,");
        }
        
        writer.write("Url = aws_s3:build_url(Client, Bucket, Key, QueryString),");
    }
    
    private void generateGenericUrl(String uri, List<MemberShape> httpLabelMembers, ErlangWriter writer) {
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
                writer.write("Body = <<\"\">>,");
                return "application/xml";
            }
            
            List<MemberShape> bodyMembers = ProtocolUtils.getBodyMembers(inputShape.get());
            if (bodyMembers.isEmpty()) {
                writer.write("%% No body members - empty request body");
                writer.write("Body = <<\"\">>,");
                return "application/xml";
            }
            
            writer.write("%% Encode body members as XML");
            writer.write("BodyMap0 = #{},");
            for (int i = 0; i < bodyMembers.size(); i++) {
                MemberShape member = bodyMembers.get(i);
                String memberName = member.getMemberName();
                String currentMapVar = "BodyMap" + i;
                String nextMapVar = "BodyMap" + (i + 1);
                writer.write("$L = case maps:get(<<\"$L\">>, Input, undefined) of", nextMapVar, memberName);
                writer.indent();
                writer.write("undefined -> $L;", currentMapVar);
                writer.write("Val -> $L#{<<\"$L\">> => Val}", currentMapVar, memberName);
                writer.dedent();
                writer.write("end,");
            }
            writer.write("Body = iolist_to_binary(aws_xml:encode($L)),", "BodyMap" + bodyMembers.size());
            return "application/xml";
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
            writer.write("Body = iolist_to_binary(aws_xml:encode(maps:get(<<\"$L\">>, Input))),", memberName);
            return "application/xml";
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
        writer.write("_ -> {binary_to_list(Url), StringHeaders, ContentType, Body}");
        writer.dedent();
        writer.write("end,");
        writer.write("");
        writer.write("case httpc:request(binary_to_atom(string:lowercase(Method), utf8), Request, [], [{body_format, binary}]) of");
        writer.indent();
        writer.write("{ok, {{_, 200, _}, _RespHeaders, ResponseBody}} ->");
        writer.indent();
        generateResponseDeserializerInternal(operation, model, writer);
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
    
    /**
     * Internal helper for response deserialization that takes model directly.
     */
    private void generateResponseDeserializerInternal(OperationShape operation, Model model, ErlangWriter writer) {
        // Check if output has @httpPayload pointing to blob/string
        Optional<StructureShape> outputShape = operation.getOutput()
                .map(id -> model.expectShape(id, StructureShape.class));
        
        if (outputShape.isPresent()) {
            Optional<MemberShape> payloadMember = ProtocolUtils.getPayloadMember(outputShape.get());
            if (payloadMember.isPresent()) {
                Shape targetShape = model.expectShape(payloadMember.get().getTarget());
                if (targetShape.isBlobShape() || targetShape.isStringShape()) {
                    // Return raw body for blob/string payloads
                    String memberName = payloadMember.get().getMemberName();
                    writer.write("%% Return raw body as payload");
                    writer.write("{ok, #{<<\"$L\">> => ResponseBody}};", memberName);
                    return;
                }
            }
            
            // Check if there are any body members (excluding header members)
            List<MemberShape> bodyMembers = ProtocolUtils.getBodyMembers(outputShape.get());
            if (bodyMembers.isEmpty()) {
                // No body members - return empty map or just headers
                writer.write("%% No body content expected");
                writer.write("{ok, #{}};");
                return;
            }
        }
        
        // Check for empty response (PutObject, DeleteObject, etc.)
        // These operations may return 200 with empty body
        writer.write("%% Decode XML response (or handle empty)");
        writer.write("case ResponseBody of");
        writer.indent();
        writer.write("<<>> -> {ok, #{}};");
        writer.write("_ ->");
        writer.indent();
        writer.write("case aws_xml:decode(ResponseBody) of");
        writer.indent();
        writer.write("{ok, XmlMap} -> {ok, XmlMap};");
        writer.write("{error, DecodeError} -> {error, {xml_decode_error, DecodeError}}");
        writer.dedent();
        writer.write("end");
        writer.dedent();
        writer.dedent();
        writer.write("end;");
    }
    
    @Override
    public void generateHeaders(OperationShape operation, ServiceShape service,
            ErlangWriter writer, ErlangContext context) {
        // Headers are generated inline in generateOperation()
        writer.writeComment("REST-XML headers (generated in operation)");
    }
    
    @Override
    public void generateRequestSerializer(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        writer.writeComment("REST-XML request serialization uses aws_xml:encode/1");
        writer.write("Body = iolist_to_binary(aws_xml:encode(Input)),");
    }
    
    @Override
    public void generateResponseDeserializer(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        if (context != null) {
            generateResponseDeserializerInternal(operation, context.model(), writer);
        } else {
            // Fallback for standalone calls - simple XML decode
            writer.write("%% Decode XML response");
            writer.write("case aws_xml:decode(ResponseBody) of");
            writer.indent();
            writer.write("{ok, XmlMap} -> {ok, XmlMap};");
            writer.write("{error, DecodeError} -> {error, {xml_decode_error, DecodeError}}");
            writer.dedent();
            writer.write("end;");
        }
    }
    
    @Override
    public void generateErrorParser(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        writer.write("%% Parse REST-XML error response");
        writer.write("case aws_xml:decode(ErrorBody) of");
        writer.indent();
        writer.write("{ok, #{<<\"Error\">> := Error}} ->");
        writer.indent();
        writer.write("Code = maps:get(<<\"Code\">>, Error, <<\"Unknown\">>),");
        writer.write("Message = maps:get(<<\"Message\">>, Error, <<\"Unknown error\">>),");
        writer.write("{error, {aws_error, StatusCode, Code, Message}};");
        writer.dedent();
        writer.write("{ok, _} -> {error, {http_error, StatusCode, ErrorBody}};");
        writer.write("{error, _} -> {error, {http_error, StatusCode, ErrorBody}}");
        writer.dedent();
        writer.write("end;");
    }
    
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
    
    private boolean isS3Service(ServiceShape service) {
        String serviceName = service.getId().getName().toLowerCase();
        return serviceName.contains("s3") || 
               serviceName.equals("simplestorage") ||
               serviceName.equals("simplestorageservice");
    }
    
    private boolean hasS3BucketParameter(List<MemberShape> httpLabelMembers) {
        return httpLabelMembers.stream()
            .anyMatch(m -> m.getMemberName().equalsIgnoreCase("Bucket"));
    }
}
