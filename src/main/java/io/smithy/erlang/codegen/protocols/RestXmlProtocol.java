package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.ErlangSymbolProvider;
import io.smithy.erlang.codegen.ErlangWriter;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.MemberShape;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.Shape;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.shapes.StructureShape;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpPayloadTrait;
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.RequiredTrait;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * REST-XML Protocol implementation for S3, CloudFront, Route 53, SES, and other REST-XML AWS services.
 * 
 * Key characteristics:
 * - RESTful HTTP methods (GET, PUT, POST, DELETE) from @http trait
 * - URI patterns from @http trait (may contain {label} placeholders)
 * - Path parameters from @httpLabel
 * - Query parameters from @httpQuery
 * - Headers from @httpHeader
 * - XML request/response bodies (encoded/decoded with aws_xml)
 * - AWS SigV4 signing
 * 
 * Request format:
 *   - Method and URI from @http trait
 *   - Path parameters substituted in URI
 *   - Query string from @httpQuery members
 *   - Headers from @httpHeader members
 *   - Body: XML encoded structure or @httpPayload member
 * 
 * Response format:
 *   - XML body decoded to map
 *   - Headers extracted from @httpHeader members in output
 */
public class RestXmlProtocol implements Protocol {
    
    /**
     * Creates a REST-XML protocol handler.
     */
    public RestXmlProtocol() {
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
    public void generateOperation(
            OperationShape operation,
            ServiceShape service,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangWriter writer) {
        
        String opName = ErlangSymbolProvider.toErlangName(operation.getId().getName());
        
        // Determine input and output types
        String inputType = "map()";
        String outputType = "map()";
        
        if (operation.getInput().isPresent()) {
            ShapeId inputId = operation.getInput().get();
            String inputRecordName = ErlangSymbolProvider.toErlangName(inputId.getName());
            inputType = inputRecordName + "()";
        }
        
        if (operation.getOutput().isPresent()) {
            ShapeId outputId = operation.getOutput().get();
            String outputRecordName = ErlangSymbolProvider.toErlangName(outputId.getName());
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
        writer.writeComment("Options:");
        writer.writeComment("  - enable_retry: Enable automatic retry (default: true)");
        writer.writeComment("  - max_retries: Maximum retry attempts (default: 3)");
        writer.writeComment("  - initial_backoff: Initial backoff in ms (default: 100)");
        writer.writeComment("  - max_backoff: Maximum backoff in ms (default: 20000)");
        writer.write("-spec $L(Client :: map(), Input :: $L, Options :: map()) -> {ok, $L} | {error, term()}.",
                opName, inputType, outputType);
        writer.write("$L(Client, Input, Options) when is_map(Input), is_map(Options) ->", opName);
        writer.indent();
        
        // Create the request function
        String internalFunctionName = "make_" + opName + "_request";
        writer.write("RequestFun = fun() -> $L(Client, Input) end,", internalFunctionName);
        writer.write("");
        
        // Check if retry is enabled
        writer.write("case maps:get(enable_retry, Options, true) of");
        writer.indent();
        writer.write("true ->");
        writer.indent();
        writer.write("%% Retry enabled - wrap with retry logic");
        writer.write("aws_retry:with_retry(RequestFun, Options);");
        writer.dedent();
        writer.write("false ->");
        writer.indent();
        writer.write("%% Retry disabled - call directly");
        writer.write("RequestFun()");
        writer.dedent();
        writer.dedent();
        writer.write("end.");
        writer.dedent();
        writer.write("");
        
        // Generate internal request function
        writer.writeComment("Internal function to make the " + operation.getId().getName() + " request");
        writer.write("-spec $L(Client :: map(), Input :: $L) -> {ok, $L} | {error, term()}.",
                internalFunctionName, inputType, outputType);
        writer.write("$L(Client, Input) when is_map(Input) ->", internalFunctionName);
        writer.indent();
        
        // Get HTTP method and URI from @http trait
        String method = ProtocolUtils.getHttpMethod(operation, "POST");
        String uri = ProtocolUtils.getHttpUri(operation, "/");
        
        if (method == null) {
            method = "POST"; // Default fallback
        }
        if (uri == null) {
            uri = "/"; // Default fallback
        }
        
        // Get HTTP binding members
        Optional<StructureShape> inputShape = ProtocolUtils.getInputShape(operation, model);
        List<MemberShape> httpLabelMembers = inputShape
            .map(ProtocolUtils::getLabelMembers)
            .orElse(java.util.Collections.emptyList());
        List<MemberShape> httpQueryMembers = inputShape
            .map(ProtocolUtils::getQueryMembers)
            .orElse(java.util.Collections.emptyList());
        List<MemberShape> httpHeaderInputMembers = inputShape
            .map(ProtocolUtils::getHeaderMembers)
            .orElse(java.util.Collections.emptyList());
        
        // Check if this is an S3 service (uses aws_s3:build_url for proper bucket routing)
        boolean isS3Service = isS3Service(service);
        boolean useS3Url = isS3Service && hasS3BucketParameter(httpLabelMembers);
        
        writer.write("Method = <<\"$L\">>,", method);
        // Only generate Endpoint if not using S3 URL builder (S3 extracts endpoint from Client)
        if (!useS3Url) {
            writer.write("Endpoint = maps:get(endpoint, Client),");
        }
        writer.write("");
        
        // Build query string first (needed for both S3 and generic URL building)
        if (httpQueryMembers.isEmpty()) {
            writer.write("%% No query parameters");
            writer.write("QueryString = <<\"\">>,");
        } else {
            writer.write("%% Build query string from @httpQuery parameters");
            writer.write("QueryPairs0 = [],");
            
            for (int i = 0; i < httpQueryMembers.size(); i++) {
                MemberShape member = httpQueryMembers.get(i);
                String memberName = member.getMemberName();
                String erlangFieldName = ErlangSymbolProvider.toErlangName(memberName);
                HttpQueryTrait queryTrait = member.expectTrait(HttpQueryTrait.class);
                String queryName = queryTrait.getValue();
                if (queryName == null || queryName.isEmpty()) {
                    queryName = memberName;
                }
                String currentPairsVar = "QueryPairs" + i;
                String nextPairsVar = "QueryPairs" + (i + 1);
                
                writer.write("$L = case maps:get(<<\"$L\">>, Input, undefined) of",
                        nextPairsVar, memberName);
                writer.indent();
                writer.write("undefined -> $L;", currentPairsVar);
                writer.write("$LVal -> [{<<\"$L\">>, ensure_binary($LVal)} | $L]",
                        capitalize(erlangFieldName), queryName, capitalize(erlangFieldName), currentPairsVar);
                writer.dedent();
                writer.write("end,");
            }
            
            writer.write("");
            String finalPairsVar = "QueryPairs" + httpQueryMembers.size();
            writer.write("QueryString = case $L of", finalPairsVar);
            writer.indent();
            writer.write("[] -> <<\"\">>;");
            writer.write("Pairs ->");
            writer.indent();
            writer.write("Encoded = uri_string:compose_query(Pairs),");
            writer.write("<<\"?\", Encoded/binary>>");
            writer.dedent();
            writer.dedent();
            writer.write("end,");
        }
        
        writer.write("");
        
        // Generate URL - use aws_s3:build_url for S3 services with Bucket parameter
        if (useS3Url) {
            generateS3Url(httpLabelMembers, writer);
        } else {
            // Generic URL building for non-S3 REST-XML services
            generateGenericUrl(uri, httpLabelMembers, writer);
        }
        
        writer.write("");
        
        // Generate request body based on @httpPayload
        Optional<MemberShape> payloadMember = inputShape.flatMap(ProtocolUtils::getPayloadMember);
        String contentType;
        
        if (!payloadMember.isPresent()) {
            // No @httpPayload - encode entire structure as XML
            if (!inputShape.isPresent()) {
                // No input shape - empty body
                writer.write("%% No input - empty request body");
                writer.write("Body = <<\"\">>,");
                contentType = "application/xml";
            } else {
                List<MemberShape> bodyMembers = ProtocolUtils.getBodyMembers(inputShape.get());
                
                if (bodyMembers.isEmpty()) {
                    writer.write("%% No body members - empty request body");
                    writer.write("Body = <<\"\">>,");
                    contentType = "application/xml";
                } else {
                    writer.write("%% No @httpPayload - encode body members as XML");
                    writer.write("BodyMap0 = #{},");
                    int bodyMemberIndex = 0;
                    for (MemberShape member : bodyMembers) {
                        String memberName = member.getMemberName();
                        String currentMapVar = "BodyMap" + bodyMemberIndex;
                        String nextMapVar = "BodyMap" + (bodyMemberIndex + 1);
                        writer.write("$L = case maps:get(<<\"$L\">>, Input, undefined) of",
                                nextMapVar, memberName);
                        writer.indent();
                        writer.write("undefined -> $L;", currentMapVar);
                        writer.write("Val -> $L#{<<\"$L\">> => Val}", currentMapVar, memberName);
                        writer.dedent();
                        writer.write("end,");
                        bodyMemberIndex++;
                    }
                    String finalMapVar = "BodyMap" + bodyMemberIndex;
                    writer.write("Body = iolist_to_binary(aws_xml:encode($L)),", finalMapVar);
                    contentType = "application/xml";
                }
            }
        } else {
            // @httpPayload present - extract member and handle by type
            MemberShape payload = payloadMember.get();
            String memberName = payload.getMemberName();
            Shape targetShape = model.expectShape(payload.getTarget());
            
            writer.write("%% @httpPayload - extract payload member");
            if (targetShape.isBlobShape()) {
                writer.write("Body = maps:get(<<\"$L\">>, Input),", memberName);
                contentType = "application/octet-stream";
            } else if (targetShape.isStringShape()) {
                writer.write("PayloadString = maps:get(<<\"$L\">>, Input),", memberName);
                writer.write("Body = ensure_binary(PayloadString),");
                contentType = "text/plain";
            } else {
                // Structure payload - encode as XML
                writer.write("PayloadData = maps:get(<<\"$L\">>, Input),", memberName);
                writer.write("Body = iolist_to_binary(aws_xml:encode(PayloadData)),");
                contentType = "application/xml";
            }
        }
        
        writer.write("");
        
        // Generate headers with @httpHeader members
        if (httpHeaderInputMembers.isEmpty()) {
            writer.write("%% Headers");
            writer.write("Headers = [{<<\"Content-Type\">>, <<\"$L\">>}],", contentType);
        } else {
            writer.write("%% Build headers with @httpHeader members");
            writer.write("Headers0 = [{<<\"Content-Type\">>, <<\"$L\">>}],", contentType);
            
            for (int i = 0; i < httpHeaderInputMembers.size(); i++) {
                MemberShape member = httpHeaderInputMembers.get(i);
                String memberName = member.getMemberName();
                String erlangFieldName = ErlangSymbolProvider.toErlangName(memberName);
                HttpHeaderTrait headerTrait = member.expectTrait(HttpHeaderTrait.class);
                String headerName = headerTrait.getValue();
                if (headerName == null || headerName.isEmpty()) {
                    headerName = memberName; // Fallback to member name if header name not specified
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
                    writer.write("$L = case maps:get(<<\"$L\">>, Input, undefined) of",
                            nextHeadersVar, memberName);
                    writer.indent();
                    writer.write("undefined -> $L;", currentHeadersVar);
                    writer.write("$LValue -> [{<<\"$L\">>, ensure_binary($LValue)} | $L]",
                            capitalize(erlangFieldName), headerName, capitalize(erlangFieldName), currentHeadersVar);
                    writer.dedent();
                    writer.write("end,");
                }
            }
            
            String finalHeadersVar = "Headers" + httpHeaderInputMembers.size();
            writer.write("Headers = $L,", finalHeadersVar);
        }
        
        writer.write("");
        
        // Make the request with SigV4 signing
        writer.write("%% Sign and send request");
        writer.write("case aws_sigv4:sign_request(Method, Url, Headers, Body, Client) of");
        writer.indent();
        writer.write("{ok, SignedHeaders} ->");
        writer.indent();
        writer.write("ContentType = \"$L\",", contentType);
        writer.write("%% Convert binary headers to string format for httpc");
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
        
        // Check for output headers
        Optional<StructureShape> outputShape = ProtocolUtils.getOutputShape(operation, model);
        List<MemberShape> httpHeaderOutputMembers = outputShape
            .map(ProtocolUtils::getHeaderMembers)
            .orElse(java.util.Collections.emptyList());
        
        if (httpHeaderOutputMembers.isEmpty()) {
            writer.write("{ok, {{_, 200, _}, _, ResponseBody}} ->");
        } else {
            writer.write("{ok, {{_, 200, _}, ResponseHeaders, ResponseBody}} ->");
        }
        writer.indent();
        
        // Generate response decoding
        generateResponseDecoding(operation, model, writer, httpHeaderOutputMembers);
        
        writer.dedent();
        writer.write("{ok, {{_, StatusCode, _}, _RespHeaders, ErrorBody}} ->");
        writer.indent();
        writer.write("%% Parse REST-XML error response");
        writer.write("case aws_xml:decode(ErrorBody) of");
        writer.indent();
        writer.write("{ok, ErrorMap} ->");
        writer.indent();
        writer.write("case ErrorMap of");
        writer.indent();
        writer.write("#{<<\"Error\">> := Error} ->");
        writer.indent();
        writer.write("Code = maps:get(<<\"Code\">>, Error, <<\"Unknown\">>),");
        writer.write("Message = maps:get(<<\"Message\">>, Error, <<\"Unknown error\">>),");
        writer.write("{error, {aws_error, StatusCode, Code, Message}};");
        writer.dedent();
        writer.write("_ ->");
        writer.indent();
        writer.write("{error, {http_error, StatusCode, ErrorBody}}");
        writer.dedent();
        writer.dedent();
        writer.write("end;");
        writer.dedent();
        writer.write("{error, _} ->");
        writer.indent();
        writer.write("{error, {http_error, StatusCode, ErrorBody}}");
        writer.dedent();
        writer.dedent();
        writer.write("end;");
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
        writer.dedent();
        writer.write("");
    }
    
    @Override
    public void generateHeaders(
            OperationShape operation,
            ServiceShape service,
            ErlangWriter writer) {
        
        // Headers are generated in generateOperation() with @httpHeader members
        // This method is not used for REST protocols
        writer.writeComment("REST-XML protocol headers (generated in operation)");
        writer.write("Headers = [],");
    }
    
    @Override
    public void generateRequestBody(
            OperationShape operation,
            Model model,
            ErlangWriter writer) {
        
        // Request body is generated in generateOperation() with XML encoding
        // This method is not used for REST protocols
        writer.writeComment("REST-XML protocol request body (generated in operation)");
        writer.write("Body = <<\"\">>,");
    }
    
    private void generateResponseDecoding(
            OperationShape operation,
            Model model,
            ErlangWriter writer,
            List<MemberShape> httpHeaderOutputMembers) {
        
        Optional<StructureShape> outputShape = ProtocolUtils.getOutputShape(operation, model);
        Optional<MemberShape> outputPayloadMember = outputShape.flatMap(ProtocolUtils::getPayloadMember);
        
        if (httpHeaderOutputMembers.isEmpty()) {
            // No output headers to extract
            if (!outputPayloadMember.isPresent()) {
                // No @httpPayload - parse entire response as XML
                writer.write("case aws_xml:decode(ResponseBody) of");
                writer.indent();
                writer.write("{ok, XmlMap} ->");
                writer.indent();
                writer.write("{ok, XmlMap};");
                writer.dedent();
                writer.write("{error, DecodeError} ->");
                writer.indent();
                writer.write("{error, {xml_decode_error, DecodeError}}");
                writer.dedent();
                writer.dedent();
                writer.write("end;");
            } else {
                // @httpPayload present - handle by type
                MemberShape payload = outputPayloadMember.get();
                String memberName = payload.getMemberName();
                Shape targetShape = model.expectShape(payload.getTarget());
                
                if (targetShape.isBlobShape()) {
                    writer.write("{ok, #{<<\"$L\">> => ResponseBody}};", memberName);
                } else if (targetShape.isStringShape()) {
                    writer.write("{ok, #{<<\"$L\">> => ResponseBody}};", memberName);
                } else {
                    writer.write("case aws_xml:decode(ResponseBody) of");
                    writer.indent();
                    writer.write("{ok, XmlMap} ->");
                    writer.indent();
                    writer.write("{ok, #{<<\"$L\">> => XmlMap}};", memberName);
                    writer.dedent();
                    writer.write("{error, DecodeError} ->");
                    writer.indent();
                    writer.write("{error, {xml_decode_error, DecodeError}}");
                    writer.dedent();
                    writer.dedent();
                    writer.write("end;");
                }
            }
        } else {
            // Extract @httpHeader members from response
            // Handle response body based on @httpPayload
            if (!outputPayloadMember.isPresent()) {
                // No @httpPayload - parse entire response as XML (if not empty)
                // Some operations (like S3 PutObject) return empty body with headers only
                writer.write("Output0 = case ResponseBody of");
                writer.indent();
                writer.write("<<>> -> #{};");
                writer.write("_ ->");
                writer.indent();
                writer.write("case aws_xml:decode(ResponseBody) of");
                writer.indent();
                writer.write("{ok, XmlMap} -> XmlMap;");
                writer.write("{error, _} -> #{}");
                writer.dedent();
                writer.write("end");
                writer.dedent();
                writer.dedent();
                writer.write("end,");
            } else {
                // @httpPayload present - handle by type
                MemberShape payload = outputPayloadMember.get();
                String memberName = payload.getMemberName();
                Shape targetShape = model.expectShape(payload.getTarget());
                
                if (targetShape.isBlobShape()) {
                    writer.write("Output0 = #{<<\"$L\">> => ResponseBody},", memberName);
                } else if (targetShape.isStringShape()) {
                    writer.write("Output0 = #{<<\"$L\">> => ResponseBody},", memberName);
                } else {
                    // Structure payload - parse XML
                    writer.write("case aws_xml:decode(ResponseBody) of");
                    writer.indent();
                    writer.write("{ok, XmlMap} ->");
                    writer.indent();
                    writer.write("Output0 = #{<<\"$L\">> => XmlMap},", memberName);
                    writer.write("");
                    writer.write("%% Extract @httpHeader members from response");
                    
                    for (int i = 0; i < httpHeaderOutputMembers.size(); i++) {
                        MemberShape member = httpHeaderOutputMembers.get(i);
                        String memberName2 = member.getMemberName();
                        String erlangFieldName = ErlangSymbolProvider.toErlangName(memberName2);
                        HttpHeaderTrait headerTrait = member.expectTrait(HttpHeaderTrait.class);
                        String headerName = headerTrait.getValue();
                        if (headerName == null || headerName.isEmpty()) {
                            headerName = memberName2; // Fallback to member name if header name not specified
                        }
                        String headerNameLower = headerName.toLowerCase();
                        
                        // Use unique variable names to avoid "unsafe variable in case" error
                        // W1_N for keyfind result, W2_N for case match
                        String w1Var = "W1_" + i;
                        String w2Var = "W2_" + i;
                        writer.write("$LValue = case lists:keyfind(\"$L\", 1, ResponseHeaders) of",
                                capitalize(erlangFieldName), headerNameLower);
                        writer.indent();
                        writer.write("{_, $L} -> list_to_binary($L);", w1Var, w1Var);
                        writer.write("false -> undefined");
                        writer.dedent();
                        writer.write("end,");
                        String currentOutputVar2 = "Output" + i;
                        String nextOutputVar2 = "Output" + (i + 1);
                        writer.write("$L = case $LValue of",
                                nextOutputVar2, capitalize(erlangFieldName));
                        writer.indent();
                        writer.write("undefined -> $L;", currentOutputVar2);
                        writer.write("$L -> $L#{<<\"$L\">> => $L}", w2Var, currentOutputVar2, memberName2, w2Var);
                        writer.dedent();
                        writer.write("end,");
                    }
                    
                    String finalOutputVar2 = "Output" + httpHeaderOutputMembers.size();
                    writer.write("{ok, $L}", finalOutputVar2);
                    writer.dedent();
                    writer.dedent();
                    writer.write("end;");
                    writer.write("{error, DecodeError} ->");
                    writer.indent();
                    writer.write("{error, {xml_decode_error, DecodeError}}");
                    writer.dedent();
                    writer.dedent();
                    writer.write("end;");
                    return; // Early return - structure payload with headers already handled
                }
            }
            
            writer.write("");
            writer.write("%% Extract @httpHeader members from response");
            
            for (int i = 0; i < httpHeaderOutputMembers.size(); i++) {
                MemberShape member = httpHeaderOutputMembers.get(i);
                String memberName = member.getMemberName();
                String erlangFieldName = ErlangSymbolProvider.toErlangName(memberName);
                HttpHeaderTrait headerTrait = member.expectTrait(HttpHeaderTrait.class);
                String headerName = headerTrait.getValue();
                if (headerName == null || headerName.isEmpty()) {
                    headerName = memberName; // Fallback to member name if header name not specified
                }
                String headerNameLower = headerName.toLowerCase();
                String currentOutputVar = "Output" + i;
                String nextOutputVar = "Output" + (i + 1);
                
                // Use unique variable names to avoid "unsafe variable in case" error
                // V1_N for keyfind result, V2_N for case match
                String v1Var = "V1_" + i;
                String v2Var = "V2_" + i;
                writer.write("$LValue = case lists:keyfind(\"$L\", 1, ResponseHeaders) of",
                        capitalize(erlangFieldName), headerNameLower);
                writer.indent();
                writer.write("{_, $L} -> list_to_binary($L);", v1Var, v1Var);
                writer.write("false -> undefined");
                writer.dedent();
                writer.write("end,");
                writer.write("$L = case $LValue of",
                        nextOutputVar, capitalize(erlangFieldName));
                writer.indent();
                writer.write("undefined -> $L;", currentOutputVar);
                writer.write("$L -> $L#{<<\"$L\">> => $L}", v2Var, currentOutputVar, memberName, v2Var);
                writer.dedent();
                writer.write("end,");
            }
            
            // Return the final output
            // Note: When !outputPayloadMember.isPresent(), we use inline case expression
            // for ResponseBody handling (handles empty bodies gracefully), so no case to close
            String finalOutputVar = "Output" + httpHeaderOutputMembers.size();
            writer.write("{ok, $L};", finalOutputVar);
        }
    }
    
    @Override
    public void generateResponseDecoding(
            OperationShape operation,
            Model model,
            ErlangWriter writer) {
        
        // This method is not used for REST protocols
        // Response decoding is handled in generateOperation()
        writer.writeComment("REST-XML protocol response decoding (generated in operation)");
        writer.write("{ok, #{}};");
    }
    
    /**
     * Capitalizes the first letter of a string.
     */
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
    
    /**
     * Checks if the service is S3 or S3-like.
     * S3 services need special URL routing (bucket-in-hostname vs path-style).
     */
    private boolean isS3Service(ServiceShape service) {
        String serviceName = service.getId().getName().toLowerCase();
        return serviceName.contains("s3") || 
               serviceName.equals("simplestorage") ||
               serviceName.equals("simplesstorageservice");
    }
    
    /**
     * Checks if the operation has a Bucket parameter (typical S3 pattern).
     */
    private boolean hasS3BucketParameter(List<MemberShape> httpLabelMembers) {
        return httpLabelMembers.stream()
            .anyMatch(m -> m.getMemberName().equalsIgnoreCase("Bucket"));
    }
    
    /**
     * Generates S3-specific URL building using aws_s3:build_url/4.
     * This handles virtual-hosted-style vs path-style URL routing.
     */
    private void generateS3Url(List<MemberShape> httpLabelMembers, ErlangWriter writer) {
        writer.write("%% S3-specific URL building with bucket routing");
        writer.write("%% Uses aws_s3:build_url/4 for virtual-hosted/path-style auto-detection");
        
        // Extract Bucket parameter
        Optional<MemberShape> bucketMember = httpLabelMembers.stream()
            .filter(m -> m.getMemberName().equalsIgnoreCase("Bucket"))
            .findFirst();
        
        // Extract Key parameter (may not exist for bucket-level operations)
        Optional<MemberShape> keyMember = httpLabelMembers.stream()
            .filter(m -> m.getMemberName().equalsIgnoreCase("Key"))
            .findFirst();
        
        if (bucketMember.isPresent()) {
            String bucketMemberName = bucketMember.get().getMemberName();
            writer.write("Bucket = maps:get(<<\"$L\">>, Input, <<>>),", bucketMemberName);
        } else {
            writer.write("Bucket = <<>>,");
        }
        
        if (keyMember.isPresent()) {
            String keyMemberName = keyMember.get().getMemberName();
            writer.write("Key = maps:get(<<\"$L\">>, Input, <<>>),", keyMemberName);
        } else {
            writer.write("Key = <<>>,");
        }
        
        writer.write("Url = aws_s3:build_url(Client, Bucket, Key, QueryString),");
    }
    
    /**
     * Generates generic URL building for non-S3 REST-XML services.
     */
    private void generateGenericUrl(String uri, List<MemberShape> httpLabelMembers, ErlangWriter writer) {
        String uriVariable;
        if (httpLabelMembers.isEmpty()) {
            writer.write("%% No path parameters");
            writer.write("Uri = <<\"$L\">>,", uri);
            uriVariable = "Uri";
        } else {
            writer.write("%% Build URL with path parameters");
            writer.write("Uri0 = <<\"$L\">>,", uri);
            
            // Generate substitution code for each @httpLabel member
            for (int i = 0; i < httpLabelMembers.size(); i++) {
                MemberShape member = httpLabelMembers.get(i);
                String memberName = member.getMemberName();
                String erlangFieldName = ErlangSymbolProvider.toErlangName(memberName);
                String labelName = memberName;
                String currentUriVar = "Uri" + i;
                String nextUriVar = "Uri" + (i + 1);
                
                writer.write("$LValue = maps:get(<<\"$L\">>, Input),",
                        capitalize(erlangFieldName), memberName);
                writer.write("$LEncoded = url_encode(ensure_binary($LValue)),",
                        capitalize(erlangFieldName), capitalize(erlangFieldName));
                writer.write("$L = binary:replace($L, <<\"{$L}\">>, $LEncoded),",
                        nextUriVar, currentUriVar, labelName, capitalize(erlangFieldName));
            }
            
            uriVariable = "Uri" + httpLabelMembers.size();
        }
        
        writer.write("Url = <<Endpoint/binary, $L/binary, QueryString/binary>>,", uriVariable);
    }
}
