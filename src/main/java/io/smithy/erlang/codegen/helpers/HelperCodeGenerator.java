package io.smithy.erlang.codegen.helpers;

import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.*;
import java.util.*;

/**
 * Utility class for generating helper-based Erlang code.
 * 
 * This class provides static methods to generate Erlang code that uses
 * the runtime helper modules (runtime_http_request, runtime_http_response, runtime_types)
 * instead of inline code generation.
 * 
 * @author Smithy Erlang Code Generator
 * @version 1.0.0
 */
public class HelperCodeGenerator {
    
    /**
     * Generate URI building code using runtime_http_request:build_uri/3
     * 
     * @param httpTrait HTTP trait containing URI pattern
     * @param operation Operation shape
     * @param inputVarName Variable name for input map
     * @return Generated Erlang code for URI building
     */
    public static String generateUriBuildingCode(
            HttpTrait httpTrait, 
            OperationShape operation,
            String inputVarName) {
        
        StringBuilder code = new StringBuilder();
        String uriPattern = httpTrait.getUri().toString();
        
        code.append("    %% Build URI using helper\n");
        code.append("    UriPattern = <<\"").append(uriPattern).append("\">>,\n");
        code.append("    PathParams = [\n");
        
        // Extract path parameters
        List<String> pathParams = extractPathParameters(uriPattern);
        for (int i = 0; i < pathParams.size(); i++) {
            String param = pathParams.get(i);
            code.append("        {<<\"").append(param).append("\">>, ")
                .append("maps:get(<<\"").append(param).append("\">>, ")
                .append(inputVarName).append(")}");
            if (i < pathParams.size() - 1) {
                code.append(",");
            }
            code.append("\n");
        }
        
        code.append("    ],\n");
        code.append("    Url = runtime_http_request:build_uri(UriPattern, PathParams, Endpoint)");
        
        return code.toString();
    }
    
    /**
     * Generate header building code using runtime_http_request:build_headers/2
     * 
     * @param operation Operation shape
     * @param inputShape Input structure shape
     * @param inputVarName Variable name for input map
     * @return Generated Erlang code for header building
     */
    public static String generateHeaderBuildingCode(
            OperationShape operation,
            StructureShape inputShape,
            String inputVarName) {
        
        StringBuilder code = new StringBuilder();
        
        code.append("    %% Build headers using helper\n");
        code.append("    BaseHeaders = [{<<\"Content-Type\">>, <<\"application/json\">>}],\n");
        code.append("    HeaderMapping = [\n");
        
        // Find members with httpHeader trait
        List<MemberShape> headerMembers = findHeaderMembers(inputShape);
        for (int i = 0; i < headerMembers.size(); i++) {
            MemberShape member = headerMembers.get(i);
            String headerName = getHttpHeaderName(member);
            String memberName = member.getMemberName();
            boolean required = member.isRequired();
            
            code.append("        {<<\"").append(headerName).append("\">>, ")
                .append("<<\"").append(memberName).append("\">>, ")
                .append(required).append("}");
            
            if (i < headerMembers.size() - 1) {
                code.append(",");
            }
            code.append("\n");
        }
        
        code.append("    ],\n");
        code.append("    Headers = BaseHeaders ++ runtime_http_request:build_headers(HeaderMapping, ")
            .append(inputVarName).append(")");
        
        return code.toString();
    }
    
    /**
     * Generate query building code using runtime_http_request:build_query/2
     * 
     * @param operation Operation shape
     * @param inputShape Input structure shape
     * @param inputVarName Variable name for input map
     * @return Generated Erlang code for query building
     */
    public static String generateQueryBuildingCode(
            OperationShape operation,
            StructureShape inputShape,
            String inputVarName) {
        
        StringBuilder code = new StringBuilder();
        
        List<MemberShape> queryMembers = findQueryMembers(inputShape);
        if (queryMembers.isEmpty()) {
            return "    Query = <<\"\">>"; // No query parameters
        }
        
        code.append("    %% Build query using helper\n");
        code.append("    QueryMapping = [\n");
        
        for (int i = 0; i < queryMembers.size(); i++) {
            MemberShape member = queryMembers.get(i);
            String queryName = getHttpQueryName(member);
            String memberName = member.getMemberName();
            boolean required = member.isRequired();
            
            code.append("        {<<\"").append(memberName).append("\">>, ")
                .append("<<\"").append(queryName).append("\">>, ")
                .append(required).append("}");
            
            if (i < queryMembers.size() - 1) {
                code.append(",");
            }
            code.append("\n");
        }
        
        code.append("    ],\n");
        code.append("    Query = runtime_http_request:build_query(QueryMapping, ")
            .append(inputVarName).append(")");
        
        return code.toString();
    }
    
    /**
     * Generate HTTP request execution using runtime_http_request:execute_request/5
     * 
     * @return Generated Erlang code for request execution
     */
    public static String generateRequestExecutionCode() {
        return "    case runtime_http_request:execute_request(Client, Method, Url, Headers, Body) of\n" +
               "        {ok, {StatusCode, ResponseHeaders, ResponseBody}} ->\n" +
               "            %% Parse response\n" +
               "            parse_response(StatusCode, ResponseHeaders, ResponseBody);\n" +
               "        {error, Reason} ->\n" +
               "            {error, Reason}\n" +
               "    end";
    }
    
    /**
     * Generate response parsing using runtime_http_response:parse_response/6
     * 
     * @param operation Operation shape
     * @param outputShape Output structure shape
     * @param expectedStatusCode Expected HTTP status code for success
     * @return Generated Erlang code for response parsing
     */
    public static String generateResponseParsingCode(
            OperationShape operation,
            StructureShape outputShape,
            int expectedStatusCode) {
        
        StringBuilder code = new StringBuilder();
        
        code.append("    ExpectedStatus = ").append(expectedStatusCode).append(",\n");
        code.append("    ResponseHeaderMapping = [\n");
        
        // Find members with httpHeader trait in output
        List<MemberShape> headerMembers = findHeaderMembers(outputShape);
        for (int i = 0; i < headerMembers.size(); i++) {
            MemberShape member = headerMembers.get(i);
            String headerName = getHttpHeaderName(member);
            String memberName = member.getMemberName();
            
            code.append("        {<<\"").append(headerName.toLowerCase()).append("\">>, ")
                .append("<<\"").append(memberName).append("\">>}");
            
            if (i < headerMembers.size() - 1) {
                code.append(",");
            }
            code.append("\n");
        }
        
        code.append("    ],\n");
        
        // Determine body type
        String bodyType = determineBodyType(outputShape);
        code.append("    BodyType = ").append(bodyType).append(",\n");
        
        code.append("    runtime_http_response:parse_response(\n");
        code.append("        StatusCode, ResponseHeaders, ResponseBody,\n");
        code.append("        ExpectedStatus, ResponseHeaderMapping, BodyType\n");
        code.append("    )");
        
        return code.toString();
    }
    
    // ===== Helper Methods =====
    
    /**
     * Extract path parameters from URI pattern
     * 
     * @param uriPattern URI pattern with {param} placeholders
     * @return List of parameter names
     */
    private static List<String> extractPathParameters(String uriPattern) {
        List<String> params = new ArrayList<>();
        int start = 0;
        while ((start = uriPattern.indexOf("{", start)) != -1) {
            int end = uriPattern.indexOf("}", start);
            if (end != -1) {
                params.add(uriPattern.substring(start + 1, end));
                start = end + 1;
            } else {
                break;
            }
        }
        return params;
    }
    
    /**
     * Find all members with httpHeader trait
     * 
     * @param shape Structure shape to search
     * @return List of member shapes with httpHeader trait
     */
    private static List<MemberShape> findHeaderMembers(StructureShape shape) {
        List<MemberShape> members = new ArrayList<>();
        for (MemberShape member : shape.members()) {
            if (member.hasTrait(HttpHeaderTrait.class)) {
                members.add(member);
            }
        }
        return members;
    }
    
    /**
     * Find all members with httpQuery trait
     * 
     * @param shape Structure shape to search
     * @return List of member shapes with httpQuery trait
     */
    private static List<MemberShape> findQueryMembers(StructureShape shape) {
        List<MemberShape> members = new ArrayList<>();
        for (MemberShape member : shape.members()) {
            if (member.hasTrait(HttpQueryTrait.class)) {
                members.add(member);
            }
        }
        return members;
    }
    
    /**
     * Get HTTP header name from member shape
     * 
     * @param member Member shape with httpHeader trait
     * @return HTTP header name
     */
    private static String getHttpHeaderName(MemberShape member) {
        return member.getTrait(HttpHeaderTrait.class)
                    .map(HttpHeaderTrait::getValue)
                    .orElse(member.getMemberName());
    }
    
    /**
     * Get HTTP query parameter name from member shape
     * 
     * @param member Member shape with httpQuery trait
     * @return HTTP query parameter name
     */
    private static String getHttpQueryName(MemberShape member) {
        return member.getTrait(HttpQueryTrait.class)
                    .map(HttpQueryTrait::getValue)
                    .orElse(member.getMemberName());
    }
    
    /**
     * Determine the body type for response parsing
     * 
     * @param outputShape Output structure shape
     * @return Body type as string ("json", "blob", or "text")
     */
    private static String determineBodyType(StructureShape outputShape) {
        // Check for payload trait
        Optional<MemberShape> payloadMember = outputShape.members().stream()
            .filter(m -> m.hasTrait(HttpPayloadTrait.class))
            .findFirst();
        
        if (payloadMember.isPresent()) {
            // Check if it's a blob
            // This would need access to the model to resolve the target
            // For now, return generic
            return "blob";
        }
        
        // Default to JSON for structure types
        return "json";
    }
}
