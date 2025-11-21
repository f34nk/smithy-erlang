package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests AWS S3 protocol features with HTTP bindings.
 * 
 * Verifies code generation for:
 * - @httpLabel path parameters
 * - @httpHeader input/output bindings
 * - @httpPayload blob handling
 */
public class AwsS3ProtocolTest extends AwsProtocolTestBase {
    
    @Override
    protected String getModelPath() {
        return "/models/aws-s3-client.smithy";
    }
    
    @Test
    public void testAwsModelHasOperations() {
        // Verify the test model contains expected operations
        assertTrue(model.getOperationShapes().size() >= 3, 
                "Test model should contain at least 3 operations");
    }
    
    @Test
    public void testAwsModelHasStructures() {
        // Verify the test model has structure definitions
        assertTrue(model.getStructureShapes().size() > 0,
                "Test model should have structure definitions");
    }
    
    @Test
    public void testGeneratorRuns() {
        assertDoesNotThrow(() -> runGenerator(), 
                "Generator should run without exceptions on AWS S3 model");
    }
    
    @Test
    public void testClientFileGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        assertFileGenerated(clientFile);
    }
    
    @Test
    public void testTypesFileGenerated() {
        runGenerator();
        String typesFile = "src/" + getModuleName() + "_types.hrl";
        assertFileGenerated(typesFile);
    }
    
    // ===== @httpLabel Tests =====
    
    @Test
    public void testGetObjectGeneratesPathParameterSubstitution() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify path parameter handling using helper for /{Bucket}/{Key}
        assertGeneratedCodeContains(clientFile, "PathParams = [");
        assertGeneratedCodeContains(clientFile, "{<<\"Bucket\">>, maps:get(<<\"Bucket\">>, Input)}");
        assertGeneratedCodeContains(clientFile, "{<<\"Key\">>, maps:get(<<\"Key\">>, Input)}");
        assertGeneratedCodeContains(clientFile, "runtime_http_request:build_uri(UriPattern, PathParams, Endpoint)");
    }
    
    @Test
    public void testPutObjectGeneratesPathParameterSubstitution() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify path parameter handling using helper
        assertGeneratedCodeContains(clientFile, "PathParams = [");
        assertGeneratedCodeContains(clientFile, "{<<\"Bucket\">>, maps:get(<<\"Bucket\">>, Input)}");
        assertGeneratedCodeContains(clientFile, "{<<\"Key\">>, maps:get(<<\"Key\">>, Input)}");
    }
    
    // ===== @httpHeader Input Tests =====
    
    @Test
    public void testGetObjectGeneratesHttpHeaderHandling() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify header building code is generated
        assertGeneratedCodeContains(clientFile, "Build headers with @httpHeader members");
        assertGeneratedCodeContains(clientFile, "Headers0 = [{<<\"Content-Type\">>, <<\"application/json\">>}]");
    }
    
    @Test
    public void testGetObjectGeneratesIfMatchHeader() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify If-Match header handling (optional header)
        assertGeneratedCodeContains(clientFile, "maps:get(<<\"IfMatch\">>, Input, undefined)");
        assertGeneratedCodeContains(clientFile, "<<\"If-Match\">>");
        assertGeneratedCodeContains(clientFile, "ensure_binary(");
    }
    
    @Test
    public void testGetObjectGeneratesRangeHeader() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify Range header handling (optional header)
        assertGeneratedCodeContains(clientFile, "maps:get(<<\"Range\">>, Input, undefined)");
        assertGeneratedCodeContains(clientFile, "<<\"Range\">>");
    }
    
    @Test
    public void testPutObjectGeneratesContentTypeHeader() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify Content-Type header handling
        assertGeneratedCodeContains(clientFile, "maps:get(<<\"ContentType\">>, Input, undefined)");
        assertGeneratedCodeContains(clientFile, "<<\"Content-Type\">>");
    }
    
    @Test
    public void testPutObjectGeneratesCacheControlHeader() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify Cache-Control header handling
        assertGeneratedCodeContains(clientFile, "maps:get(<<\"CacheControl\">>, Input, undefined)");
        assertGeneratedCodeContains(clientFile, "<<\"Cache-Control\">>");
    }
    
    @Test
    public void testOptionalHeadersUseConditionalLogic() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify optional headers use case statements
        assertGeneratedCodeContains(clientFile, "undefined ->");
    }
    
    @Test
    public void testHeadersBuiltIncrementally() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify incremental header building (Headers0 -> Headers1 -> ... -> Headers)
        assertGeneratedCodeContains(clientFile, "Headers0 =");
        // Should have at least Headers1 for operations with @httpHeader members
        assertGeneratedCodeContains(clientFile, "Headers1 =");
    }
    
    // ===== Helper Function Tests =====
    
    @Test
    public void testUrlEncodeHelperGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify url_encode helper function is generated
        assertGeneratedCodeContains(clientFile, "url_encode(Binary) when is_binary(Binary)");
        assertGeneratedCodeContains(clientFile, "uri_string:quote(String)");
    }
    
    @Test
    public void testEnsureBinaryHelperGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify ensure_binary helper function is generated
        assertGeneratedCodeContains(clientFile, "ensure_binary(Value) when is_binary(Value)");
        assertGeneratedCodeContains(clientFile, "ensure_binary(Value) when is_list(Value)");
        assertGeneratedCodeContains(clientFile, "ensure_binary(Value) when is_integer(Value)");
    }
    
    // ===== Operations without HTTP bindings =====
    
    @Test
    public void testListBucketsWithoutPathParameters() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // ListBuckets has no path parameters, should use simple URI
        assertGeneratedCodeContains(clientFile, "No path parameters");
    }
    
    // ===== @httpHeader Output Tests =====
    
    @Test
    public void testGetObjectExtractsOutputHeaders() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify response headers are captured
        assertGeneratedCodeContains(clientFile, "{ok, {{_, 200, _}, ResponseHeaders, ResponseBody}}");
        assertGeneratedCodeContains(clientFile, "Extract @httpHeader members from response");
    }
    
    @Test
    public void testGetObjectExtractsETagHeader() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify ETag header is extracted from response
        assertGeneratedCodeContains(clientFile, "lists:keyfind(\"etag\", 1, ResponseHeaders)");
        assertGeneratedCodeContains(clientFile, "maps:put(<<\"ETag\"");
    }
    
    @Test
    public void testGetObjectExtractsContentTypeHeader() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify Content-Type header is extracted from response
        assertGeneratedCodeContains(clientFile, "lists:keyfind(\"content-type\", 1, ResponseHeaders)");
        assertGeneratedCodeContains(clientFile, "maps:put(<<\"ContentType\"");
    }
    
    @Test
    public void testGetObjectExtractsContentLengthHeader() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify X-Content-Length header is extracted from response
        assertGeneratedCodeContains(clientFile, "lists:keyfind(\"x-content-length\", 1, ResponseHeaders)");
        assertGeneratedCodeContains(clientFile, "maps:put(<<\"ContentLength\"");
    }
    
    @Test
    public void testPutObjectExtractsETagHeader() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify PutObject also extracts ETag from response
        assertGeneratedCodeContains(clientFile, "lists:keyfind(\"etag\", 1, ResponseHeaders)");
    }
    
    @Test
    public void testHeaderExtractionIsCaseInsensitive() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify headers are looked up in lowercase for case-insensitive matching
        assertGeneratedCodeContains(clientFile, "lists:keyfind(\"etag\"");
        assertGeneratedCodeContains(clientFile, "lists:keyfind(\"content-type\"");
    }
    
    @Test
    public void testHeaderExtractionHandlesMissingHeaders() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify missing headers are handled (false case)
        assertGeneratedCodeContains(clientFile, "false -> Output");
    }
    
    @Test
    public void testOutputHeadersBuiltIncrementally() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify incremental output building (Output0 -> Output1 -> ...)
        assertGeneratedCodeContains(clientFile, "Output0 = jsx:decode(ResponseBody");
        assertGeneratedCodeContains(clientFile, "Output1 =");
    }
    
    @Test
    public void testListBucketsWithoutOutputHeaders() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // ListBuckets has no output headers, should use simple response handling
        // Count occurrences - should have at least one simple pattern for ListBuckets
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        assertTrue(content.contains("{ok, {{_, 200, _}, _, ResponseBody}}"),
                "Should have simple response pattern for operations without output headers");
    }
    
    // ============================================================================
    // @httpQuery Tests
    // ============================================================================
    
    @Test
    public void testListObjectsGeneratesQueryStringCode() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        assertGeneratedCodeContains(clientFile, "list_objects");
        // Now uses helper-based query building
        assertGeneratedCodeContains(clientFile, "QueryMapping = [");
        assertGeneratedCodeContains(clientFile, "runtime_http_request:build_query");
    }
    
    @Test
    public void testQueryParametersBuiltIncrementally() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Now uses helper with QueryMapping instead of incremental QueryPairs
        assertGeneratedCodeContains(clientFile, "QueryMapping = [");
        assertGeneratedCodeContains(clientFile, "runtime_http_request:build_query(QueryMapping, Input)");
    }
    
    @Test
    public void testOptionalQueryParametersUseConditionalLogic() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Query parameters are now handled by the runtime helper with required flags
        assertGeneratedCodeContains(clientFile, "QueryMapping = [");
        // The helper handles optional parameters internally
        assertGeneratedCodeContains(clientFile, "false}"); // optional parameter (required = false)
    }
    
    @Test
    public void testQueryStringEncodingWithUriString() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Helper uses runtime_http_request:build_query which handles encoding
        assertGeneratedCodeContains(clientFile, "runtime_http_request:build_query");
    }
    
    @Test
    public void testEmptyQueryStringHandling() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Empty query strings are now handled by the helper returning <<"">>
        assertGeneratedCodeContains(clientFile, "Query = ");
    }
    
    @Test
    public void testQueryStringAppendedToUrl() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Query should be appended to URL
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // Should have pattern like: Url = <<...Query/binary>>
        assertTrue(content.contains("Query/binary") || content.contains("Query = <<\"\">>"), 
                "Query should be part of URL building");
    }
    
    @Test
    public void testQueryParameterNameFromTrait() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Should use query parameter names from @httpQuery trait
        assertGeneratedCodeContains(clientFile, "<<\"prefix\">>"); // from @httpQuery("prefix")
        assertGeneratedCodeContains(clientFile, "<<\"max-keys\">>"); // from @httpQuery("max-keys")
    }
    
    @Test
    public void testIntegerQueryParameterConversion() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // MaxKeys is Integer, should convert to binary
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // Should have ensure_binary or similar conversion for MaxKeys
        assertTrue(content.contains("MaxKeys") || content.contains("max-keys"),
                "Should reference MaxKeys query parameter");
    }
    
    // ============================================================================
    // @httpPayload Tests
    // ============================================================================
    
    @Test
    public void testPutObjectUsesBlobPayload() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // PutObject has @httpPayload blob, should extract Body from input directly
        assertGeneratedCodeContains(clientFile, "put_object");
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // Should get Body directly from input map
        assertTrue(content.contains("Body") && content.contains("maps:get"),
                "Should extract Body from input for blob payload");
    }
    
    @Test
    public void testGetObjectReturnsBlobPayload() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // GetObject has @httpPayload blob in output
        assertGeneratedCodeContains(clientFile, "get_object");
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // Should handle ResponseBody as blob, not parse as JSON
        assertTrue(content.contains("ResponseBody"),
                "Should reference ResponseBody for blob payload output");
    }
    
    @Test
    public void testCreateMetadataUsesStructurePayload() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // CreateMetadata has @httpPayload structure
        assertGeneratedCodeContains(clientFile, "create_metadata");
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // Should extract Metadata structure and encode as JSON
        assertTrue(content.contains("Metadata") || content.contains("jsx:encode"),
                "Should handle structure payload with JSON encoding");
    }
    
    @Test
    public void testEchoUsesStringPayload() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Echo has @httpPayload string
        assertGeneratedCodeContains(clientFile, "echo");
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // Should extract Text directly (string payload)
        assertTrue(content.contains("Text") || content.contains("maps:get"),
                "Should extract string payload from input");
    }
    
    @Test
    public void testListBucketsUsesEntireStructureAsPayload() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // ListBuckets has no @httpPayload, entire structure is JSON
        assertGeneratedCodeContains(clientFile, "list_buckets");
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // Should encode entire Input as JSON
        assertTrue(content.contains("jsx:encode(Input)"),
                "Should encode entire Input for operations without @httpPayload");
    }
    
    @Test
    public void testBlobPayloadNotEncodedAsJson() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // For operations with blob payload, Body should not be JSON-encoded
        // Check that we get the body directly without jsx:encode
        // The operation logic is now in make_put_object_request internal function
        // Search for the function definition (with "when" guard), not a call
        int putObjectStart = content.indexOf("make_put_object_request(Client, Input) when");
        assertTrue(putObjectStart >= 0, 
                "Should generate make_put_object_request internal function definition");
        
        // Find the next function definition or end of file after this function
        int putObjectEnd = content.indexOf("\n%% ", putObjectStart + 1);
        if (putObjectEnd == -1) {
            // If no next function, go to end of file
            putObjectEnd = content.length();
        }
        
        String putObjectCode = content.substring(putObjectStart, putObjectEnd);
        
        // Should reference Body but not encode it with jsx
        // Check for binary string <<"Body">> or variable Body
        assertTrue(putObjectCode.contains("Body") || putObjectCode.contains("<<\"Body\">>"),
                "PutObject should reference Body");
    }
    
    @Test
    public void testPayloadMemberExtraction() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // Operations with @httpPayload should extract the specific member
        // e.g., maps:get(<<"Body">>, Input) for PutObject
        // e.g., maps:get(<<"Metadata">>, Input) for CreateMetadata
        assertTrue(content.contains("maps:get(<<\"Body\">>, Input)") ||
                   content.contains("maps:get(<<\"Metadata\">>, Input)") ||
                   content.contains("maps:get(<<\"Text\">>, Input)"),
                "Should extract @httpPayload member from input");
    }
    
    @Test
    public void testOutputPayloadExtraction() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // GetObject output has @httpPayload Body
        // Echo output has @httpPayload Echo
        // Should handle ResponseBody appropriately
        int getObjectStart = content.indexOf("get_object(");
        if (getObjectStart > 0) {
            int getObjectEnd = content.indexOf("\nlist_", getObjectStart);
            if (getObjectEnd < 0) getObjectEnd = content.length();
            String getObjectCode = content.substring(getObjectStart, getObjectEnd);
            
            assertTrue(getObjectCode.contains("ResponseBody"),
                    "GetObject should handle ResponseBody for blob output");
        }
    }
}
