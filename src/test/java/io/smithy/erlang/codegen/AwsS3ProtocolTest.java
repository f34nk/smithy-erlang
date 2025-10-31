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
        return "/models/aws-test-s3.smithy";
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
        String typesFile = "src/" + getModuleName() + "_types.erl";
        assertFileGenerated(typesFile);
    }
    
    // ===== @httpLabel Tests =====
    
    @Test
    public void testGetObjectGeneratesPathParameterSubstitution() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify path parameter handling for /{Bucket}/{Key}
        assertGeneratedCodeContains(clientFile, "BucketValue = maps:get(<<\"Bucket\">>, Input)");
        assertGeneratedCodeContains(clientFile, "KeyValue = maps:get(<<\"Key\">>, Input)");
        assertGeneratedCodeContains(clientFile, "url_encode(ensure_binary(");
    }
    
    @Test
    public void testPutObjectGeneratesPathParameterSubstitution() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify path parameter handling exists
        assertGeneratedCodeContains(clientFile, "BucketValue = maps:get(<<\"Bucket\">>, Input)");
        assertGeneratedCodeContains(clientFile, "KeyValue = maps:get(<<\"Key\">>, Input)");
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
}
