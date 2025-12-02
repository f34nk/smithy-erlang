package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.AwsProtocolTestBase;
import io.smithy.erlang.codegen.aws.AwsProtocol;
import io.smithy.erlang.codegen.aws.AwsProtocolDetector;
import org.junit.jupiter.api.Test;
import software.amazon.smithy.model.shapes.ServiceShape;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests AWS JSON protocol code generation for DynamoDB-like services.
 * 
 * Verifies that the awsJson1_0 protocol generates correct:
 * - X-Amz-Target headers
 * - Content-Type headers
 * - POST requests to "/"
 * - JSON encoding/decoding with jsx
 * - AWS SigV4 signing
 */
public class AwsJsonProtocolTest extends AwsProtocolTestBase {
    
    @Override
    protected String getModelPath() {
        return "/models/dynamodb-minimal.smithy";
    }
    
    @Override
    protected String getServiceShapeId() {
        return "com.amazonaws.dynamodb#DynamoDB";
    }
    
    @Override
    protected String getModuleName() {
        return "dynamodb_client";
    }
    
    // ===== Protocol Detection Tests =====
    
    @Test
    public void testProtocolDetection() {
        // Get the service shape
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        // Verify protocol detection recognizes awsJson1_0
        AwsProtocol detectedProtocol = AwsProtocolDetector.detectProtocol(service);
        assertEquals(AwsProtocol.AWS_JSON_1_0, detectedProtocol,
            "Should detect AWS JSON 1.0 protocol");
    }
    
    @Test
    public void testProtocolIsJsonProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertTrue(AwsProtocolDetector.isJsonProtocol(service),
            "DynamoDB should be recognized as JSON protocol");
    }
    
    @Test
    public void testProtocolIsNotRestProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertFalse(AwsProtocolDetector.isRestProtocol(service),
            "DynamoDB should not be recognized as REST protocol");
    }
    
    // ===== Code Generation Tests =====
    
    @Test
    public void testGeneratorRuns() {
        assertDoesNotThrow(() -> runGenerator(), 
                "Generator should run without exceptions on DynamoDB model");
    }
    
    @Test
    public void testClientFileGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        assertFileGenerated(clientFile);
    }
    
    @Test
    public void testOperationFunctionsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify all operations are generated
        assertGeneratedCodeContains(clientFile, "get_item(Client, Input)");
        assertGeneratedCodeContains(clientFile, "put_item(Client, Input)");
        assertGeneratedCodeContains(clientFile, "query(Client, Input)");
    }
    
    // ===== AWS JSON Protocol Specific Tests =====
    
    @Test
    public void testPostMethodGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify POST method is used (AWS JSON always uses POST)
        assertGeneratedCodeContains(clientFile, "Method = <<\"POST\">>");
    }
    
    @Test
    public void testRootPathGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify path is "/" (AWS JSON always uses root path)
        assertGeneratedCodeContains(clientFile, "Path = <<\"/\">>");
    }
    
    @Test
    public void testXAmzTargetHeaderGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify X-Amz-Target header is generated
        assertGeneratedCodeContains(clientFile, "<<\"X-Amz-Target\">>");
        
        // Verify header format: ServiceName_Version.OperationName
        // For DynamoDB 2012-08-10, it should be DynamoDB_20120810.GetItem, etc.
        assertGeneratedCodeContains(clientFile, "<<\"DynamoDB_2012-08-10.GetItem\">>");
        assertGeneratedCodeContains(clientFile, "<<\"DynamoDB_2012-08-10.PutItem\">>");
        assertGeneratedCodeContains(clientFile, "<<\"DynamoDB_2012-08-10.Query\">>");
    }
    
    @Test
    public void testContentTypeHeaderGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify Content-Type header for AWS JSON 1.0
        assertGeneratedCodeContains(clientFile, "<<\"Content-Type\">>");
        assertGeneratedCodeContains(clientFile, "<<\"application/x-amz-json-1.0\">>");
    }
    
    @Test
    public void testJsonEncodingPresent() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify jsx:encode is used for request serialization
        assertGeneratedCodeContains(clientFile, "jsx:encode(Input)");
    }
    
    @Test
    public void testJsonDecodingPresent() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify jsx:decode is used for response deserialization
        assertGeneratedCodeContains(clientFile, "jsx:decode(ResponseBody, [return_maps])");
    }
    
    @Test
    public void testAwsSigV4SigningIntegration() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify AWS SigV4 signing is integrated
        // Argument order: (Method, Url, Headers, Payload, Client/Credentials)
        assertGeneratedCodeContains(clientFile, "aws_sigv4:sign_request(Method, Url, Headers, Payload, Client)");
    }
    
    @Test
    public void testRetryWrapperGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify retry wrapper is generated (2-arity and 3-arity functions)
        assertGeneratedCodeContains(clientFile, "get_item(Client, Input, #{})");
        assertGeneratedCodeContains(clientFile, "get_item(Client, Input, Options)");
        assertGeneratedCodeContains(clientFile, "aws_retry:with_retry(RequestFun, Options)");
    }
    
    @Test
    public void testErrorHandlingGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify error handling for HTTP errors
        assertGeneratedCodeContains(clientFile, "{error, {http_error,");
        
        // Verify error handling for signing errors
        assertGeneratedCodeContains(clientFile, "{error, {signing_error,");
    }
    
    @Test
    public void testTypeDefinitionsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify type definitions are generated
        assertGeneratedCodeContains(clientFile, "-type get_item_input()");
        assertGeneratedCodeContains(clientFile, "-type get_item_output()");
        assertGeneratedCodeContains(clientFile, "-type put_item_input()");
        assertGeneratedCodeContains(clientFile, "-type query_input()");
    }
    
    @Test
    public void testFunctionSpecsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify function specs use type aliases
        assertGeneratedCodeContains(clientFile, "-spec get_item(Client :: map(), Input :: get_item_input())");
        assertGeneratedCodeContains(clientFile, "-> {ok, get_item_output()} | {error, term()}");
    }
    
    // ===== Model Structure Tests =====
    
    @Test
    public void testDynamoDBModelHasOperations() {
        // Verify the test model contains expected operations
        assertEquals(3, model.getOperationShapes().size(), 
                "DynamoDB test model should contain exactly 3 operations");
    }
    
    @Test
    public void testDynamoDBModelHasStructures() {
        // Verify the test model has structure definitions
        assertTrue(model.getStructureShapes().size() > 0,
                "DynamoDB test model should have structure definitions");
    }
    
    @Test
    public void testServiceHasAwsJson1_0Trait() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        // Verify the service has the awsJson1_0 trait
        assertTrue(service.getAllTraits().values().stream()
            .anyMatch(trait -> trait.toShapeId().toString().equals("aws.protocols#awsJson1_0")),
            "Service should have awsJson1_0 trait");
    }
}
