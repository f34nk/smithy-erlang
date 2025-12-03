package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.AwsProtocolTestBase;
import io.smithy.erlang.codegen.aws.AwsProtocol;
import io.smithy.erlang.codegen.aws.AwsProtocolDetector;
import org.junit.jupiter.api.Test;
import software.amazon.smithy.model.shapes.ServiceShape;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests REST-JSON protocol code generation for API Gateway-like services.
 * 
 * Verifies that the restJson1 protocol generates correct:
 * - RESTful HTTP methods (GET, POST, PUT, DELETE)
 * - Path parameter substitution from @httpLabel
 * - Query string parameters from @httpQuery
 * - Header handling from @httpHeader
 * - JSON encoding/decoding with jsx
 * - AWS SigV4 signing
 * - Error response parsing
 */
public class RestJsonProtocolTest extends AwsProtocolTestBase {
    
    @Override
    protected String getModelPath() {
        return "/models/apigateway-minimal.smithy";
    }
    
    @Override
    protected String getServiceShapeId() {
        return "com.amazonaws.apigateway#APIGateway";
    }
    
    @Override
    protected String getModuleName() {
        return "apigateway_client";
    }
    
    // ===== Protocol Detection Tests =====
    
    @Test
    public void testProtocolDetection() {
        // Get the service shape
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        // Verify protocol detection recognizes restJson1
        AwsProtocol detectedProtocol = AwsProtocolDetector.detectProtocol(service);
        assertEquals(AwsProtocol.REST_JSON_1, detectedProtocol,
            "Should detect REST JSON 1 protocol");
    }
    
    @Test
    public void testProtocolIsRestProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertTrue(AwsProtocolDetector.isRestProtocol(service),
            "API Gateway should be recognized as REST protocol");
    }
    
    @Test
    public void testProtocolIsJsonProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertTrue(AwsProtocolDetector.isJsonProtocol(service),
            "API Gateway REST-JSON should be recognized as JSON protocol (uses JSON encoding)");
    }
    
    // ===== Code Generation Tests =====
    
    @Test
    public void testGeneratorRuns() {
        assertDoesNotThrow(() -> runGenerator(), 
                "Generator should run without exceptions on API Gateway model");
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
        assertGeneratedCodeContains(clientFile, "get_rest_api(Client, Input)");
        assertGeneratedCodeContains(clientFile, "create_rest_api(Client, Input)");
        assertGeneratedCodeContains(clientFile, "delete_rest_api(Client, Input)");
        assertGeneratedCodeContains(clientFile, "get_resource(Client, Input)");
        assertGeneratedCodeContains(clientFile, "create_resource(Client, Input)");
    }
    
    // ===== REST-JSON Protocol Specific Tests =====
    
    @Test
    public void testHttpMethodsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify various HTTP methods are used
        assertGeneratedCodeContains(clientFile, "Method = <<\"GET\">>");
        assertGeneratedCodeContains(clientFile, "Method = <<\"POST\">>");
        assertGeneratedCodeContains(clientFile, "Method = <<\"DELETE\">>");
    }
    
    @Test
    public void testPathParameterSubstitution() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify path parameter extraction and substitution
        // GetRestApi has @httpLabel restApiId in URI: /restapis/{restApiId}
        assertGeneratedCodeContains(clientFile, "maps:get(<<\"restApiId\">>, Input)");
        assertGeneratedCodeContains(clientFile, "binary:replace");
        assertGeneratedCodeContains(clientFile, "<<\"{restApiId}\">>");
    }
    
    @Test
    public void testQueryParameterHandling() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify query parameter handling
        // CreateRestApi has @httpQuery("mode") mode: String
        assertGeneratedCodeContains(clientFile, "QueryPairs");
        assertGeneratedCodeContains(clientFile, "uri_string:compose_query");
    }
    
    @Test
    public void testHeaderHandling() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify header handling
        // CreateRestApi has @httpHeader("X-Api-Key") apiKey: String
        assertGeneratedCodeContains(clientFile, "<<\"X-Api-Key\">>");
    }
    
    @Test
    public void testContentTypeHeader() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify Content-Type header for REST-JSON
        assertGeneratedCodeContains(clientFile, "<<\"Content-Type\">>");
        assertGeneratedCodeContains(clientFile, "<<\"application/json\">>");
    }
    
    @Test
    public void testJsonEncodingPresent() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify jsx:encode is used for request serialization
        assertGeneratedCodeContains(clientFile, "jsx:encode");
    }
    
    @Test
    public void testJsonDecodingPresent() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify jsx:decode is used for response deserialization
        assertGeneratedCodeContains(clientFile, "jsx:decode");
        assertGeneratedCodeContains(clientFile, "return_maps");
    }
    
    @Test
    public void testAwsSigV4SigningIntegration() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify AWS SigV4 signing is integrated
        assertGeneratedCodeContains(clientFile, "aws_sigv4:sign_request");
    }
    
    @Test
    public void testRetryWrapperGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify retry wrapper is generated (2-arity and 3-arity functions)
        assertGeneratedCodeContains(clientFile, "get_rest_api(Client, Input, #{})");
        assertGeneratedCodeContains(clientFile, "get_rest_api(Client, Input, Options)");
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
        
        // Verify AWS error parsing (message and code extraction)
        assertGeneratedCodeContains(clientFile, "{error, {aws_error,");
    }
    
    @Test
    public void testRestJsonErrorParsing() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify REST-JSON error parsing extracts message and code
        assertGeneratedCodeContains(clientFile, "<<\"message\">>");
        assertGeneratedCodeContains(clientFile, "<<\"__type\">>");
    }
    
    @Test
    public void testEmptyBodyHandling() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify empty response body handling (some REST operations return empty body)
        assertGeneratedCodeContains(clientFile, "<<>>");
    }
    
    @Test
    public void testUrlEncoding() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify URL encoding is applied to path parameters
        assertGeneratedCodeContains(clientFile, "url_encode");
    }
    
    @Test
    public void testTypeDefinitionsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify type definitions are generated
        assertGeneratedCodeContains(clientFile, "-type get_rest_api_request()");
        assertGeneratedCodeContains(clientFile, "-type rest_api()");
        assertGeneratedCodeContains(clientFile, "-type create_rest_api_request()");
    }
    
    @Test
    public void testFunctionSpecsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify function specs use type aliases
        assertGeneratedCodeContains(clientFile, "-spec get_rest_api(Client :: map(), Input :: get_rest_api_request())");
        assertGeneratedCodeContains(clientFile, "-> {ok, rest_api()} | {error, term()}");
    }
    
    // ===== Model Structure Tests =====
    
    @Test
    public void testAPIGatewayModelHasOperations() {
        // Verify the test model contains expected operations
        assertEquals(6, model.getOperationShapes().size(), 
                "API Gateway test model should contain exactly 6 operations");
    }
    
    @Test
    public void testAPIGatewayModelHasStructures() {
        // Verify the test model has structure definitions
        assertTrue(model.getStructureShapes().size() > 0,
                "API Gateway test model should have structure definitions");
    }
    
    @Test
    public void testServiceHasRestJson1Trait() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        // Verify the service has the restJson1 trait
        assertTrue(service.getAllTraits().values().stream()
            .anyMatch(trait -> trait.toShapeId().toString().equals("aws.protocols#restJson1")),
            "Service should have restJson1 trait");
    }
    
    @Test
    public void testProtocolNameIsCorrect() {
        RestJsonProtocol protocol = new RestJsonProtocol();
        assertEquals("restJson1", protocol.getName(),
            "Protocol name should be 'restJson1'");
    }
    
    @Test
    public void testDefaultMethodIsNull() {
        RestJsonProtocol protocol = new RestJsonProtocol();
        assertNull(protocol.getDefaultMethod(),
            "REST protocols should return null for default method (determined by @http trait)");
    }
    
    @Test
    public void testDefaultUriIsNull() {
        RestJsonProtocol protocol = new RestJsonProtocol();
        assertNull(protocol.getDefaultUri(),
            "REST protocols should return null for default URI (determined by @http trait)");
    }
    
    @Test
    public void testContentTypeIsJson() {
        RestJsonProtocol protocol = new RestJsonProtocol();
        assertEquals("application/json", protocol.getContentType(null),
            "Content-Type should be 'application/json'");
    }
}
