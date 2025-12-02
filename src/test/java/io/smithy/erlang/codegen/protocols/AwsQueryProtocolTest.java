package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.AwsProtocolTestBase;
import io.smithy.erlang.codegen.aws.AwsProtocol;
import io.smithy.erlang.codegen.aws.AwsProtocolDetector;
import org.junit.jupiter.api.Test;
import software.amazon.smithy.model.shapes.ServiceShape;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests AWS Query protocol code generation for SQS-like services.
 * 
 * Verifies that the awsQuery protocol generates correct:
 * - POST requests to "/"
 * - Content-Type: application/x-www-form-urlencoded
 * - Action parameter in request body
 * - aws_query:encode for request serialization
 * - aws_xml:decode for response deserialization
 * - AWS SigV4 signing
 */
public class AwsQueryProtocolTest extends AwsProtocolTestBase {
    
    @Override
    protected String getModelPath() {
        return "/models/sqs-minimal.smithy";
    }
    
    @Override
    protected String getServiceShapeId() {
        return "com.amazonaws.sqs#SQS";
    }
    
    @Override
    protected String getModuleName() {
        return "sqs_client";
    }
    
    // ===== Protocol Detection Tests =====
    
    @Test
    public void testProtocolDetection() {
        // Get the service shape
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        // Verify protocol detection recognizes awsQuery
        AwsProtocol detectedProtocol = AwsProtocolDetector.detectProtocol(service);
        assertEquals(AwsProtocol.AWS_QUERY, detectedProtocol,
            "Should detect AWS Query protocol");
    }
    
    @Test
    public void testProtocolIsQueryProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertTrue(AwsProtocolDetector.isQueryProtocol(service),
            "SQS should be recognized as Query protocol");
    }
    
    @Test
    public void testProtocolIsXmlProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertTrue(AwsProtocolDetector.isXmlProtocol(service),
            "SQS should be recognized as XML protocol (responses are XML)");
    }
    
    @Test
    public void testProtocolIsNotRestProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertFalse(AwsProtocolDetector.isRestProtocol(service),
            "SQS should not be recognized as REST protocol");
    }
    
    @Test
    public void testProtocolIsNotJsonProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertFalse(AwsProtocolDetector.isJsonProtocol(service),
            "SQS should not be recognized as JSON protocol");
    }
    
    // ===== Code Generation Tests =====
    
    @Test
    public void testGeneratorRuns() {
        assertDoesNotThrow(() -> runGenerator(), 
                "Generator should run without exceptions on SQS model");
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
        
        // Verify key operations are generated
        assertGeneratedCodeContains(clientFile, "send_message(Client, Input)");
        assertGeneratedCodeContains(clientFile, "receive_message(Client, Input)");
        assertGeneratedCodeContains(clientFile, "delete_message(Client, Input)");
    }
    
    // ===== AWS Query Protocol Specific Tests =====
    
    @Test
    public void testPostMethodGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify POST method is used (AWS Query always uses POST)
        assertGeneratedCodeContains(clientFile, "Method = <<\"POST\">>");
    }
    
    @Test
    public void testRootPathGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify path is "/" (AWS Query always uses root path)
        assertGeneratedCodeContains(clientFile, "Path = <<\"/\">>");
    }
    
    @Test
    public void testContentTypeHeaderGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify Content-Type header for AWS Query
        assertGeneratedCodeContains(clientFile, "<<\"Content-Type\">>");
        assertGeneratedCodeContains(clientFile, "<<\"application/x-www-form-urlencoded\">>");
    }
    
    @Test
    public void testContentTypeInHttpcRequest() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify content type is passed to httpc:request
        assertGeneratedCodeContains(clientFile, "ContentType = \"application/x-www-form-urlencoded\"");
    }
    
    @Test
    public void testActionParameterGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify Action parameter is generated for each operation
        assertGeneratedCodeContains(clientFile, "Action = <<\"SendMessage\">>");
        assertGeneratedCodeContains(clientFile, "Action = <<\"ReceiveMessage\">>");
        assertGeneratedCodeContains(clientFile, "Action = <<\"DeleteMessage\">>");
    }
    
    @Test
    public void testQueryEncodingPresent() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify aws_query:encode is used for request serialization
        assertGeneratedCodeContains(clientFile, "aws_query:encode(Action, Input)");
    }
    
    @Test
    public void testXmlDecodingPresent() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify aws_xml:decode is used for response deserialization
        assertGeneratedCodeContains(clientFile, "aws_xml:decode(ResponseBody)");
    }
    
    @Test
    public void testResponseParsingFormat() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify response parsing handles AWS Query response format
        // Format: <OperationNameResponse><OperationNameResult>...</OperationNameResult></OperationNameResponse>
        assertGeneratedCodeContains(clientFile, "<<\"SendMessageResponse\">>");
        assertGeneratedCodeContains(clientFile, "<<\"SendMessageResult\">>");
    }
    
    @Test
    public void testAwsSigV4SigningIntegration() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify AWS SigV4 signing is integrated
        assertGeneratedCodeContains(clientFile, "aws_sigv4:sign_request(Client, Method, Url, Headers, Payload)");
    }
    
    @Test
    public void testRetryWrapperGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify retry wrapper is generated (2-arity and 3-arity functions)
        assertGeneratedCodeContains(clientFile, "send_message(Client, Input, #{})");
        assertGeneratedCodeContains(clientFile, "send_message(Client, Input, Options)");
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
    public void testAwsQueryErrorParsing() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify AWS Query error response parsing (XML format)
        assertGeneratedCodeContains(clientFile, "<<\"ErrorResponse\">>");
        assertGeneratedCodeContains(clientFile, "<<\"Error\">>");
        assertGeneratedCodeContains(clientFile, "<<\"Code\">>");
        assertGeneratedCodeContains(clientFile, "<<\"Message\">>");
        assertGeneratedCodeContains(clientFile, "{error, {aws_error, StatusCode, Code, Message}}");
    }
    
    @Test
    public void testTypeDefinitionsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify type definitions are generated
        assertGeneratedCodeContains(clientFile, "-type send_message_request()");
        assertGeneratedCodeContains(clientFile, "-type send_message_result()");
        assertGeneratedCodeContains(clientFile, "-type receive_message_request()");
        assertGeneratedCodeContains(clientFile, "-type delete_message_request()");
    }
    
    @Test
    public void testFunctionSpecsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify function specs use type aliases
        assertGeneratedCodeContains(clientFile, "-spec send_message(Client :: map(), Input :: send_message_request())");
        assertGeneratedCodeContains(clientFile, "-> {ok, send_message_result()} | {error, term()}");
    }
    
    // ===== Model Structure Tests =====
    
    @Test
    public void testSQSModelHasOperations() {
        // Verify the test model contains expected operations
        assertEquals(3, model.getOperationShapes().size(), 
                "SQS test model should contain exactly 3 operations");
    }
    
    @Test
    public void testSQSModelHasStructures() {
        // Verify the test model has structure definitions
        assertTrue(model.getStructureShapes().size() > 0,
                "SQS test model should have structure definitions");
    }
    
    @Test
    public void testServiceHasAwsQueryTrait() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        // Verify the service has the awsQuery trait
        assertTrue(service.getAllTraits().values().stream()
            .anyMatch(trait -> trait.toShapeId().toString().equals("aws.protocols#awsQuery")),
            "Service should have awsQuery trait");
    }
    
    // ===== AwsQueryProtocol Class Tests =====
    
    @Test
    public void testProtocolName() {
        AwsQueryProtocol protocol = new AwsQueryProtocol();
        assertEquals("awsQuery", protocol.getName());
    }
    
    @Test
    public void testProtocolDefaultMethod() {
        AwsQueryProtocol protocol = new AwsQueryProtocol();
        assertEquals("POST", protocol.getDefaultMethod());
    }
    
    @Test
    public void testProtocolDefaultUri() {
        AwsQueryProtocol protocol = new AwsQueryProtocol();
        assertEquals("/", protocol.getDefaultUri());
    }
    
    @Test
    public void testProtocolContentType() {
        AwsQueryProtocol protocol = new AwsQueryProtocol();
        assertEquals("application/x-www-form-urlencoded", protocol.getContentType(null));
    }
    
    // ===== ProtocolFactory Tests =====
    
    @Test
    public void testProtocolFactoryReturnsAwsQueryProtocol() {
        Protocol protocol = ProtocolFactory.createProtocol(AwsProtocol.AWS_QUERY);
        assertNotNull(protocol);
        assertTrue(protocol instanceof AwsQueryProtocol);
        assertEquals("awsQuery", protocol.getName());
    }
}
