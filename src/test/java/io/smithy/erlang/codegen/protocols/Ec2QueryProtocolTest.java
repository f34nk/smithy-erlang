package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.AwsProtocolTestBase;
import io.smithy.erlang.codegen.aws.AwsProtocol;
import io.smithy.erlang.codegen.aws.AwsProtocolDetector;
import org.junit.jupiter.api.Test;
import software.amazon.smithy.model.shapes.ServiceShape;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests EC2 Query protocol code generation for EC2-like services.
 * 
 * Verifies that the ec2Query protocol generates correct:
 * - POST requests to "/"
 * - Content-Type: application/x-www-form-urlencoded
 * - Action parameter in request body
 * - aws_query:encode for request serialization
 * - aws_xml:decode for response deserialization
 * - EC2-specific error response parsing
 * - AWS SigV4 signing
 */
public class Ec2QueryProtocolTest extends AwsProtocolTestBase {
    
    @Override
    protected String getModelPath() {
        return "/models/ec2-minimal.smithy";
    }
    
    @Override
    protected String getServiceShapeId() {
        return "com.amazonaws.ec2#EC2";
    }
    
    @Override
    protected String getModuleName() {
        return "ec2_client";
    }
    
    // ===== Protocol Detection Tests =====
    
    @Test
    public void testProtocolDetection() {
        // Get the service shape
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        // Verify protocol detection recognizes ec2Query
        AwsProtocol detectedProtocol = AwsProtocolDetector.detectProtocol(service);
        assertEquals(AwsProtocol.EC2_QUERY, detectedProtocol,
            "Should detect EC2 Query protocol");
    }
    
    @Test
    public void testProtocolIsQueryProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertTrue(AwsProtocolDetector.isQueryProtocol(service),
            "EC2 should be recognized as Query protocol");
    }
    
    @Test
    public void testProtocolIsXmlProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertTrue(AwsProtocolDetector.isXmlProtocol(service),
            "EC2 should be recognized as XML protocol (responses are XML)");
    }
    
    @Test
    public void testProtocolIsNotRestProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertFalse(AwsProtocolDetector.isRestProtocol(service),
            "EC2 should not be recognized as REST protocol");
    }
    
    @Test
    public void testProtocolIsNotJsonProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertFalse(AwsProtocolDetector.isJsonProtocol(service),
            "EC2 should not be recognized as JSON protocol");
    }
    
    // ===== Code Generation Tests =====
    
    @Test
    public void testGeneratorRuns() {
        assertDoesNotThrow(() -> runGenerator(), 
                "Generator should run without exceptions on EC2 model");
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
        assertGeneratedCodeContains(clientFile, "describe_instances(Client, Input)");
        assertGeneratedCodeContains(clientFile, "run_instances(Client, Input)");
        assertGeneratedCodeContains(clientFile, "terminate_instances(Client, Input)");
    }
    
    // ===== EC2 Query Protocol Specific Tests =====
    
    @Test
    public void testPostMethodGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify POST method is used (EC2 Query always uses POST)
        assertGeneratedCodeContains(clientFile, "Method = <<\"POST\">>");
    }
    
    @Test
    public void testRootPathGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify path is "/" (EC2 Query always uses root path)
        assertGeneratedCodeContains(clientFile, "Path = <<\"/\">>");
    }
    
    @Test
    public void testContentTypeHeaderGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify Content-Type header for EC2 Query
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
        assertGeneratedCodeContains(clientFile, "Action = <<\"DescribeInstances\">>");
        assertGeneratedCodeContains(clientFile, "Action = <<\"RunInstances\">>");
        assertGeneratedCodeContains(clientFile, "Action = <<\"TerminateInstances\">>");
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
        
        // Verify response parsing handles EC2 Query response format
        // Format: <OperationNameResponse><OperationNameResult>...</OperationNameResult></OperationNameResponse>
        assertGeneratedCodeContains(clientFile, "<<\"DescribeInstancesResponse\">>");
        assertGeneratedCodeContains(clientFile, "<<\"DescribeInstancesResult\">>");
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
        assertGeneratedCodeContains(clientFile, "describe_instances(Client, Input, #{})");
        assertGeneratedCodeContains(clientFile, "describe_instances(Client, Input, Options)");
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
    public void testEc2QueryErrorParsing() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify EC2 Query error response parsing (XML format)
        // EC2 error format: <Response><Errors><Error><Code>...</Code><Message>...</Message></Error></Errors></Response>
        assertGeneratedCodeContains(clientFile, "<<\"Response\">>");
        assertGeneratedCodeContains(clientFile, "<<\"Errors\">>");
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
        assertGeneratedCodeContains(clientFile, "-type describe_instances_request()");
        assertGeneratedCodeContains(clientFile, "-type describe_instances_result()");
        assertGeneratedCodeContains(clientFile, "-type run_instances_request()");
        assertGeneratedCodeContains(clientFile, "-type terminate_instances_request()");
    }
    
    @Test
    public void testFunctionSpecsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify function specs use type aliases
        assertGeneratedCodeContains(clientFile, "-spec describe_instances(Client :: map(), Input :: describe_instances_request())");
        assertGeneratedCodeContains(clientFile, "-> {ok, describe_instances_result()} | {error, term()}");
    }
    
    // ===== Model Structure Tests =====
    
    @Test
    public void testEC2ModelHasOperations() {
        // Verify the test model contains expected operations
        assertEquals(3, model.getOperationShapes().size(), 
                "EC2 test model should contain exactly 3 operations");
    }
    
    @Test
    public void testEC2ModelHasStructures() {
        // Verify the test model has structure definitions
        assertTrue(model.getStructureShapes().size() > 0,
                "EC2 test model should have structure definitions");
    }
    
    @Test
    public void testServiceHasEc2QueryTrait() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        // Verify the service has the ec2Query trait
        assertTrue(service.getAllTraits().values().stream()
            .anyMatch(trait -> trait.toShapeId().toString().equals("aws.protocols#ec2Query")),
            "Service should have ec2Query trait");
    }
    
    // ===== Ec2QueryProtocol Class Tests =====
    
    @Test
    public void testProtocolName() {
        Ec2QueryProtocol protocol = new Ec2QueryProtocol();
        assertEquals("ec2Query", protocol.getName());
    }
    
    @Test
    public void testProtocolDefaultMethod() {
        Ec2QueryProtocol protocol = new Ec2QueryProtocol();
        assertEquals("POST", protocol.getDefaultMethod());
    }
    
    @Test
    public void testProtocolDefaultUri() {
        Ec2QueryProtocol protocol = new Ec2QueryProtocol();
        assertEquals("/", protocol.getDefaultUri());
    }
    
    @Test
    public void testProtocolContentType() {
        Ec2QueryProtocol protocol = new Ec2QueryProtocol();
        assertEquals("application/x-www-form-urlencoded", protocol.getContentType(null));
    }
    
    // ===== ProtocolFactory Tests =====
    
    @Test
    public void testProtocolFactoryReturnsEc2QueryProtocol() {
        Protocol protocol = ProtocolFactory.createProtocol(AwsProtocol.EC2_QUERY);
        assertNotNull(protocol);
        assertTrue(protocol instanceof Ec2QueryProtocol);
        assertEquals("ec2Query", protocol.getName());
    }
}
