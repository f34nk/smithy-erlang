package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.AwsProtocolTestBase;
import io.smithy.erlang.codegen.aws.AwsProtocol;
import io.smithy.erlang.codegen.aws.AwsProtocolDetector;
import org.junit.jupiter.api.Test;
import software.amazon.smithy.model.shapes.ServiceShape;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests REST-XML protocol code generation for S3-like services.
 * 
 * Verifies that the restXml protocol generates correct:
 * - RESTful HTTP methods (GET, PUT, POST, DELETE) from @http trait
 * - URI patterns with path parameter substitution
 * - Query parameters from @httpQuery
 * - Headers from @httpHeader
 * - XML request/response encoding/decoding with aws_xml
 * - AWS SigV4 signing
 */
public class RestXmlProtocolTest extends AwsProtocolTestBase {
    
    @Override
    protected String getModelPath() {
        return "/models/s3-minimal.smithy";
    }
    
    @Override
    protected String getServiceShapeId() {
        return "com.amazonaws.s3#S3";
    }
    
    @Override
    protected String getModuleName() {
        return "s3_client";
    }
    
    // ===== Protocol Detection Tests =====
    
    @Test
    public void testProtocolDetection() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        AwsProtocol detectedProtocol = AwsProtocolDetector.detectProtocol(service);
        assertEquals(AwsProtocol.REST_XML, detectedProtocol,
            "Should detect REST-XML protocol");
    }
    
    @Test
    public void testProtocolIsRestProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertTrue(AwsProtocolDetector.isRestProtocol(service),
            "S3 should be recognized as REST protocol");
    }
    
    @Test
    public void testProtocolIsXmlProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertTrue(AwsProtocolDetector.isXmlProtocol(service),
            "S3 should be recognized as XML protocol (responses are XML)");
    }
    
    @Test
    public void testProtocolIsNotQueryProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertFalse(AwsProtocolDetector.isQueryProtocol(service),
            "S3 should not be recognized as Query protocol");
    }
    
    @Test
    public void testProtocolIsNotJsonProtocol() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertFalse(AwsProtocolDetector.isJsonProtocol(service),
            "S3 should not be recognized as JSON protocol");
    }
    
    // ===== Code Generation Tests =====
    
    @Test
    public void testGeneratorRuns() {
        try {
            runGenerator();
        } catch (RuntimeException e) {
            System.err.println("Generator failed with exception: " + e.getMessage());
            if (e.getCause() != null) {
                System.err.println("Cause: " + e.getCause().getMessage());
                e.getCause().printStackTrace();
            }
            throw e;
        }
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
        assertGeneratedCodeContains(clientFile, "get_object(Client, Input)");
        assertGeneratedCodeContains(clientFile, "put_object(Client, Input)");
        assertGeneratedCodeContains(clientFile, "delete_object(Client, Input)");
        assertGeneratedCodeContains(clientFile, "list_buckets(Client, Input)");
        assertGeneratedCodeContains(clientFile, "create_bucket(Client, Input)");
    }
    
    // ===== REST-XML Protocol Specific Tests =====
    
    @Test
    public void testHttpMethodFromTrait() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify HTTP methods from @http trait are used
        assertGeneratedCodeContains(clientFile, "Method = <<\"GET\">>");
        assertGeneratedCodeContains(clientFile, "Method = <<\"PUT\">>");
        assertGeneratedCodeContains(clientFile, "Method = <<\"DELETE\">>");
    }
    
    @Test
    public void testPathParametersGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // For S3 services, path parameters (Bucket, Key) are handled by aws_s3:build_url
        // which provides virtual-hosted-style vs path-style URL routing
        assertGeneratedCodeContains(clientFile, "Bucket = maps:get(<<\"Bucket\">>, Input");
        assertGeneratedCodeContains(clientFile, "Key = maps:get(<<\"Key\">>, Input");
        assertGeneratedCodeContains(clientFile, "aws_s3:build_url");
    }
    
    @Test
    public void testQueryParametersGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify query parameter handling
        assertGeneratedCodeContains(clientFile, "versionId");
        assertGeneratedCodeContains(clientFile, "uri_string:compose_query");
    }
    
    @Test
    public void testHeadersGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify headers from @httpHeader are generated
        assertGeneratedCodeContains(clientFile, "<<\"Content-Type\">>");
        assertGeneratedCodeContains(clientFile, "<<\"Range\">>");
        assertGeneratedCodeContains(clientFile, "<<\"ETag\">>");
    }
    
    @Test
    public void testXmlEncodingPresent() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify aws_xml:encode is used for request serialization
        assertGeneratedCodeContains(clientFile, "aws_xml:encode");
    }
    
    @Test
    public void testXmlDecodingPresent() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify aws_xml:decode is used for response deserialization
        assertGeneratedCodeContains(clientFile, "aws_xml:decode(ResponseBody)");
    }
    
    @Test
    public void testHttpPayloadHandling() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify @httpPayload handling for blob payloads
        assertGeneratedCodeContains(clientFile, "@httpPayload");
        assertGeneratedCodeContains(clientFile, "Body = maps:get(<<\"Body\">>, Input)");
    }
    
    @Test
    public void testAwsSigV4SigningIntegration() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify AWS SigV4 signing is integrated
        // Argument order: (Method, Url, Headers, Body, Client/Credentials)
        assertGeneratedCodeContains(clientFile, "aws_sigv4:sign_request(Method, Url, Headers, Body, Client)");
    }
    
    @Test
    public void testRetryWrapperGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify retry wrapper is generated
        assertGeneratedCodeContains(clientFile, "get_object(Client, Input, #{})");
        assertGeneratedCodeContains(clientFile, "get_object(Client, Input, Options)");
        assertGeneratedCodeContains(clientFile, "aws_retry:with_retry(RequestFun, Options)");
    }
    
    @Test
    public void testErrorHandlingGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify error handling
        assertGeneratedCodeContains(clientFile, "{error, {http_error,");
        assertGeneratedCodeContains(clientFile, "{error, {signing_error,");
        assertGeneratedCodeContains(clientFile, "{error, {aws_error,");
    }
    
    @Test
    public void testRestXmlErrorParsing() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify REST-XML error response parsing
        assertGeneratedCodeContains(clientFile, "<<\"Error\">>");
        assertGeneratedCodeContains(clientFile, "<<\"Code\">>");
        assertGeneratedCodeContains(clientFile, "<<\"Message\">>");
        assertGeneratedCodeContains(clientFile, "{error, {aws_error, StatusCode, Code, Message}}");
    }
    
    @Test
    public void testResponseHeaderExtraction() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify response header extraction from @httpHeader members
        assertGeneratedCodeContains(clientFile, "ResponseHeaders");
        assertGeneratedCodeContains(clientFile, "lists:keyfind");
    }
    
    @Test
    public void testTypeDefinitionsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify type definitions are generated
        assertGeneratedCodeContains(clientFile, "-type get_object_request()");
        assertGeneratedCodeContains(clientFile, "-type get_object_response()");
        assertGeneratedCodeContains(clientFile, "-type put_object_request()");
    }
    
    @Test
    public void testFunctionSpecsGenerated() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        // Verify function specs use type aliases
        assertGeneratedCodeContains(clientFile, "-spec get_object(Client :: map(), Input :: get_object_request())");
        assertGeneratedCodeContains(clientFile, "-> {ok, get_object_response()} | {error, term()}");
    }
    
    // ===== Model Structure Tests =====
    
    @Test
    public void testS3ModelHasOperations() {
        assertEquals(5, model.getOperationShapes().size(), 
                "S3 test model should contain exactly 5 operations");
    }
    
    @Test
    public void testS3ModelHasStructures() {
        assertTrue(model.getStructureShapes().size() > 0,
                "S3 test model should have structure definitions");
    }
    
    @Test
    public void testServiceHasRestXmlTrait() {
        ServiceShape service = model.expectShape(
            software.amazon.smithy.model.shapes.ShapeId.from(getServiceShapeId()),
            ServiceShape.class
        );
        
        assertTrue(service.getAllTraits().values().stream()
            .anyMatch(trait -> trait.toShapeId().toString().equals("aws.protocols#restXml")),
            "Service should have restXml trait");
    }
    
    // ===== RestXmlProtocol Class Tests =====
    
    @Test
    public void testProtocolName() {
        RestXmlProtocol protocol = new RestXmlProtocol();
        assertEquals("restXml", protocol.getName());
    }
    
    @Test
    public void testProtocolDefaultMethod() {
        RestXmlProtocol protocol = new RestXmlProtocol();
        assertNull(protocol.getDefaultMethod(),
            "REST protocols should return null for default method (uses @http trait)");
    }
    
    @Test
    public void testProtocolDefaultUri() {
        RestXmlProtocol protocol = new RestXmlProtocol();
        assertNull(protocol.getDefaultUri(),
            "REST protocols should return null for default URI (uses @http trait)");
    }
    
    @Test
    public void testProtocolContentType() {
        RestXmlProtocol protocol = new RestXmlProtocol();
        assertEquals("application/xml", protocol.getContentType(null));
    }
    
    // ===== ProtocolFactory Tests =====
    
    @Test
    public void testProtocolFactoryReturnsRestXmlProtocol() {
        Protocol protocol = ProtocolFactory.createProtocol(AwsProtocol.REST_XML);
        assertNotNull(protocol);
        assertTrue(protocol instanceof RestXmlProtocol);
        assertEquals("restXml", protocol.getName());
    }
}
