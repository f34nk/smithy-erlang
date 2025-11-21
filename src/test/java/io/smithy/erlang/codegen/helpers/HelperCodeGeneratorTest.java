package io.smithy.erlang.codegen.helpers;

import org.junit.jupiter.api.Test;
import software.amazon.smithy.model.shapes.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for HelperCodeGenerator
 * 
 * Note: These tests focus on testing the code generation methods that don't require
 * complex trait setups. Full integration testing will be done with real Smithy models.
 */
public class HelperCodeGeneratorTest {
    
    @Test
    public void testGenerateRequestExecutionCode() {
        String code = HelperCodeGenerator.generateRequestExecutionCode();
        
        assertNotNull(code);
        assertTrue(code.contains("runtime_http_request:execute_request"));
        assertTrue(code.contains("Client, Method, Url, Headers, Body"));
        assertTrue(code.contains("{ok, {StatusCode, ResponseHeaders, ResponseBody}}"));
        assertTrue(code.contains("{error, Reason}"));
        assertTrue(code.contains("parse_response"));
    }
    
    @Test
    public void testGenerateQueryBuildingCodeWithNoQuery() {
        StructureShape inputShape = StructureShape.builder()
                .id("com.example#GetUserInput")
                .build();
        
        OperationShape operation = OperationShape.builder()
                .id("com.example#GetUser")
                .build();
        
        String code = HelperCodeGenerator.generateQueryBuildingCode(operation, inputShape, "Input");
        
        assertNotNull(code);
        assertEquals("    Query = <<\"\">>", code);
    }
    
    @Test
    public void testGenerateHeaderBuildingCodeWithNoHeaders() {
        StructureShape inputShape = StructureShape.builder()
                .id("com.example#GetUserInput")
                .build();
        
        OperationShape operation = OperationShape.builder()
                .id("com.example#GetUser")
                .build();
        
        String code = HelperCodeGenerator.generateHeaderBuildingCode(operation, inputShape, "Input");
        
        assertNotNull(code);
        assertTrue(code.contains("BaseHeaders = [{<<\"Content-Type\">>, <<\"application/json\">>}]"));
        assertTrue(code.contains("HeaderMapping = ["));
        assertTrue(code.contains("runtime_http_request:build_headers"));
    }
    
    @Test
    public void testGenerateResponseParsingCode() {
        StructureShape outputShape = StructureShape.builder()
                .id("com.example#GetUserOutput")
                .build();
        
        OperationShape operation = OperationShape.builder()
                .id("com.example#GetUser")
                .build();
        
        String code = HelperCodeGenerator.generateResponseParsingCode(operation, outputShape, 200);
        
        assertNotNull(code);
        assertTrue(code.contains("ExpectedStatus = 200"));
        assertTrue(code.contains("ResponseHeaderMapping = ["));
        assertTrue(code.contains("BodyType = json"));
        assertTrue(code.contains("runtime_http_response:parse_response"));
    }
    
    @Test
    public void testGenerateResponseParsingCodeWithDifferentStatusCode() {
        StructureShape outputShape = StructureShape.builder()
                .id("com.example#CreateUserOutput")
                .build();
        
        OperationShape operation = OperationShape.builder()
                .id("com.example#CreateUser")
                .build();
        
        String code = HelperCodeGenerator.generateResponseParsingCode(operation, outputShape, 201);
        
        assertNotNull(code);
        assertTrue(code.contains("ExpectedStatus = 201"));
        assertTrue(code.contains("runtime_http_response:parse_response"));
    }
}
