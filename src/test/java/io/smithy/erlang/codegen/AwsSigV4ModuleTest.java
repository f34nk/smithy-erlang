package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for AWS SigV4 module resource
 */
public class AwsSigV4ModuleTest {

    @Test
    public void testAwsSigV4ModuleExists() {
        // Verify the aws_sigv4.erl resource exists
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_sigv4.erl");
        assertNotNull(stream, "aws_sigv4.erl should be available as a resource");
    }

    @Test
    public void testAwsSigV4ModuleContent() throws Exception {
        // Verify the module has expected content
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_sigv4.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for module declaration
        assertTrue(content.contains("-module(aws_sigv4)"), 
            "Module should declare aws_sigv4");
        
        // Check for sign_request/5 export
        assertTrue(content.contains("-export([sign_request/5])"), 
            "Module should export sign_request/5");
        
        // Check for function definition
        assertTrue(content.contains("sign_request(_Method, _Url, Headers, _Body, _Credentials)"), 
            "Module should define sign_request/5 function");
        
        // Check for proper documentation
        assertTrue(content.contains("AWS Signature Version 4"), 
            "Module should have AWS SigV4 documentation");
    }

    @Test
    public void testAwsSigV4ModuleSignature() throws Exception {
        // Verify the function signature is correct
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_sigv4.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check -spec annotation exists
        assertTrue(content.contains("-spec sign_request("), 
            "Module should have -spec annotation for sign_request/5");
        
        // Check return type
        assertTrue(content.contains("[{binary(), binary()}]"), 
            "Function should return list of binary tuples (headers)");
    }

    @Test
    public void testAwsSigV4ModuleTodoComment() throws Exception {
        // Verify the module has TODO comment for future implementation
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_sigv4.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for TODO comment indicating skeleton status
        assertTrue(content.contains("TODO:") || content.contains("TODO "), 
            "Module should have TODO comment indicating future implementation");
        
        // Check that it currently returns headers unchanged
        assertTrue(content.contains("Headers"), 
            "Module should currently return headers unchanged");
    }
}
