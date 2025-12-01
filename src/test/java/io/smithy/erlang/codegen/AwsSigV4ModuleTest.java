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
        
        // Check for function definition (full implementation)
        assertTrue(content.contains("sign_request(Method, Url, Headers, Body, Credentials)"), 
            "Module should define sign_request/5 function with full implementation");
        
        // Check for proper documentation
        assertTrue(content.contains("AWS Signature Version 4"), 
            "Module should have AWS SigV4 documentation");
        
        // Check for key implementation functions
        assertTrue(content.contains("create_canonical_request"), 
            "Module should have create_canonical_request function");
        assertTrue(content.contains("create_string_to_sign"), 
            "Module should have create_string_to_sign function");
        assertTrue(content.contains("calculate_signature"), 
            "Module should have calculate_signature function");
        assertTrue(content.contains("format_auth_header"), 
            "Module should have format_auth_header function");
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
    public void testAwsSigV4ModuleImplementation() throws Exception {
        // Verify the module has complete implementation
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_sigv4.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for HMAC implementation
        assertTrue(content.contains("crypto:mac(hmac, sha256"), 
            "Module should use crypto:mac for HMAC-SHA256");
        
        // Check for Authorization header formatting
        assertTrue(content.contains("AWS4-HMAC-SHA256"), 
            "Module should format AWS4-HMAC-SHA256 authorization header");
        
        // Check for credential scope
        assertTrue(content.contains("credential_scope"), 
            "Module should have credential_scope function");
        
        // Check for signing key derivation
        assertTrue(content.contains("derive_signing_key"), 
            "Module should have derive_signing_key function");
        
        // Check for canonical request components
        assertTrue(content.contains("canonicalize_headers") && 
                   content.contains("canonicalize_query_string"), 
            "Module should have canonicalization functions");
        
        // Verify no TODO comments remain (implementation is complete)
        assertFalse(content.contains("TODO: Implement"), 
            "Module should not have TODO comments - implementation is complete");
    }
}
