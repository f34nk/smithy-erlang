package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for AWS credentials module resource
 */
public class AwsCredentialsModuleTest {

    @Test
    public void testAwsCredentialsModuleExists() {
        // Verify the aws_credentials.erl resource exists
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_credentials.erl");
        assertNotNull(stream, "aws_credentials.erl should be available as a resource");
    }

    @Test
    public void testAwsCredentialsModuleContent() throws Exception {
        // Verify the module has expected content
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_credentials.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for module declaration
        assertTrue(content.contains("-module(aws_credentials)"), 
            "Module should declare aws_credentials");
        
        // Check for exports
        assertTrue(content.contains("-export(["), 
            "Module should have exports");
        assertTrue(content.contains("get_credentials/0"), 
            "Module should export get_credentials/0");
        assertTrue(content.contains("from_environment/0"), 
            "Module should export from_environment/0");
    }

    @Test
    public void testFromEnvironmentFunction() throws Exception {
        // Verify from_environment/0 function exists and has correct implementation
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_credentials.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for function definition
        assertTrue(content.contains("from_environment()"), 
            "Module should define from_environment/0 function");
        
        // Check for environment variable reading
        assertTrue(content.contains("os:getenv(\"AWS_ACCESS_KEY_ID\")"), 
            "Function should read AWS_ACCESS_KEY_ID");
        assertTrue(content.contains("os:getenv(\"AWS_SECRET_ACCESS_KEY\")"), 
            "Function should read AWS_SECRET_ACCESS_KEY");
        assertTrue(content.contains("os:getenv(\"AWS_SESSION_TOKEN\")"), 
            "Function should read AWS_SESSION_TOKEN");
        
        // Check for error handling
        assertTrue(content.contains("no_access_key"), 
            "Function should return error for missing access key");
        assertTrue(content.contains("no_secret_key"), 
            "Function should return error for missing secret key");
    }

    @Test
    public void testGetCredentialsFunction() throws Exception {
        // Verify get_credentials/0 function exists
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_credentials.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for function definition
        assertTrue(content.contains("get_credentials()"), 
            "Module should define get_credentials/0 function");
        
        // Check for credential chain logic
        assertTrue(content.contains("try_providers") || content.contains("Providers"), 
            "Function should implement credential provider chain");
    }

    @Test
    public void testModuleDocumentation() throws Exception {
        // Verify the module has proper documentation
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_credentials.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for documentation comments
        assertTrue(content.contains("@doc") || content.contains("%%"), 
            "Module should have documentation");
        
        // Check for -spec annotations
        assertTrue(content.contains("-spec from_environment()"), 
            "Function should have -spec annotation");
        assertTrue(content.contains("-spec get_credentials()"), 
            "Function should have -spec annotation");
    }

    @Test
    public void testReturnTypes() throws Exception {
        // Verify return types match AWS SDK patterns
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_credentials.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for {ok, map()} return type
        assertTrue(content.contains("{ok, map()}"), 
            "Functions should return {ok, map()} on success");
        
        // Check for {error, atom()} return type
        assertTrue(content.contains("{error, atom()}"), 
            "Functions should return {error, atom()} on failure");
        
        // Check for credential map fields
        assertTrue(content.contains("access_key_id"), 
            "Credentials should have access_key_id field");
        assertTrue(content.contains("secret_access_key"), 
            "Credentials should have secret_access_key field");
        assertTrue(content.contains("session_token"), 
            "Credentials should support session_token field");
    }

    @Test
    public void testBinaryConversion() throws Exception {
        // Verify environment variables are converted to binary
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_credentials.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for list_to_binary conversion
        assertTrue(content.contains("list_to_binary"), 
            "Module should convert environment variables to binary");
    }

    @Test
    public void testCredentialChain() throws Exception {
        // Verify credential provider chain implementation
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_credentials.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for multiple provider support
        assertTrue(content.contains("from_environment"), 
            "Chain should include environment provider");
        
        // Check for future provider placeholders
        assertTrue(content.contains("TODO") || content.contains("from_credentials_file") 
                   || content.contains("Provider"), 
            "Module should have structure for multiple providers");
    }
}
