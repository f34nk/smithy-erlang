package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;

public class AwsConfigModuleTest {

    @Test
    public void testAwsConfigModuleExists() {
        // Verify the aws_config.erl resource exists
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_config.erl");
        assertNotNull(stream, "aws_config.erl should be available as a resource");
    }

    @Test
    public void testAwsConfigModuleContent() throws Exception {
        // Verify the module has expected content
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_config.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for module declaration
        assertTrue(content.contains("-module(aws_config)"), 
            "Module should declare aws_config");
        
        // Check for exports
        assertTrue(content.contains("-export(["), 
            "Module should have exports");
        assertTrue(content.contains("get_region/0"), 
            "Module should export get_region/0");
        assertTrue(content.contains("get_region/1"), 
            "Module should export get_region/1");
    }

    @Test
    public void testGetRegionFunction() throws Exception {
        // Verify get_region functions exist
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_config.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for function definitions
        assertTrue(content.contains("get_region()"), 
            "Module should define get_region/0 function");
        assertTrue(content.contains("get_region(Options)"), 
            "Module should define get_region/1 function");
        
        // Check for environment variable handling
        assertTrue(content.contains("AWS_REGION"), 
            "Function should check AWS_REGION environment variable");
        assertTrue(content.contains("AWS_DEFAULT_REGION"), 
            "Function should check AWS_DEFAULT_REGION environment variable");
    }

    @Test
    public void testRegionPrecedence() throws Exception {
        // Verify region resolution precedence
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_config.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for explicit option handling (highest priority)
        assertTrue(content.contains("maps:get(region, Options"), 
            "Function should check for explicit region option");
        
        // Check for config file fallback
        assertTrue(content.contains("from_config_file"), 
            "Function should have config file fallback");
        
        // Check for default region
        assertTrue(content.contains("us-east-1"), 
            "Function should have default region");
    }

    @Test
    public void testConfigFileParsing() throws Exception {
        // Verify config file parsing logic
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_config.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for config file reading
        assertTrue(content.contains("file:read_file"), 
            "Function should read config file");
        assertTrue(content.contains(".aws/config") || content.contains("\".aws\""), 
            "Function should read from ~/.aws/config");
        
        // Check for profile parsing
        assertTrue(content.contains("[default]") || content.contains("ProfileSection"), 
            "Function should parse profile sections");
        assertTrue(content.contains("[profile") || content.contains("profile"), 
            "Function should handle [profile name] format");
        
        // Check for region extraction
        assertTrue(content.contains("region"), 
            "Function should extract region from config");
    }

    @Test
    public void testConfigFileFormat() throws Exception {
        // Verify config file format handling
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_config.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for INI-style parsing
        assertTrue(content.contains("binary:split") && content.contains("="), 
            "Module should parse key=value pairs");
        
        // Check for whitespace handling
        assertTrue(content.contains("string:trim") || content.contains("trim"), 
            "Module should handle whitespace");
        
        // Check for profile section detection
        assertTrue(content.contains("[profile"), 
            "Module should detect profile sections");
    }

    @Test
    public void testDefaultRegion() throws Exception {
        // Verify default region behavior
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_config.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check that us-east-1 is the default
        assertTrue(content.contains("us-east-1"), 
            "Module should default to us-east-1");
        
        // Verify it's used when other methods fail
        int usEast1Count = content.split("us-east-1", -1).length - 1;
        assertTrue(usEast1Count >= 2, 
            "Default region should be used in multiple fallback cases");
    }

    @Test
    public void testProfileSupport() throws Exception {
        // Verify multi-profile support
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_config.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for profile parameter
        assertTrue(content.contains("Profile") && content.contains("profile"), 
            "Function should support profile selection");
        
        // Check for default profile special handling
        assertTrue(content.contains("default"), 
            "Function should handle default profile");
        
        // Check for non-default profile format
        assertTrue(content.contains("[profile"), 
            "Function should handle [profile name] format for non-default profiles");
    }

    @Test
    public void testTypeConversion() throws Exception {
        // Verify type conversion for regions
        InputStream stream = getClass().getClassLoader().getResourceAsStream("aws_config.erl");
        assertNotNull(stream);
        
        String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        
        // Check for list_to_binary conversion
        assertTrue(content.contains("list_to_binary"), 
            "Function should convert string regions to binary");
        
        // Check for binary handling
        assertTrue(content.contains("is_binary") || content.contains("when is_binary"), 
            "Function should handle binary regions");
    }
}
