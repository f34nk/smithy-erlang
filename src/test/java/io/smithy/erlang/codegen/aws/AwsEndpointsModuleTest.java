package io.smithy.erlang.codegen.aws;

import org.junit.jupiter.api.Test;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for AWS endpoints Erlang module.
 * 
 * Verifies that the aws_endpoints.erl module:
 * - Exists in resources
 * - Has valid Erlang syntax structure
 * - Exports the expected functions
 * - Contains endpoint resolution logic
 */
public class AwsEndpointsModuleTest {
    
    private static final String MODULE_PATH = "aws_endpoints.erl";
    
    @Test
    public void testModuleExists() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(MODULE_PATH);
        assertNotNull(stream, "aws_endpoints.erl should exist in resources");
    }
    
    @Test
    public void testModuleHasModuleDeclaration() {
        String content = loadModule();
        assertTrue(content.contains("-module(aws_endpoints)."),
            "Module should have correct module declaration");
    }
    
    @Test
    public void testModuleExportsResolve2() {
        String content = loadModule();
        assertTrue(content.contains("resolve/2"),
            "Module should export resolve/2");
    }
    
    @Test
    public void testModuleExportsResolve3() {
        String content = loadModule();
        assertTrue(content.contains("resolve/3"),
            "Module should export resolve/3");
    }
    
    @Test
    public void testModuleExportsIsGlobalService() {
        String content = loadModule();
        assertTrue(content.contains("is_global_service/1"),
            "Module should export is_global_service/1");
    }
    
    @Test
    public void testModuleExportsGetPartition() {
        String content = loadModule();
        assertTrue(content.contains("get_partition/1"),
            "Module should export get_partition/1");
    }
    
    @Test
    public void testModuleExportsGetDnsSuffix() {
        String content = loadModule();
        assertTrue(content.contains("get_dns_suffix/1"),
            "Module should export get_dns_suffix/1");
    }
    
    @Test
    public void testModuleExportsGetSigningRegion() {
        String content = loadModule();
        assertTrue(content.contains("get_signing_region/2"),
            "Module should export get_signing_region/2");
    }
    
    @Test
    public void testModuleHasGlobalServices() {
        String content = loadModule();
        // Check for known global services
        assertTrue(content.contains("<<\"iam\">>"),
            "Module should know IAM is global");
        assertTrue(content.contains("<<\"route53\">>"),
            "Module should know Route53 is global");
        assertTrue(content.contains("<<\"cloudfront\">>"),
            "Module should know CloudFront is global");
    }
    
    @Test
    public void testModuleHasPartitions() {
        String content = loadModule();
        // Check for partition definitions
        assertTrue(content.contains("<<\"aws\">>"),
            "Module should have aws partition");
        assertTrue(content.contains("<<\"aws-cn\">>"),
            "Module should have aws-cn partition");
        assertTrue(content.contains("<<\"aws-us-gov\">>"),
            "Module should have aws-us-gov partition");
    }
    
    @Test
    public void testModuleHasDnsSuffixes() {
        String content = loadModule();
        // Check for DNS suffixes
        assertTrue(content.contains("<<\"amazonaws.com\">>"),
            "Module should have amazonaws.com suffix");
        assertTrue(content.contains("<<\"amazonaws.com.cn\">>"),
            "Module should have amazonaws.com.cn suffix for China");
    }
    
    @Test
    public void testModuleHasRegions() {
        String content = loadModule();
        // Check for common regions
        assertTrue(content.contains("<<\"us-east-1\">>"),
            "Module should have us-east-1 region");
        assertTrue(content.contains("<<\"eu-west-1\">>"),
            "Module should have eu-west-1 region");
        assertTrue(content.contains("<<\"ap-northeast-1\">>"),
            "Module should have ap-northeast-1 region");
    }
    
    @Test
    public void testModuleHasChinaRegions() {
        String content = loadModule();
        // Check for China regions
        assertTrue(content.contains("<<\"cn-north-1\">>"),
            "Module should have cn-north-1 region");
        assertTrue(content.contains("<<\"cn-northwest-1\">>"),
            "Module should have cn-northwest-1 region");
    }
    
    @Test
    public void testModuleHasGovCloudRegions() {
        String content = loadModule();
        // Check for GovCloud regions
        assertTrue(content.contains("<<\"us-gov-west-1\">>"),
            "Module should have us-gov-west-1 region");
        assertTrue(content.contains("<<\"us-gov-east-1\">>"),
            "Module should have us-gov-east-1 region");
    }
    
    @Test
    public void testModuleHasFipsSupport() {
        String content = loadModule();
        // Check for FIPS endpoint support
        assertTrue(content.contains("use_fips"),
            "Module should support FIPS endpoints");
        assertTrue(content.contains("-fips."),
            "Module should generate FIPS hostname patterns");
    }
    
    @Test
    public void testModuleHasDualStackSupport() {
        String content = loadModule();
        // Check for dual-stack endpoint support
        assertTrue(content.contains("use_dual_stack"),
            "Module should support dual-stack endpoints");
        assertTrue(content.contains(".dualstack."),
            "Module should generate dual-stack hostname patterns");
    }
    
    @Test
    public void testModuleHasEndpointOverrideSupport() {
        String content = loadModule();
        // Check for custom endpoint override support
        assertTrue(content.contains("endpoint_override"),
            "Module should support custom endpoint override");
    }
    
    @Test
    public void testModuleHasGlobalEndpoints() {
        String content = loadModule();
        // Check for global endpoint definitions
        assertTrue(content.contains("<<\"iam.amazonaws.com\">>"),
            "Module should have IAM global endpoint");
        assertTrue(content.contains("<<\"route53.amazonaws.com\">>"),
            "Module should have Route53 global endpoint");
    }
    
    @Test
    public void testModuleHasTypeDefinitions() {
        String content = loadModule();
        // Check for type definitions
        assertTrue(content.contains("-type service()"),
            "Module should have service type");
        assertTrue(content.contains("-type region()"),
            "Module should have region type");
        assertTrue(content.contains("-type endpoint_info()"),
            "Module should have endpoint_info type");
        assertTrue(content.contains("-type options()"),
            "Module should have options type");
    }
    
    @Test
    public void testModuleHasEndpointInfoFields() {
        String content = loadModule();
        // Check for endpoint_info fields
        assertTrue(content.contains("hostname :="),
            "endpoint_info should have hostname field");
        assertTrue(content.contains("url :="),
            "endpoint_info should have url field");
        assertTrue(content.contains("signing_region :="),
            "endpoint_info should have signing_region field");
        assertTrue(content.contains("signing_name :="),
            "endpoint_info should have signing_name field");
        assertTrue(content.contains("dns_suffix :="),
            "endpoint_info should have dns_suffix field");
        assertTrue(content.contains("partition :="),
            "endpoint_info should have partition field");
    }
    
    private String loadModule() {
        try {
            InputStream stream = getClass().getClassLoader().getResourceAsStream(MODULE_PATH);
            assertNotNull(stream);
            return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        } catch (Exception e) {
            fail("Failed to load module: " + e.getMessage());
            return null;
        }
    }
}
