package io.smithy.erlang.codegen.aws;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for AwsEndpointResolver.
 * 
 * Verifies endpoint resolution for:
 * - Standard regional services (S3, DynamoDB, etc.)
 * - Global services (IAM, Route53, etc.)
 * - Different partitions (aws, aws-cn, aws-us-gov)
 * - Service-specific endpoint overrides
 */
public class AwsEndpointResolverTest {
    
    private static AwsEndpointResolver resolver;
    
    @BeforeAll
    public static void setUp() {
        resolver = new AwsEndpointResolver();
    }
    
    // ===== Basic Resolution Tests =====
    
    @Test
    public void testResolverLoads() {
        assertNotNull(resolver, "Resolver should load successfully");
    }
    
    @Test
    public void testResolveS3UsEast1() {
        EndpointInfo endpoint = resolver.resolveEndpoint("s3", "us-east-1");
        assertNotNull(endpoint);
        assertEquals("s3.us-east-1.amazonaws.com", endpoint.getHostname());
        assertEquals("https://s3.us-east-1.amazonaws.com", endpoint.getUrl());
    }
    
    @Test
    public void testResolveS3EuWest1() {
        EndpointInfo endpoint = resolver.resolveEndpoint("s3", "eu-west-1");
        assertNotNull(endpoint);
        assertEquals("s3.eu-west-1.amazonaws.com", endpoint.getHostname());
    }
    
    @Test
    public void testResolveDynamoDBUsEast1() {
        EndpointInfo endpoint = resolver.resolveEndpoint("dynamodb", "us-east-1");
        assertNotNull(endpoint);
        assertEquals("dynamodb.us-east-1.amazonaws.com", endpoint.getHostname());
    }
    
    @Test
    public void testResolveSQSUsWest2() {
        EndpointInfo endpoint = resolver.resolveEndpoint("sqs", "us-west-2");
        assertNotNull(endpoint);
        assertEquals("sqs.us-west-2.amazonaws.com", endpoint.getHostname());
    }
    
    @Test
    public void testResolveLambdaApNortheast1() {
        EndpointInfo endpoint = resolver.resolveEndpoint("lambda", "ap-northeast-1");
        assertNotNull(endpoint);
        assertEquals("lambda.ap-northeast-1.amazonaws.com", endpoint.getHostname());
    }
    
    // ===== Global Service Tests =====
    
    @Test
    public void testResolveIAMGlobal() {
        EndpointInfo endpoint = resolver.resolveEndpoint("iam", "us-east-1");
        assertNotNull(endpoint);
        // IAM uses a global endpoint
        assertEquals("iam.amazonaws.com", endpoint.getHostname());
    }
    
    @Test
    public void testIsGlobalServiceIAM() {
        assertTrue(resolver.isGlobalService("iam"), 
            "IAM should be recognized as a global service");
    }
    
    @Test
    public void testIsGlobalServiceRoute53() {
        assertTrue(resolver.isGlobalService("route53"), 
            "Route53 should be recognized as a global service");
    }
    
    @Test
    public void testIsGlobalServiceCloudFront() {
        assertTrue(resolver.isGlobalService("cloudfront"), 
            "CloudFront should be recognized as a global service");
    }
    
    @Test
    public void testIsNotGlobalServiceS3() {
        assertFalse(resolver.isGlobalService("s3"), 
            "S3 should not be recognized as a global service");
    }
    
    @Test
    public void testIsNotGlobalServiceDynamoDB() {
        assertFalse(resolver.isGlobalService("dynamodb"), 
            "DynamoDB should not be recognized as a global service");
    }
    
    // ===== Signing Region Tests =====
    
    @Test
    public void testSigningRegionForRegionalService() {
        EndpointInfo endpoint = resolver.resolveEndpoint("s3", "eu-west-1");
        assertTrue(endpoint.getSigningRegion().isPresent());
        assertEquals("eu-west-1", endpoint.getSigningRegion().get());
    }
    
    @Test
    public void testSigningRegionForIAM() {
        EndpointInfo endpoint = resolver.resolveEndpoint("iam", "us-west-2");
        // IAM global endpoint uses us-east-1 for signing
        assertTrue(endpoint.getSigningRegion().isPresent());
        assertEquals("us-east-1", endpoint.getSigningRegion().get());
    }
    
    @Test
    public void testGlobalServiceSigningRegion() {
        var signingRegion = resolver.getGlobalServiceSigningRegion("iam");
        assertTrue(signingRegion.isPresent());
        assertEquals("us-east-1", signingRegion.get());
    }
    
    // ===== Partition Tests =====
    
    @Test
    public void testPartitionForUsEast1() {
        assertEquals("aws", resolver.getPartitionForRegion("us-east-1"));
    }
    
    @Test
    public void testPartitionForEuWest1() {
        assertEquals("aws", resolver.getPartitionForRegion("eu-west-1"));
    }
    
    @Test
    public void testPartitionForCnNorth1() {
        assertEquals("aws-cn", resolver.getPartitionForRegion("cn-north-1"));
    }
    
    @Test
    public void testPartitionForCnNorthwest1() {
        assertEquals("aws-cn", resolver.getPartitionForRegion("cn-northwest-1"));
    }
    
    @Test
    public void testPartitionForUsGovWest1() {
        assertEquals("aws-us-gov", resolver.getPartitionForRegion("us-gov-west-1"));
    }
    
    @Test
    public void testPartitionForUsGovEast1() {
        assertEquals("aws-us-gov", resolver.getPartitionForRegion("us-gov-east-1"));
    }
    
    // ===== DNS Suffix Tests =====
    
    @Test
    public void testDnsSuffixAws() {
        assertEquals("amazonaws.com", resolver.getDnsSuffix("aws"));
    }
    
    @Test
    public void testDnsSuffixAwsCn() {
        assertEquals("amazonaws.com.cn", resolver.getDnsSuffix("aws-cn"));
    }
    
    @Test
    public void testDnsSuffixAwsUsGov() {
        assertEquals("amazonaws.com", resolver.getDnsSuffix("aws-us-gov"));
    }
    
    // ===== China Partition Tests =====
    
    @Test
    public void testResolveS3CnNorth1() {
        EndpointInfo endpoint = resolver.resolveEndpoint("s3", "cn-north-1");
        assertNotNull(endpoint);
        assertEquals("s3.cn-north-1.amazonaws.com.cn", endpoint.getHostname());
        assertEquals("amazonaws.com.cn", endpoint.getDnsSuffix().orElse(null));
    }
    
    @Test
    public void testResolveDynamoDBCnNorthwest1() {
        EndpointInfo endpoint = resolver.resolveEndpoint("dynamodb", "cn-northwest-1");
        assertNotNull(endpoint);
        assertEquals("dynamodb.cn-northwest-1.amazonaws.com.cn", endpoint.getHostname());
    }
    
    // ===== GovCloud Partition Tests =====
    
    @Test
    public void testResolveS3UsGovWest1() {
        EndpointInfo endpoint = resolver.resolveEndpoint("s3", "us-gov-west-1");
        assertNotNull(endpoint);
        // GovCloud S3 uses a specific endpoint pattern
        assertTrue(endpoint.getHostname().contains("us-gov-west-1"));
    }
    
    @Test
    public void testResolveDynamoDBUsGovEast1() {
        EndpointInfo endpoint = resolver.resolveEndpoint("dynamodb", "us-gov-east-1");
        assertNotNull(endpoint);
        assertEquals("dynamodb.us-gov-east-1.amazonaws.com", endpoint.getHostname());
    }
    
    // ===== EndpointInfo Tests =====
    
    @Test
    public void testEndpointInfoBuilder() {
        EndpointInfo info = EndpointInfo.builder()
            .hostname("test.amazonaws.com")
            .signingRegion("us-east-1")
            .signingName("test")
            .dnsSuffix("amazonaws.com")
            .partition("aws")
            .build();
        
        assertEquals("test.amazonaws.com", info.getHostname());
        assertEquals("https://test.amazonaws.com", info.getUrl());
        assertEquals("us-east-1", info.getSigningRegion().get());
        assertEquals("test", info.getSigningName().get());
        assertEquals("amazonaws.com", info.getDnsSuffix().get());
        assertEquals("aws", info.getPartition().get());
    }
    
    @Test
    public void testEndpointInfoEquality() {
        EndpointInfo info1 = EndpointInfo.builder()
            .hostname("test.amazonaws.com")
            .signingRegion("us-east-1")
            .build();
        
        EndpointInfo info2 = EndpointInfo.builder()
            .hostname("test.amazonaws.com")
            .signingRegion("us-east-1")
            .build();
        
        assertEquals(info1, info2);
        assertEquals(info1.hashCode(), info2.hashCode());
    }
    
    @Test
    public void testEndpointInfoToString() {
        EndpointInfo info = EndpointInfo.builder()
            .hostname("test.amazonaws.com")
            .build();
        
        String str = info.toString();
        assertTrue(str.contains("test.amazonaws.com"));
        assertTrue(str.contains("EndpointInfo"));
    }
    
    // ===== Edge Cases =====
    
    @Test
    public void testResolveUnknownService() {
        // Unknown services should use the default pattern
        EndpointInfo endpoint = resolver.resolveEndpoint("unknownservice", "us-east-1");
        assertNotNull(endpoint);
        assertEquals("unknownservice.us-east-1.amazonaws.com", endpoint.getHostname());
    }
    
    @Test
    public void testResolveWithUnknownRegionDefaultsToAws() {
        // Unknown regions should default to aws partition pattern
        EndpointInfo endpoint = resolver.resolveEndpoint("s3", "unknown-region-1");
        assertNotNull(endpoint);
        assertEquals("amazonaws.com", endpoint.getDnsSuffix().orElse(null));
    }
}
