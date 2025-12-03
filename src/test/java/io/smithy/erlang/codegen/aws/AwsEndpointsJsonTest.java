package io.smithy.erlang.codegen.aws;

import org.junit.jupiter.api.Test;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.node.ObjectNode;
import software.amazon.smithy.model.node.ArrayNode;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for AWS endpoints.json file.
 * 
 * Verifies that the bundled endpoints.json file:
 * - Can be loaded from resources
 * - Contains valid JSON
 * - Has the expected structure (partitions, services, regions)
 */
public class AwsEndpointsJsonTest {
    
    private static final String ENDPOINTS_PATH = "aws/endpoints.json";
    
    @Test
    public void testEndpointsJsonExists() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH);
        assertNotNull(stream, "endpoints.json should exist in resources at " + ENDPOINTS_PATH);
    }
    
    @Test
    public void testEndpointsJsonIsValidJson() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH);
        assertNotNull(stream);
        
        assertDoesNotThrow(() -> {
            String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
            Node.parse(content);
        }, "endpoints.json should be valid JSON");
    }
    
    @Test
    public void testEndpointsJsonHasPartitions() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH);
        assertNotNull(stream);
        
        String content = assertDoesNotThrow(() -> 
            new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        
        ObjectNode root = Node.parse(content).expectObjectNode();
        assertTrue(root.containsMember("partitions"), 
            "endpoints.json should have 'partitions' array");
        
        ArrayNode partitions = root.expectArrayMember("partitions");
        assertTrue(partitions.size() > 0, 
            "partitions array should not be empty");
    }
    
    @Test
    public void testEndpointsJsonHasAwsPartition() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH);
        assertNotNull(stream);
        
        String content = assertDoesNotThrow(() -> 
            new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        
        ObjectNode root = Node.parse(content).expectObjectNode();
        ArrayNode partitions = root.expectArrayMember("partitions");
        
        // Find the 'aws' partition (standard AWS regions)
        boolean hasAwsPartition = partitions.getElements().stream()
            .map(Node::expectObjectNode)
            .anyMatch(p -> p.getStringMemberOrDefault("partition", "").equals("aws"));
        
        assertTrue(hasAwsPartition, 
            "endpoints.json should have 'aws' partition for standard AWS regions");
    }
    
    @Test
    public void testEndpointsJsonHasAwsChinaPartition() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH);
        assertNotNull(stream);
        
        String content = assertDoesNotThrow(() -> 
            new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        
        ObjectNode root = Node.parse(content).expectObjectNode();
        ArrayNode partitions = root.expectArrayMember("partitions");
        
        // Find the 'aws-cn' partition (China regions)
        boolean hasAwsCnPartition = partitions.getElements().stream()
            .map(Node::expectObjectNode)
            .anyMatch(p -> p.getStringMemberOrDefault("partition", "").equals("aws-cn"));
        
        assertTrue(hasAwsCnPartition, 
            "endpoints.json should have 'aws-cn' partition for China regions");
    }
    
    @Test
    public void testEndpointsJsonHasAwsGovPartition() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH);
        assertNotNull(stream);
        
        String content = assertDoesNotThrow(() -> 
            new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        
        ObjectNode root = Node.parse(content).expectObjectNode();
        ArrayNode partitions = root.expectArrayMember("partitions");
        
        // Find the 'aws-us-gov' partition (GovCloud regions)
        boolean hasAwsGovPartition = partitions.getElements().stream()
            .map(Node::expectObjectNode)
            .anyMatch(p -> p.getStringMemberOrDefault("partition", "").equals("aws-us-gov"));
        
        assertTrue(hasAwsGovPartition, 
            "endpoints.json should have 'aws-us-gov' partition for GovCloud regions");
    }
    
    @Test
    public void testAwsPartitionHasRegions() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH);
        assertNotNull(stream);
        
        String content = assertDoesNotThrow(() -> 
            new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        
        ObjectNode root = Node.parse(content).expectObjectNode();
        ArrayNode partitions = root.expectArrayMember("partitions");
        
        // Find the 'aws' partition and check for regions
        ObjectNode awsPartition = partitions.getElements().stream()
            .map(Node::expectObjectNode)
            .filter(p -> p.getStringMemberOrDefault("partition", "").equals("aws"))
            .findFirst()
            .orElseThrow(() -> new AssertionError("aws partition not found"));
        
        assertTrue(awsPartition.containsMember("regions"), 
            "aws partition should have 'regions' member");
        
        ObjectNode regions = awsPartition.expectObjectMember("regions");
        assertTrue(regions.getMembers().size() > 0, 
            "regions should not be empty");
        
        // Check for some well-known regions
        assertTrue(regions.containsMember("us-east-1"), 
            "regions should include us-east-1");
        assertTrue(regions.containsMember("eu-west-1"), 
            "regions should include eu-west-1");
        assertTrue(regions.containsMember("ap-northeast-1"), 
            "regions should include ap-northeast-1");
    }
    
    @Test
    public void testAwsPartitionHasServices() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH);
        assertNotNull(stream);
        
        String content = assertDoesNotThrow(() -> 
            new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        
        ObjectNode root = Node.parse(content).expectObjectNode();
        ArrayNode partitions = root.expectArrayMember("partitions");
        
        // Find the 'aws' partition and check for services
        ObjectNode awsPartition = partitions.getElements().stream()
            .map(Node::expectObjectNode)
            .filter(p -> p.getStringMemberOrDefault("partition", "").equals("aws"))
            .findFirst()
            .orElseThrow(() -> new AssertionError("aws partition not found"));
        
        assertTrue(awsPartition.containsMember("services"), 
            "aws partition should have 'services' member");
        
        ObjectNode services = awsPartition.expectObjectMember("services");
        assertTrue(services.getMembers().size() > 0, 
            "services should not be empty");
        
        // Check for some well-known services
        assertTrue(services.containsMember("s3"), 
            "services should include s3");
        assertTrue(services.containsMember("dynamodb"), 
            "services should include dynamodb");
        assertTrue(services.containsMember("sqs"), 
            "services should include sqs");
        assertTrue(services.containsMember("lambda"), 
            "services should include lambda");
    }
    
    @Test
    public void testAwsPartitionHasDefaults() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH);
        assertNotNull(stream);
        
        String content = assertDoesNotThrow(() -> 
            new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        
        ObjectNode root = Node.parse(content).expectObjectNode();
        ArrayNode partitions = root.expectArrayMember("partitions");
        
        // Find the 'aws' partition and check for defaults
        ObjectNode awsPartition = partitions.getElements().stream()
            .map(Node::expectObjectNode)
            .filter(p -> p.getStringMemberOrDefault("partition", "").equals("aws"))
            .findFirst()
            .orElseThrow(() -> new AssertionError("aws partition not found"));
        
        assertTrue(awsPartition.containsMember("defaults"), 
            "aws partition should have 'defaults' member");
        
        ObjectNode defaults = awsPartition.expectObjectMember("defaults");
        assertTrue(defaults.containsMember("hostname"), 
            "defaults should have 'hostname' template");
    }
    
    @Test
    public void testAwsPartitionHasDnsSuffix() {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH);
        assertNotNull(stream);
        
        String content = assertDoesNotThrow(() -> 
            new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        
        ObjectNode root = Node.parse(content).expectObjectNode();
        ArrayNode partitions = root.expectArrayMember("partitions");
        
        // Find the 'aws' partition and check for dnsSuffix
        ObjectNode awsPartition = partitions.getElements().stream()
            .map(Node::expectObjectNode)
            .filter(p -> p.getStringMemberOrDefault("partition", "").equals("aws"))
            .findFirst()
            .orElseThrow(() -> new AssertionError("aws partition not found"));
        
        String dnsSuffix = awsPartition.getStringMemberOrDefault("dnsSuffix", "");
        assertEquals("amazonaws.com", dnsSuffix, 
            "aws partition should have 'amazonaws.com' as dnsSuffix");
    }
}
