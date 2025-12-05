package io.smithy.erlang.codegen;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.build.PluginContext;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.ObjectNode;

import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for full AWS service client generation.
 * 
 * <p>These tests verify end-to-end code generation for real AWS service models,
 * ensuring the generated Erlang code is complete and correct.
 * 
 * <h2>Test Coverage</h2>
 * <ul>
 *   <li>AWS S3 (REST-XML protocol)</li>
 *   <li>DynamoDB (AWS JSON 1.0 protocol)</li>
 *   <li>SQS (AWS Query protocol)</li>
 * </ul>
 * 
 * <h2>Verification Points</h2>
 * <ul>
 *   <li>All expected files are generated</li>
 *   <li>Module declarations are correct</li>
 *   <li>Export declarations are present</li>
 *   <li>Type specifications are generated</li>
 *   <li>SigV4 signing is integrated</li>
 *   <li>Retry logic is included</li>
 *   <li>Protocol-specific features are present</li>
 * </ul>
 */
class IntegrationTest {
    
    @TempDir
    Path outputDir;
    
    private ErlangCodegenPlugin plugin;
    
    @BeforeEach
    void setUp() {
        plugin = new ErlangCodegenPlugin();
    }
    
    @Nested
    @DisplayName("S3 Client Generation (REST-XML)")
    class S3ClientGenerationTests {
        
        @Test
        @DisplayName("Generates S3 client module")
        void generatesS3ClientModule() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertNotNull(client.clientCode, "Client code should be generated");
            assertTrue(client.clientCode.length() > 0, "Client code should not be empty");
        }
        
        @Test
        @DisplayName("Contains module declaration")
        void containsModuleDeclaration() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(client.clientCode, "-module(");
        }
        
        @Test
        @DisplayName("Contains export declarations")
        void containsExportDeclarations() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(client.clientCode, "-export([");
        }
        
        @Test
        @DisplayName("Contains S3 operations")
        void containsS3Operations() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(client.clientCode, "get_object");
            assertContains(client.clientCode, "put_object");
            assertContains(client.clientCode, "delete_object");
        }
        
        @Test
        @DisplayName("Contains SigV4 signing")
        void containsSigV4Signing() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(client.clientCode, "aws_sigv4:sign_request");
        }
        
        @Test
        @DisplayName("Contains retry logic")
        void containsRetryLogic() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(client.clientCode, "aws_retry");
        }
        
        @Test
        @DisplayName("Uses aws_xml for encoding")
        void usesAwsXmlEncoding() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(client.clientCode, "aws_xml");
        }
        
        @Test
        @DisplayName("Generates runtime modules")
        void generatesRuntimeModules() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertTrue(client.hasFile("aws_sigv4.erl"), "Should generate aws_sigv4.erl");
            assertTrue(client.hasFile("aws_retry.erl"), "Should generate aws_retry.erl");
        }
    }
    
    @Nested
    @DisplayName("DynamoDB Client Generation (AWS JSON 1.0)")
    class DynamoDBClientGenerationTests {
        
        @Test
        @DisplayName("Generates DynamoDB client module")
        void generatesDynamoDBClientModule() throws Exception {
            GeneratedClient client = generateClient("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertNotNull(client.clientCode, "Client code should be generated");
            assertTrue(client.clientCode.length() > 0, "Client code should not be empty");
        }
        
        @Test
        @DisplayName("Contains module declaration")
        void containsModuleDeclaration() throws Exception {
            GeneratedClient client = generateClient("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertContains(client.clientCode, "-module(");
        }
        
        @Test
        @DisplayName("Contains export declarations")
        void containsExportDeclarations() throws Exception {
            GeneratedClient client = generateClient("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertContains(client.clientCode, "-export([");
        }
        
        @Test
        @DisplayName("Contains DynamoDB operations")
        void containsDynamoDBOperations() throws Exception {
            GeneratedClient client = generateClient("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertContains(client.clientCode, "get_item");
            assertContains(client.clientCode, "put_item");
            assertContains(client.clientCode, "query");
        }
        
        @Test
        @DisplayName("Contains X-Amz-Target header")
        void containsXAmzTargetHeader() throws Exception {
            GeneratedClient client = generateClient("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertContains(client.clientCode, "X-Amz-Target");
        }
        
        @Test
        @DisplayName("Uses jsx for JSON encoding")
        void usesJsxEncoding() throws Exception {
            GeneratedClient client = generateClient("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertContains(client.clientCode, "jsx:encode");
            assertContains(client.clientCode, "jsx:decode");
        }
        
        @Test
        @DisplayName("Contains POST method")
        void containsPostMethod() throws Exception {
            GeneratedClient client = generateClient("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertContains(client.clientCode, "<<\"POST\">>");
        }
        
        @Test
        @DisplayName("Contains SigV4 signing")
        void containsSigV4Signing() throws Exception {
            GeneratedClient client = generateClient("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertContains(client.clientCode, "aws_sigv4:sign_request");
        }
    }
    
    @Nested
    @DisplayName("SQS Client Generation (AWS Query)")
    class SQSClientGenerationTests {
        
        @Test
        @DisplayName("Generates SQS client module")
        void generatesSQSClientModule() throws Exception {
            GeneratedClient client = generateClient("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertNotNull(client.clientCode, "Client code should be generated");
            assertTrue(client.clientCode.length() > 0, "Client code should not be empty");
        }
        
        @Test
        @DisplayName("Contains module declaration")
        void containsModuleDeclaration() throws Exception {
            GeneratedClient client = generateClient("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertContains(client.clientCode, "-module(");
        }
        
        @Test
        @DisplayName("Contains export declarations")
        void containsExportDeclarations() throws Exception {
            GeneratedClient client = generateClient("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertContains(client.clientCode, "-export([");
        }
        
        @Test
        @DisplayName("Contains SQS operations")
        void containsSQSOperations() throws Exception {
            GeneratedClient client = generateClient("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertContains(client.clientCode, "send_message");
            assertContains(client.clientCode, "receive_message");
            assertContains(client.clientCode, "delete_message");
        }
        
        @Test
        @DisplayName("Uses form-urlencoded content type")
        void usesFormUrlencodedContentType() throws Exception {
            GeneratedClient client = generateClient("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertContains(client.clientCode, "application/x-www-form-urlencoded");
        }
        
        @Test
        @DisplayName("Uses aws_query for encoding")
        void usesAwsQueryEncoding() throws Exception {
            GeneratedClient client = generateClient("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertContains(client.clientCode, "aws_query:encode");
        }
        
        @Test
        @DisplayName("Contains SigV4 signing")
        void containsSigV4Signing() throws Exception {
            GeneratedClient client = generateClient("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertContains(client.clientCode, "aws_sigv4:sign_request");
        }
        
        @Test
        @DisplayName("Contains retry logic")
        void containsRetryLogic() throws Exception {
            GeneratedClient client = generateClient("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertContains(client.clientCode, "aws_retry");
        }
    }
    
    @Nested
    @DisplayName("Code Quality Verification")
    class CodeQualityTests {
        
        @Test
        @DisplayName("Generated code has type specs")
        void generatedCodeHasTypeSpecs() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(client.clientCode, "-spec ");
        }
        
        @Test
        @DisplayName("Generated code has function clauses")
        void generatedCodeHasFunctionClauses() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(client.clientCode, ") ->");
        }
        
        @Test
        @DisplayName("Generated code has error handling")
        void generatedCodeHasErrorHandling() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(client.clientCode, "{error,");
        }
        
        @Test
        @DisplayName("Generated code has success returns")
        void generatedCodeHasSuccessReturns() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(client.clientCode, "{ok,");
        }
        
        @Test
        @DisplayName("Multiple files are generated")
        void multipleFilesAreGenerated() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertTrue(client.generatedFiles.size() >= 2, 
                    "Should generate at least client and runtime modules. Generated: " + client.generatedFiles);
        }
    }
    
    @Nested
    @DisplayName("All Protocols Generate Valid Code")
    class AllProtocolsTest {
        
        @Test
        @DisplayName("REST-XML protocol generates valid code")
        void restXmlProtocolGeneratesValidCode() throws Exception {
            GeneratedClient client = generateClient("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            // Verify basic structure
            assertContains(client.clientCode, "-module(");
            assertContains(client.clientCode, "-export([");
            assertContains(client.clientCode, "-spec ");
            assertContains(client.clientCode, "aws_sigv4:sign_request");
        }
        
        @Test
        @DisplayName("AWS JSON protocol generates valid code")
        void awsJsonProtocolGeneratesValidCode() throws Exception {
            GeneratedClient client = generateClient("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            // Verify basic structure
            assertContains(client.clientCode, "-module(");
            assertContains(client.clientCode, "-export([");
            assertContains(client.clientCode, "-spec ");
            assertContains(client.clientCode, "aws_sigv4:sign_request");
        }
        
        @Test
        @DisplayName("AWS Query protocol generates valid code")
        void awsQueryProtocolGeneratesValidCode() throws Exception {
            GeneratedClient client = generateClient("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            // Verify basic structure
            assertContains(client.clientCode, "-module(");
            assertContains(client.clientCode, "-export([");
            assertContains(client.clientCode, "-spec ");
            assertContains(client.clientCode, "aws_sigv4:sign_request");
        }
        
        @Test
        @DisplayName("REST-JSON protocol generates valid code")
        void restJsonProtocolGeneratesValidCode() throws Exception {
            GeneratedClient client = generateClient("apigateway-minimal.smithy", "com.amazonaws.apigateway#APIGateway");
            
            // Verify basic structure
            assertContains(client.clientCode, "-module(");
            assertContains(client.clientCode, "-export([");
            assertContains(client.clientCode, "-spec ");
            assertContains(client.clientCode, "aws_sigv4:sign_request");
        }
        
        @Test
        @DisplayName("EC2 Query protocol generates valid code")
        void ec2QueryProtocolGeneratesValidCode() throws Exception {
            GeneratedClient client = generateClient("ec2-minimal.smithy", "com.amazonaws.ec2#EC2");
            
            // Verify basic structure
            assertContains(client.clientCode, "-module(");
            assertContains(client.clientCode, "-export([");
            assertContains(client.clientCode, "-spec ");
            assertContains(client.clientCode, "aws_sigv4:sign_request");
        }
    }
    
    // ========================================================================
    // Helper methods and classes
    // ========================================================================
    
    /**
     * Generates client code for a model and returns details.
     */
    private GeneratedClient generateClient(String modelFile, String serviceId) throws Exception {
        // Load model from classpath
        URL modelUrl = getClass().getResource("/models/" + modelFile);
        if (modelUrl == null) {
            throw new IOException("Model file not found: /models/" + modelFile);
        }
        
        Model model = Model.assembler()
                .discoverModels(getClass().getClassLoader())
                .addImport(modelUrl)
                .assemble()
                .unwrap();
        
        // Create plugin context
        FileManifest manifest = FileManifest.create(outputDir);
        
        String moduleName = deriveModuleName(serviceId);
        ObjectNode settings = ObjectNode.builder()
                .withMember("service", serviceId)
                .withMember("module", moduleName)
                .withMember("outputDir", "src/generated")
                .build();
        
        PluginContext context = PluginContext.builder()
                .model(model)
                .fileManifest(manifest)
                .settings(settings)
                .build();
        
        // Execute plugin
        plugin.execute(context);
        
        // Collect generated files
        List<String> generatedFiles;
        try (Stream<Path> paths = Files.walk(outputDir)) {
            generatedFiles = paths
                    .filter(Files::isRegularFile)
                    .map(p -> outputDir.relativize(p).toString())
                    .collect(Collectors.toList());
        }
        
        // Find and read the main client file
        String clientCode = findAndReadClientFile(outputDir, moduleName);
        
        return new GeneratedClient(clientCode, generatedFiles, outputDir);
    }
    
    /**
     * Finds and reads the main client .erl file.
     */
    private String findAndReadClientFile(Path dir, String moduleName) throws IOException {
        String[] possiblePaths = {
            "src/" + moduleName + ".erl",
            moduleName + ".erl",
            "src/generated/" + moduleName + ".erl",
            "src/" + moduleName + "_client.erl",
            moduleName + "_client.erl"
        };
        
        for (String path : possiblePaths) {
            Path file = dir.resolve(path);
            if (Files.exists(file)) {
                return Files.readString(file, StandardCharsets.UTF_8);
            }
        }
        
        // List all files for debugging
        List<String> allFiles;
        try (Stream<Path> paths = Files.walk(dir)) {
            allFiles = paths
                    .filter(Files::isRegularFile)
                    .map(p -> dir.relativize(p).toString())
                    .collect(Collectors.toList());
        }
        
        throw new IOException("Client file not found. Generated files: " + allFiles);
    }
    
    /**
     * Derives module name from service ID.
     */
    private String deriveModuleName(String serviceId) {
        String name = serviceId.substring(serviceId.lastIndexOf('#') + 1);
        return name.replaceAll("([a-z])([A-Z])", "$1_$2")
                   .replaceAll("([A-Z])([A-Z][a-z])", "$1_$2")
                   .toLowerCase();
    }
    
    /**
     * Asserts that actual contains expected substring.
     */
    private void assertContains(String actual, String expected) {
        assertTrue(actual.contains(expected),
                "Expected output to contain: " + expected + "\n\nFirst 500 chars:\n" + 
                actual.substring(0, Math.min(500, actual.length())));
    }
    
    /**
     * Container for generated client details.
     */
    private static class GeneratedClient {
        final String clientCode;
        final List<String> generatedFiles;
        final Path outputDir;
        
        GeneratedClient(String clientCode, List<String> generatedFiles, Path outputDir) {
            this.clientCode = clientCode;
            this.generatedFiles = generatedFiles;
            this.outputDir = outputDir;
        }
        
        boolean hasFile(String filename) {
            return generatedFiles.stream()
                    .anyMatch(f -> f.endsWith(filename));
        }
    }
}
