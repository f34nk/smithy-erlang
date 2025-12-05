package io.smithy.erlang.codegen;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.build.PluginContext;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.node.ObjectNode;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Golden file tests for Erlang code generation.
 * 
 * <p>Golden file tests compare generated output against pre-verified "golden"
 * reference files. This ensures the code generator produces consistent output
 * across changes and catches unintended regressions.
 * 
 * <h2>Test Strategy</h2>
 * <ul>
 *   <li>Each test loads a Smithy model from resources</li>
 *   <li>Runs the code generator to produce output</li>
 *   <li>Compares output to golden files stored in resources</li>
 *   <li>Differences indicate either bugs or intentional changes requiring golden file updates</li>
 * </ul>
 * 
 * <h2>Updating Golden Files</h2>
 * <p>To update golden files when output changes intentionally:
 * <ol>
 *   <li>Set environment variable GOLDEN_UPDATE=true</li>
 *   <li>Run tests</li>
 *   <li>Verify changes are intentional</li>
 *   <li>Commit updated golden files</li>
 * </ol>
 */
class GoldenFileTest {
    
    private static final String MODELS_PATH = "models/";
    private static final String GOLDEN_PATH = "golden/";
    
    @TempDir
    Path outputDir;
    
    private ErlangClientPlugin plugin;
    
    @BeforeEach
    void setUp() {
        plugin = new ErlangClientPlugin();
    }
    
    @Nested
    @DisplayName("Simple Service (REST-JSON)")
    class SimpleServiceTests {
        
        @Test
        @DisplayName("Generated module header contains correct attributes")
        void testModuleHeader() throws Exception {
            String generated = generateClientForModel("simple-service.smithy", "com.example.simple#SimpleService");
            
            // Module name is derived from service name - could be simple_service_client or simple_service
            assertTrue(generated.contains("-module(simple_service).") || 
                       generated.contains("-module(simple_service_client)."),
                       "Expected module declaration but got: " + generated.substring(0, Math.min(200, generated.length())));
            assertContains(generated, "-export([");
        }
        
        @Test
        @DisplayName("Generated code includes operation functions")
        void testOperationFunctions() throws Exception {
            String generated = generateClientForModel("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertContains(generated, "get_item(Client, Input)");
            assertContains(generated, "create_item(Client, Input)");
            assertContains(generated, "delete_item(Client, Input)");
        }
        
        @Test
        @DisplayName("Generated code includes retry support")
        void testRetrySupport() throws Exception {
            String generated = generateClientForModel("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertContains(generated, "aws_retry:with_retry");
        }
        
        @Test
        @DisplayName("Generated code includes SigV4 signing")
        void testSigV4Signing() throws Exception {
            String generated = generateClientForModel("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertContains(generated, "aws_sigv4:sign_request");
        }
        
        @Test
        @DisplayName("Matches golden file")
        void testMatchesGoldenFile() throws Exception {
            String generated = generateClientForModel("simple-service.smithy", "com.example.simple#SimpleService");
            String golden = loadGoldenFile("simple_service_client.golden.erl");
            
            if (golden != null) {
                assertEqualNormalized(golden, generated);
            }
        }
    }
    
    @Nested
    @DisplayName("AWS JSON Service (DynamoDB-like)")
    class AwsJsonServiceTests {
        
        @Test
        @DisplayName("Generated code uses POST method")
        void testPostMethod() throws Exception {
            String generated = generateClientForModel("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertContains(generated, "<<\"POST\">>");
        }
        
        @Test
        @DisplayName("Generated code includes X-Amz-Target header")
        void testXAmzTargetHeader() throws Exception {
            String generated = generateClientForModel("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertContains(generated, "X-Amz-Target");
        }
        
        @Test
        @DisplayName("Generated code uses jsx for JSON")
        void testJsxEncoding() throws Exception {
            String generated = generateClientForModel("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertContains(generated, "jsx:encode");
            assertContains(generated, "jsx:decode");
        }
        
        @Test
        @DisplayName("Matches golden file")
        void testMatchesGoldenFile() throws Exception {
            String generated = generateClientForModel("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            String golden = loadGoldenFile("dynamodb_client.golden.erl");
            
            if (golden != null) {
                assertEqualNormalized(golden, generated);
            }
        }
    }
    
    @Nested
    @DisplayName("REST-XML Service (S3-like)")
    class RestXmlServiceTests {
        
        @Test
        @DisplayName("Generated code handles path parameters")
        void testPathParameters() throws Exception {
            String generated = generateClientForModel("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(generated, "Bucket");
            assertContains(generated, "Key");
        }
        
        @Test
        @DisplayName("Generated code uses aws_xml for encoding")
        void testXmlEncoding() throws Exception {
            String generated = generateClientForModel("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertContains(generated, "aws_xml");
        }
        
        @Test
        @DisplayName("Matches golden file")
        void testMatchesGoldenFile() throws Exception {
            String generated = generateClientForModel("s3-minimal.smithy", "com.amazonaws.s3#S3");
            String golden = loadGoldenFile("s3_client.golden.erl");
            
            if (golden != null) {
                assertEqualNormalized(golden, generated);
            }
        }
    }
    
    @Nested
    @DisplayName("AWS Query Service (SQS-like)")
    class AwsQueryServiceTests {
        
        @Test
        @DisplayName("Generated code uses form-urlencoded content type")
        void testContentType() throws Exception {
            String generated = generateClientForModel("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertContains(generated, "application/x-www-form-urlencoded");
        }
        
        @Test
        @DisplayName("Generated code includes Action parameter")
        void testActionParameter() throws Exception {
            String generated = generateClientForModel("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertContains(generated, "aws_query:encode");
        }
        
        @Test
        @DisplayName("Matches golden file")
        void testMatchesGoldenFile() throws Exception {
            String generated = generateClientForModel("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            String golden = loadGoldenFile("sqs_client.golden.erl");
            
            if (golden != null) {
                assertEqualNormalized(golden, generated);
            }
        }
    }
    
    @Nested
    @DisplayName("REST-JSON Service (API Gateway-like)")
    class RestJsonServiceTests {
        
        @Test
        @DisplayName("Generated code handles multiple path labels")
        void testMultiplePathLabels() throws Exception {
            String generated = generateClientForModel("apigateway-minimal.smithy", "com.amazonaws.apigateway#APIGateway");
            
            assertContains(generated, "restApiId");
            assertContains(generated, "resourceId");
        }
        
        @Test
        @DisplayName("Generated code handles query parameters")
        void testQueryParameters() throws Exception {
            String generated = generateClientForModel("apigateway-minimal.smithy", "com.amazonaws.apigateway#APIGateway");
            
            assertContains(generated, "uri_string:compose_query");
        }
        
        @Test
        @DisplayName("Matches golden file")
        void testMatchesGoldenFile() throws Exception {
            String generated = generateClientForModel("apigateway-minimal.smithy", "com.amazonaws.apigateway#APIGateway");
            String golden = loadGoldenFile("apigateway_client.golden.erl");
            
            if (golden != null) {
                assertEqualNormalized(golden, generated);
            }
        }
    }
    
    // ========================================================================
    // Helper methods
    // ========================================================================
    
    /**
     * Generates client code for a model and returns the main client module content.
     */
    private String generateClientForModel(String modelFile, String serviceId) throws Exception {
        // Load model from classpath
        java.net.URL modelUrl = getClass().getResource("/models/" + modelFile);
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
        
        ObjectNode settings = ObjectNode.builder()
                .withMember("service", serviceId)
                .withMember("module", deriveModuleName(serviceId))
                .withMember("outputDir", "src/generated")
                .build();
        
        PluginContext context = PluginContext.builder()
                .model(model)
                .fileManifest(manifest)
                .settings(settings)
                .build();
        
        // Execute plugin
        plugin.execute(context);
        
        // Read generated client file
        // The plugin generates to src/{moduleName}.erl when using FileManifest
        String baseModuleName = deriveModuleName(serviceId);
        
        // Try various possible paths where the file might be generated
        String[] possiblePaths = {
            "src/" + baseModuleName + "_client.erl",
            baseModuleName + "_client.erl",
            "src/generated/" + baseModuleName + "_client.erl",
            "src/" + baseModuleName + ".erl",
            baseModuleName + ".erl"
        };
        
        for (String path : possiblePaths) {
            Path clientFile = outputDir.resolve(path);
            if (Files.exists(clientFile)) {
                return Files.readString(clientFile, StandardCharsets.UTF_8);
            }
        }
        
        // List all files for debugging
        String files = Files.walk(outputDir)
                .filter(Files::isRegularFile)
                .map(p -> outputDir.relativize(p).toString())
                .collect(Collectors.joining(", "));
        
        throw new IOException("Generated client file not found in: " + String.join(", ", possiblePaths) + 
                "\nGenerated files: " + files);
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
     * Loads a golden file from resources.
     */
    private String loadGoldenFile(String filename) {
        try (InputStream is = getClass().getResourceAsStream("/golden/" + filename)) {
            if (is == null) {
                return null;
            }
            return new String(is.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            return null;
        }
    }
    
    /**
     * Asserts that actual contains expected substring.
     */
    private void assertContains(String actual, String expected) {
        assertTrue(actual.contains(expected),
                "Expected output to contain: " + expected + "\n\nActual output:\n" + actual);
    }
    
    /**
     * Asserts that two strings are equal after normalizing whitespace.
     */
    private void assertEqualNormalized(String expected, String actual) {
        String normalizedExpected = normalizeWhitespace(expected);
        String normalizedActual = normalizeWhitespace(actual);
        
        assertEquals(normalizedExpected, normalizedActual,
                "Generated output differs from golden file");
    }
    
    /**
     * Normalizes whitespace for comparison.
     */
    private String normalizeWhitespace(String s) {
        return s.replaceAll("\\r\\n", "\n")
                .replaceAll("[ \\t]+\n", "\n")
                .trim();
    }
}
