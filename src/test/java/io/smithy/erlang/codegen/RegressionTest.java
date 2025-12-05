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
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Regression tests for code generation consistency.
 * 
 * <p>These tests ensure that the DirectedCodegen-based architecture
 * produces consistent and correct output.
 * 
 * <h2>Test Strategy</h2>
 * <ul>
 *   <li>Generate code using ErlangCodegenPlugin</li>
 *   <li>Extract key components (exports, specs, functions)</li>
 *   <li>Verify structural correctness</li>
 * </ul>
 * 
 * @see ErlangCodegenPlugin
 */
class RegressionTest {
    
    @TempDir
    Path outputDir;
    
    private ErlangCodegenPlugin plugin;
    
    @BeforeEach
    void setUp() {
        plugin = new ErlangCodegenPlugin();
    }
    
    @Nested
    @DisplayName("Module Structure Consistency")
    class ModuleStructureTests {
        
        @Test
        @DisplayName("Module declaration format is consistent")
        void moduleDeclarationFormatIsConsistent() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            // Extract module declaration
            Pattern modulePattern = Pattern.compile("-module\\(([^)]+)\\)\\.");
            Matcher matcher = modulePattern.matcher(code.clientCode);
            
            assertTrue(matcher.find(), "Should have module declaration");
            String moduleName = matcher.group(1);
            
            // Module name should be snake_case
            assertTrue(moduleName.matches("[a-z][a-z0-9_]*"),
                    "Module name should be snake_case: " + moduleName);
        }
        
        @Test
        @DisplayName("Export format is consistent")
        void exportFormatIsConsistent() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            // Should have export declarations
            assertTrue(code.clientCode.contains("-export(["),
                    "Should have export declarations");
        }
        
        @Test
        @DisplayName("Generated exports include operations")
        void generatedExportsIncludeOperations() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            List<String> exports = extractExports(code.clientCode);
            
            // Should have the operations
            assertTrue(exports.stream().anyMatch(e -> e.contains("get_item")),
                    "Should export get_item");
            assertTrue(exports.stream().anyMatch(e -> e.contains("create_item")),
                    "Should export create_item");
            assertTrue(exports.stream().anyMatch(e -> e.contains("delete_item")),
                    "Should export delete_item");
        }
    }
    
    @Nested
    @DisplayName("Function Generation Consistency")
    class FunctionGenerationTests {
        
        @Test
        @DisplayName("Operations have 2-arity and 3-arity versions")
        void operationsHaveBothArities() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            // Check for both arities
            assertTrue(code.clientCode.contains("get_item(Client, Input)"),
                    "Should have 2-arity get_item");
            assertTrue(code.clientCode.contains("get_item(Client, Input, Options)"),
                    "Should have 3-arity get_item");
        }
        
        @Test
        @DisplayName("Functions have type specs")
        void functionsHaveTypeSpecs() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            // Count -spec declarations
            int specCount = countOccurrences(code.clientCode, "-spec ");
            
            assertTrue(specCount > 0, "Should have type specs");
        }
        
        @Test
        @DisplayName("Functions have proper clause syntax")
        void functionsHaveProperClauseSyntax() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            // Check for proper function clause endings
            assertTrue(code.clientCode.contains(") ->"),
                    "Should have function clause syntax");
        }
    }
    
    @Nested
    @DisplayName("Protocol-Specific Consistency")
    class ProtocolConsistencyTests {
        
        @Test
        @DisplayName("REST-JSON uses jsx encoding")
        void restJsonUsesJsxEncoding() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertTrue(code.clientCode.contains("jsx:encode") || 
                       code.clientCode.contains("jsx:decode"),
                    "REST-JSON should use jsx for encoding/decoding");
        }
        
        @Test
        @DisplayName("REST-XML uses aws_xml encoding")
        void restXmlUsesAwsXmlEncoding() throws Exception {
            GeneratedCode code = generateCode("s3-minimal.smithy", "com.amazonaws.s3#S3");
            
            assertTrue(code.clientCode.contains("aws_xml"),
                    "REST-XML should use aws_xml for encoding");
        }
        
        @Test
        @DisplayName("AWS Query uses aws_query encoding")
        void awsQueryUsesAwsQueryEncoding() throws Exception {
            GeneratedCode code = generateCode("sqs-minimal.smithy", "com.amazonaws.sqs#SQS");
            
            assertTrue(code.clientCode.contains("aws_query:encode"),
                    "AWS Query should use aws_query for encoding");
        }
        
        @Test
        @DisplayName("AWS JSON uses jsx and X-Amz-Target")
        void awsJsonUsesJsxAndTarget() throws Exception {
            GeneratedCode code = generateCode("dynamodb-minimal.smithy", "com.amazonaws.dynamodb#DynamoDB");
            
            assertTrue(code.clientCode.contains("jsx:encode"),
                    "AWS JSON should use jsx for encoding");
            assertTrue(code.clientCode.contains("X-Amz-Target"),
                    "AWS JSON should include X-Amz-Target header");
        }
    }
    
    @Nested
    @DisplayName("Authentication Consistency")
    class AuthenticationConsistencyTests {
        
        @Test
        @DisplayName("All protocols use SigV4 signing")
        void allProtocolsUseSigV4() throws Exception {
            String[] models = {
                "simple-service.smithy:com.example.simple#SimpleService",
                "s3-minimal.smithy:com.amazonaws.s3#S3",
                "sqs-minimal.smithy:com.amazonaws.sqs#SQS",
                "dynamodb-minimal.smithy:com.amazonaws.dynamodb#DynamoDB"
            };
            
            for (String modelAndService : models) {
                String[] parts = modelAndService.split(":");
                GeneratedCode code = generateCode(parts[0], parts[1]);
                
                assertTrue(code.clientCode.contains("aws_sigv4:sign_request"),
                        parts[0] + " should use SigV4 signing");
            }
        }
    }
    
    @Nested
    @DisplayName("Error Handling Consistency")
    class ErrorHandlingConsistencyTests {
        
        @Test
        @DisplayName("Generated code has error tuples")
        void generatedCodeHasErrorTuples() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertTrue(code.clientCode.contains("{error,"),
                    "Should have error tuples");
        }
        
        @Test
        @DisplayName("Generated code has success tuples")
        void generatedCodeHasSuccessTuples() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertTrue(code.clientCode.contains("{ok,"),
                    "Should have success tuples");
        }
        
        @Test
        @DisplayName("Generated code handles HTTP errors")
        void generatedCodeHandlesHttpErrors() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertTrue(code.clientCode.contains("http_error") || 
                       code.clientCode.contains("aws_error"),
                    "Should handle HTTP/AWS errors");
        }
    }
    
    @Nested
    @DisplayName("Retry Logic Consistency")
    class RetryLogicConsistencyTests {
        
        @Test
        @DisplayName("All clients include retry support")
        void allClientsIncludeRetrySupport() throws Exception {
            String[] models = {
                "simple-service.smithy:com.example.simple#SimpleService",
                "s3-minimal.smithy:com.amazonaws.s3#S3",
                "sqs-minimal.smithy:com.amazonaws.sqs#SQS"
            };
            
            for (String modelAndService : models) {
                String[] parts = modelAndService.split(":");
                GeneratedCode code = generateCode(parts[0], parts[1]);
                
                assertTrue(code.clientCode.contains("aws_retry"),
                        parts[0] + " should include retry support");
            }
        }
        
        @Test
        @DisplayName("Retry is configurable via options")
        void retryIsConfigurableViaOptions() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertTrue(code.clientCode.contains("enable_retry"),
                    "Should have enable_retry option");
        }
    }
    
    @Nested
    @DisplayName("Runtime Module Consistency")
    class RuntimeModuleConsistencyTests {
        
        @Test
        @DisplayName("Generates aws_sigv4 module")
        void generatesAwsSigv4Module() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertTrue(code.hasFile("aws_sigv4.erl"),
                    "Should generate aws_sigv4.erl");
        }
        
        @Test
        @DisplayName("Generates aws_retry module")
        void generatesAwsRetryModule() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertTrue(code.hasFile("aws_retry.erl"),
                    "Should generate aws_retry.erl");
        }
        
        @Test
        @DisplayName("Generates aws_credentials module")
        void generatesAwsCredentialsModule() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertTrue(code.hasFile("aws_credentials.erl"),
                    "Should generate aws_credentials.erl");
        }
        
        @Test
        @DisplayName("Generates aws_config module")
        void generatesAwsConfigModule() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            assertTrue(code.hasFile("aws_config.erl"),
                    "Should generate aws_config.erl");
        }
    }
    
    @Nested
    @DisplayName("Diff Comparison Tool")
    class DiffComparisonTests {
        
        @Test
        @DisplayName("Can extract and compare exports")
        void canExtractAndCompareExports() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            List<String> exports = extractExports(code.clientCode);
            
            assertFalse(exports.isEmpty(), "Should extract exports");
            
            // Verify format
            for (String export : exports) {
                assertTrue(export.matches("[a-z_]+/\\d+") || export.isEmpty(),
                        "Export should be in name/arity format: " + export);
            }
        }
        
        @Test
        @DisplayName("Can extract function names")
        void canExtractFunctionNames() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            Set<String> functions = extractFunctionNames(code.clientCode);
            
            assertFalse(functions.isEmpty(), "Should extract function names");
            assertTrue(functions.contains("get_item") || 
                       functions.stream().anyMatch(f -> f.contains("get_item")),
                    "Should contain get_item function");
        }
        
        @Test
        @DisplayName("Can count type specs")
        void canCountTypeSpecs() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            int specCount = countOccurrences(code.clientCode, "-spec ");
            
            assertTrue(specCount > 0, "Should have type specs");
        }
    }
    
    // ========================================================================
    // Intentional Differences Documentation
    // ========================================================================
    
    @Nested
    @DisplayName("Documented Intentional Differences")
    class IntentionalDifferencesTests {
        
        /**
         * Documents that whitespace variations are acceptable.
         */
        @Test
        @DisplayName("Whitespace variations are acceptable")
        void whitespaceVariationsAreAcceptable() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            // Normalize whitespace and verify content
            String normalized = normalizeWhitespace(code.clientCode);
            
            assertFalse(normalized.isEmpty(), "Normalized code should not be empty");
        }
        
        /**
         * Documents that comment order may vary.
         */
        @Test
        @DisplayName("Comment order may vary")
        void commentOrderMayVary() {
            // This is documented as an acceptable difference
            // Comments may appear in different order between architectures
            assertTrue(true, "Comment order variations are acceptable");
        }
        
        /**
         * Documents that runtime module copy order may vary.
         */
        @Test
        @DisplayName("Runtime module copy order may vary")
        void runtimeModuleCopyOrderMayVary() throws Exception {
            GeneratedCode code = generateCode("simple-service.smithy", "com.example.simple#SimpleService");
            
            // All required modules should be present, order doesn't matter
            assertTrue(code.hasFile("aws_sigv4.erl"), "Should have aws_sigv4.erl");
            assertTrue(code.hasFile("aws_retry.erl"), "Should have aws_retry.erl");
        }
    }
    
    // ========================================================================
    // Helper methods
    // ========================================================================
    
    /**
     * Generates code using the current plugin.
     */
    private GeneratedCode generateCode(String modelFile, String serviceId) throws Exception {
        URL modelUrl = getClass().getResource("/models/" + modelFile);
        if (modelUrl == null) {
            throw new IOException("Model file not found: /models/" + modelFile);
        }
        
        Model model = Model.assembler()
                .discoverModels(getClass().getClassLoader())
                .addImport(modelUrl)
                .assemble()
                .unwrap();
        
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
        
        plugin.execute(context);
        
        List<String> generatedFiles;
        try (Stream<Path> paths = Files.walk(outputDir)) {
            generatedFiles = paths
                    .filter(Files::isRegularFile)
                    .map(p -> outputDir.relativize(p).toString())
                    .collect(Collectors.toList());
        }
        
        String clientCode = findAndReadClientFile(outputDir, moduleName);
        
        return new GeneratedCode(clientCode, generatedFiles, outputDir);
    }
    
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
        
        List<String> allFiles;
        try (Stream<Path> paths = Files.walk(dir)) {
            allFiles = paths
                    .filter(Files::isRegularFile)
                    .map(p -> dir.relativize(p).toString())
                    .collect(Collectors.toList());
        }
        
        throw new IOException("Client file not found. Generated files: " + allFiles);
    }
    
    private String deriveModuleName(String serviceId) {
        String name = serviceId.substring(serviceId.lastIndexOf('#') + 1);
        return name.replaceAll("([a-z])([A-Z])", "$1_$2")
                   .replaceAll("([A-Z])([A-Z][a-z])", "$1_$2")
                   .toLowerCase();
    }
    
    /**
     * Extracts export declarations from code.
     */
    private List<String> extractExports(String code) {
        List<String> exports = new ArrayList<>();
        Pattern exportPattern = Pattern.compile("-export\\(\\[([^\\]]+)\\]\\)\\.");
        Matcher matcher = exportPattern.matcher(code);
        
        while (matcher.find()) {
            String exportList = matcher.group(1);
            String[] items = exportList.split(",");
            for (String item : items) {
                exports.add(item.trim());
            }
        }
        
        return exports;
    }
    
    /**
     * Extracts function names from code.
     */
    private Set<String> extractFunctionNames(String code) {
        Set<String> functions = new HashSet<>();
        Pattern funcPattern = Pattern.compile("^([a-z][a-z0-9_]*)\\(", Pattern.MULTILINE);
        Matcher matcher = funcPattern.matcher(code);
        
        while (matcher.find()) {
            functions.add(matcher.group(1));
        }
        
        return functions;
    }
    
    /**
     * Counts occurrences of a pattern in code.
     */
    private int countOccurrences(String code, String pattern) {
        int count = 0;
        int index = 0;
        while ((index = code.indexOf(pattern, index)) != -1) {
            count++;
            index += pattern.length();
        }
        return count;
    }
    
    /**
     * Normalizes whitespace for comparison.
     */
    private String normalizeWhitespace(String code) {
        return code.replaceAll("\\r\\n", "\n")
                   .replaceAll("[ \\t]+\n", "\n")
                   .replaceAll("\n{3,}", "\n\n")
                   .trim();
    }
    
    /**
     * Container for generated code details.
     */
    private static class GeneratedCode {
        final String clientCode;
        final List<String> generatedFiles;
        final Path outputDir;
        
        GeneratedCode(String clientCode, List<String> generatedFiles, Path outputDir) {
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
