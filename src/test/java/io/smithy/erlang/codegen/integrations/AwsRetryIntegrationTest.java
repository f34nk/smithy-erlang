package io.smithy.erlang.codegen.integrations;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangSettings;
import io.smithy.erlang.codegen.ErlangWriter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.codegen.core.WriterDelegator;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for AwsRetryIntegration.
 */
class AwsRetryIntegrationTest {
    
    private AwsRetryIntegration integration;
    
    @BeforeEach
    void setUp() {
        integration = new AwsRetryIntegration();
    }
    
    @Nested
    @DisplayName("name()")
    class NameTests {
        
        @Test
        @DisplayName("Returns 'AwsRetryIntegration'")
        void returnsCorrectName() {
            assertEquals("AwsRetryIntegration", integration.name());
        }
    }
    
    @Nested
    @DisplayName("priority()")
    class PriorityTests {
        
        @Test
        @DisplayName("Returns 16 (medium priority)")
        void returnsMediumPriority() {
            assertEquals(16, integration.priority());
        }
        
        @Test
        @DisplayName("Priority is lower than AwsSigV4Integration")
        void priorityIsLowerThanSigV4() {
            AwsSigV4Integration sigv4 = new AwsSigV4Integration();
            assertTrue(integration.priority() < sigv4.priority(),
                    "Retry integration should run after SigV4 integration");
        }
        
        @Test
        @DisplayName("Priority is lower than AwsProtocolIntegration")
        void priorityIsLowerThanProtocol() {
            AwsProtocolIntegration protocol = new AwsProtocolIntegration();
            assertTrue(integration.priority() < protocol.priority(),
                    "Retry integration should run after Protocol integration");
        }
    }
    
    @Nested
    @DisplayName("preprocessModel()")
    class PreprocessModelTests {
        
        @Test
        @DisplayName("Copies aws_retry.erl for any service")
        void copiesRetryModuleForAnyService() throws IOException {
            Path tempDir = Files.createTempDirectory("retry-test");
            
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/s3-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ErlangContext context = createTestContext(model, tempDir, "com.amazonaws.s3#S3");
            
            integration.preprocessModel(context);
            
            assertTrue(Files.exists(tempDir.resolve("aws_retry.erl")),
                    "aws_retry.erl should be copied for any service");
            
            // Cleanup
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Copies aws_retry.erl for REST-JSON service")
        void copiesRetryModuleForRestJsonService() throws IOException {
            Path tempDir = Files.createTempDirectory("retry-test");
            
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/apigateway-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ErlangContext context = createTestContext(model, tempDir, "com.amazonaws.apigateway#APIGateway");
            
            integration.preprocessModel(context);
            
            assertTrue(Files.exists(tempDir.resolve("aws_retry.erl")),
                    "aws_retry.erl should be copied for REST-JSON service");
            
            // Cleanup
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Copies aws_retry.erl for AWS Query service")
        void copiesRetryModuleForAwsQueryService() throws IOException {
            Path tempDir = Files.createTempDirectory("retry-test");
            
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/sqs-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ErlangContext context = createTestContext(model, tempDir, "com.amazonaws.sqs#SQS");
            
            integration.preprocessModel(context);
            
            assertTrue(Files.exists(tempDir.resolve("aws_retry.erl")),
                    "aws_retry.erl should be copied for AWS Query service");
            
            // Cleanup
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Copies aws_retry.erl for DynamoDB service")
        void copiesRetryModuleForDynamoDbService() throws IOException {
            Path tempDir = Files.createTempDirectory("retry-test");
            
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/dynamodb-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ErlangContext context = createTestContext(model, tempDir, "com.amazonaws.dynamodb#DynamoDB");
            
            integration.preprocessModel(context);
            
            assertTrue(Files.exists(tempDir.resolve("aws_retry.erl")),
                    "aws_retry.erl should be copied for DynamoDB service");
            
            // Cleanup
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Handles null service shape gracefully")
        void handlesNullServiceGracefully() throws IOException {
            Path tempDir = Files.createTempDirectory("retry-test");
            
            Model model = Model.builder().build();
            
            ErlangContext context = createContextWithoutService(model, tempDir);
            
            // Should not throw
            assertDoesNotThrow(() -> integration.preprocessModel(context));
            
            // Module should NOT be copied when there's no service
            assertFalse(Files.exists(tempDir.resolve("aws_retry.erl")),
                    "aws_retry.erl should not be copied when there's no service");
            
            // Cleanup
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("aws_retry.erl contains expected content")
        void retryModuleContainsExpectedContent() throws IOException {
            Path tempDir = Files.createTempDirectory("retry-test");
            
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/s3-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ErlangContext context = createTestContext(model, tempDir, "com.amazonaws.s3#S3");
            
            integration.preprocessModel(context);
            
            Path retryModule = tempDir.resolve("aws_retry.erl");
            assertTrue(Files.exists(retryModule));
            
            String content = Files.readString(retryModule);
            assertTrue(content.contains("-module(aws_retry)"),
                    "Should contain module declaration");
            assertTrue(content.contains("with_retry"),
                    "Should contain with_retry function");
            assertTrue(content.contains("is_retryable_error"),
                    "Should contain is_retryable_error function");
            assertTrue(content.contains("exponential"),
                    "Should mention exponential backoff");
            
            // Cleanup
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("Integration Priority Chain")
    class IntegrationPriorityChainTests {
        
        @Test
        @DisplayName("Integrations run in correct order")
        void integrationsRunInCorrectOrder() {
            AwsSigV4Integration sigv4 = new AwsSigV4Integration();
            AwsProtocolIntegration protocol = new AwsProtocolIntegration();
            AwsRetryIntegration retry = new AwsRetryIntegration();
            
            // Priority order: SigV4 (64) > Protocol (32) > Retry (16)
            assertTrue(sigv4.priority() > protocol.priority());
            assertTrue(protocol.priority() > retry.priority());
            assertTrue(retry.priority() > 0);
        }
    }
    
    /**
     * Creates a test context with the given model, output directory, and service ID.
     */
    private ErlangContext createTestContext(Model model, Path outputDir, String serviceId) {
        ShapeId shapeId = ShapeId.from(serviceId);
        
        ErlangSettings settings = ErlangSettings.builder()
                .service(shapeId)
                .moduleName("test")
                .outputDir(outputDir.toString())
                .build();
        
        FileManifest fileManifest = FileManifest.create(outputDir);
        
        SymbolProvider symbolProvider = shape -> null;
        
        WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                fileManifest,
                symbolProvider,
                (filename, namespace) -> new ErlangWriter("test")
        );
        
        ServiceShape service = model.expectShape(shapeId, ServiceShape.class);
        
        return ErlangContext.builder()
                .model(model)
                .settings(settings)
                .symbolProvider(symbolProvider)
                .fileManifest(fileManifest)
                .writerDelegator(writerDelegator)
                .integrations(List.of())
                .service(service)
                .build();
    }
    
    /**
     * Creates a test context without a service shape.
     */
    private ErlangContext createContextWithoutService(Model model, Path outputDir) {
        ErlangSettings settings = ErlangSettings.builder()
                .service(ShapeId.from("com.example#TestService"))
                .moduleName("test")
                .outputDir(outputDir.toString())
                .build();
        
        FileManifest fileManifest = FileManifest.create(outputDir);
        
        SymbolProvider symbolProvider = shape -> null;
        
        WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                fileManifest,
                symbolProvider,
                (filename, namespace) -> new ErlangWriter("test")
        );
        
        return ErlangContext.builder()
                .model(model)
                .settings(settings)
                .symbolProvider(symbolProvider)
                .fileManifest(fileManifest)
                .writerDelegator(writerDelegator)
                .integrations(List.of())
                .service(null)
                .build();
    }
    
    /**
     * Recursively deletes a directory.
     */
    private void cleanupDir(Path dir) {
        try {
            Files.walk(dir)
                    .sorted((a, b) -> b.compareTo(a))
                    .forEach(path -> {
                        try { Files.deleteIfExists(path); } catch (IOException ignored) {}
                    });
        } catch (IOException ignored) {}
    }
}
