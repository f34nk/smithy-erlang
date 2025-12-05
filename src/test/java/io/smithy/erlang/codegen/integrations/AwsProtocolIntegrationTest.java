package io.smithy.erlang.codegen.integrations;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangSettings;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.aws.AwsProtocol;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.codegen.core.WriterDelegator;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for AwsProtocolIntegration.
 */
class AwsProtocolIntegrationTest {
    
    private AwsProtocolIntegration integration;
    
    @BeforeEach
    void setUp() {
        integration = new AwsProtocolIntegration();
    }
    
    @Nested
    @DisplayName("name()")
    class NameTests {
        
        @Test
        @DisplayName("Returns 'AwsProtocolIntegration'")
        void returnsCorrectName() {
            assertEquals("AwsProtocolIntegration", integration.name());
        }
    }
    
    @Nested
    @DisplayName("priority()")
    class PriorityTests {
        
        @Test
        @DisplayName("Returns 32 (medium-high priority)")
        void returnsMediumHighPriority() {
            assertEquals(32, integration.priority());
        }
        
        @Test
        @DisplayName("Priority is lower than AwsSigV4Integration")
        void priorityIsLowerThanSigV4() {
            AwsSigV4Integration sigv4 = new AwsSigV4Integration();
            assertTrue(integration.priority() < sigv4.priority(),
                    "Protocol integration should run after SigV4 integration");
        }
    }
    
    @Nested
    @DisplayName("getProtocol()")
    class GetProtocolTests {
        
        @Test
        @DisplayName("Detects REST-XML protocol")
        void detectsRestXmlProtocol() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/s3-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.s3#S3"),
                    ServiceShape.class);
            
            AwsProtocol protocol = integration.getProtocol(service);
            assertEquals(AwsProtocol.REST_XML, protocol);
        }
        
        @Test
        @DisplayName("Detects AWS Query protocol")
        void detectsAwsQueryProtocol() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/sqs-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.sqs#SQS"),
                    ServiceShape.class);
            
            AwsProtocol protocol = integration.getProtocol(service);
            assertEquals(AwsProtocol.AWS_QUERY, protocol);
        }
        
        @Test
        @DisplayName("Detects EC2 Query protocol")
        void detectsEc2QueryProtocol() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/ec2-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.ec2#EC2"),
                    ServiceShape.class);
            
            AwsProtocol protocol = integration.getProtocol(service);
            assertEquals(AwsProtocol.EC2_QUERY, protocol);
        }
        
        @Test
        @DisplayName("Detects AWS JSON 1.0 protocol")
        void detectsAwsJson10Protocol() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/dynamodb-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.dynamodb#DynamoDB"),
                    ServiceShape.class);
            
            AwsProtocol protocol = integration.getProtocol(service);
            assertEquals(AwsProtocol.AWS_JSON_1_0, protocol);
        }
        
        @Test
        @DisplayName("Detects REST-JSON protocol")
        void detectsRestJsonProtocol() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/apigateway-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.apigateway#APIGateway"),
                    ServiceShape.class);
            
            AwsProtocol protocol = integration.getProtocol(service);
            assertEquals(AwsProtocol.REST_JSON_1, protocol);
        }
    }
    
    @Nested
    @DisplayName("usesXmlProtocol()")
    class UsesXmlProtocolTests {
        
        @Test
        @DisplayName("Returns true for REST-XML service")
        void returnsTrueForRestXml() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/s3-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.s3#S3"),
                    ServiceShape.class);
            
            assertTrue(integration.usesXmlProtocol(service));
        }
        
        @Test
        @DisplayName("Returns true for AWS Query service")
        void returnsTrueForAwsQuery() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/sqs-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.sqs#SQS"),
                    ServiceShape.class);
            
            assertTrue(integration.usesXmlProtocol(service));
        }
        
        @Test
        @DisplayName("Returns false for REST-JSON service")
        void returnsFalseForRestJson() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/apigateway-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.apigateway#APIGateway"),
                    ServiceShape.class);
            
            assertFalse(integration.usesXmlProtocol(service));
        }
    }
    
    @Nested
    @DisplayName("usesQueryProtocol()")
    class UsesQueryProtocolTests {
        
        @Test
        @DisplayName("Returns true for AWS Query service")
        void returnsTrueForAwsQuery() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/sqs-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.sqs#SQS"),
                    ServiceShape.class);
            
            assertTrue(integration.usesQueryProtocol(service));
        }
        
        @Test
        @DisplayName("Returns true for EC2 Query service")
        void returnsTrueForEc2Query() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/ec2-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.ec2#EC2"),
                    ServiceShape.class);
            
            assertTrue(integration.usesQueryProtocol(service));
        }
        
        @Test
        @DisplayName("Returns false for REST-XML service")
        void returnsFalseForRestXml() {
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/s3-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ServiceShape service = model.expectShape(
                    ShapeId.from("com.amazonaws.s3#S3"),
                    ServiceShape.class);
            
            assertFalse(integration.usesQueryProtocol(service));
        }
    }
    
    @Nested
    @DisplayName("preprocessModel()")
    class PreprocessModelTests {
        
        @Test
        @DisplayName("Copies aws_xml.erl for REST-XML service")
        void copiesXmlModuleForRestXml() throws IOException {
            Path tempDir = Files.createTempDirectory("protocol-test");
            
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/s3-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ErlangContext context = createTestContext(model, tempDir, "com.amazonaws.s3#S3");
            
            integration.preprocessModel(context);
            
            assertTrue(Files.exists(tempDir.resolve("aws_xml.erl")),
                    "aws_xml.erl should be copied for REST-XML protocol");
            
            // Cleanup
            Files.walk(tempDir)
                    .sorted((a, b) -> b.compareTo(a))
                    .forEach(path -> {
                        try { Files.deleteIfExists(path); } catch (IOException ignored) {}
                    });
        }
        
        @Test
        @DisplayName("Copies aws_s3.erl for REST-XML service")
        void copiesS3ModuleForRestXml() throws IOException {
            Path tempDir = Files.createTempDirectory("protocol-test");
            
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/s3-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ErlangContext context = createTestContext(model, tempDir, "com.amazonaws.s3#S3");
            
            integration.preprocessModel(context);
            
            assertTrue(Files.exists(tempDir.resolve("aws_s3.erl")),
                    "aws_s3.erl should be copied for REST-XML protocol");
            
            // Cleanup
            Files.walk(tempDir)
                    .sorted((a, b) -> b.compareTo(a))
                    .forEach(path -> {
                        try { Files.deleteIfExists(path); } catch (IOException ignored) {}
                    });
        }
        
        @Test
        @DisplayName("Copies aws_query.erl for AWS Query service")
        void copiesQueryModuleForAwsQuery() throws IOException {
            Path tempDir = Files.createTempDirectory("protocol-test");
            
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/sqs-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ErlangContext context = createTestContext(model, tempDir, "com.amazonaws.sqs#SQS");
            
            integration.preprocessModel(context);
            
            assertTrue(Files.exists(tempDir.resolve("aws_query.erl")),
                    "aws_query.erl should be copied for AWS Query protocol");
            
            // Cleanup
            Files.walk(tempDir)
                    .sorted((a, b) -> b.compareTo(a))
                    .forEach(path -> {
                        try { Files.deleteIfExists(path); } catch (IOException ignored) {}
                    });
        }
        
        @Test
        @DisplayName("Copies both xml and query modules for EC2 Query service")
        void copiesBothModulesForEc2Query() throws IOException {
            Path tempDir = Files.createTempDirectory("protocol-test");
            
            Model model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())
                    .addImport(getClass().getResource("/models/ec2-minimal.smithy"))
                    .assemble()
                    .unwrap();
            
            ErlangContext context = createTestContext(model, tempDir, "com.amazonaws.ec2#EC2");
            
            integration.preprocessModel(context);
            
            assertTrue(Files.exists(tempDir.resolve("aws_query.erl")),
                    "aws_query.erl should be copied for EC2 Query protocol");
            assertTrue(Files.exists(tempDir.resolve("aws_xml.erl")),
                    "aws_xml.erl should be copied for EC2 Query protocol");
            
            // Cleanup
            Files.walk(tempDir)
                    .sorted((a, b) -> b.compareTo(a))
                    .forEach(path -> {
                        try { Files.deleteIfExists(path); } catch (IOException ignored) {}
                    });
        }
        
        @Test
        @DisplayName("Handles null service shape gracefully")
        void handlesNullServiceGracefully() throws IOException {
            Path tempDir = Files.createTempDirectory("protocol-test");
            
            // Create a model without a service
            Model model = Model.builder().build();
            
            ErlangContext context = createContextWithoutService(model, tempDir);
            
            // Should not throw
            assertDoesNotThrow(() -> integration.preprocessModel(context));
            
            // Cleanup
            Files.walk(tempDir)
                    .sorted((a, b) -> b.compareTo(a))
                    .forEach(path -> {
                        try { Files.deleteIfExists(path); } catch (IOException ignored) {}
                    });
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
        
        // Get the service shape from the model
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
                .service(null)  // No service
                .build();
    }
}
