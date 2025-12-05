package io.smithy.erlang.codegen.protocols;

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
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for the ProtocolGenerator interface.
 */
class ProtocolGeneratorTest {
    
    /**
     * Test implementation of ProtocolGenerator for unit testing.
     */
    static class TestProtocolGenerator implements ProtocolGenerator {
        
        private static final ShapeId PROTOCOL_ID = ShapeId.from("test.protocols#testProtocol");
        private boolean generateOperationCalled = false;
        private boolean generateRequestSerializerCalled = false;
        private boolean generateResponseDeserializerCalled = false;
        private String contentType = "application/test";
        
        @Override
        public ShapeId getProtocol() {
            return PROTOCOL_ID;
        }
        
        @Override
        public void generateOperation(OperationShape operation, ErlangWriter writer, ErlangContext context) {
            generateOperationCalled = true;
            writer.write("%% Generated operation: $L", operation.getId().getName());
        }
        
        @Override
        public void generateRequestSerializer(OperationShape operation, ErlangWriter writer, ErlangContext context) {
            generateRequestSerializerCalled = true;
            writer.write("%% Request serializer for: $L", operation.getId().getName());
        }
        
        @Override
        public void generateResponseDeserializer(OperationShape operation, ErlangWriter writer, ErlangContext context) {
            generateResponseDeserializerCalled = true;
            writer.write("%% Response deserializer for: $L", operation.getId().getName());
        }
        
        @Override
        public String getContentType(ServiceShape service) {
            return contentType;
        }
        
        public boolean wasGenerateOperationCalled() {
            return generateOperationCalled;
        }
        
        public boolean wasGenerateRequestSerializerCalled() {
            return generateRequestSerializerCalled;
        }
        
        public boolean wasGenerateResponseDeserializerCalled() {
            return generateResponseDeserializerCalled;
        }
        
        public void setContentType(String contentType) {
            this.contentType = contentType;
        }
    }
    
    private TestProtocolGenerator generator;
    private ErlangWriter writer;
    
    @BeforeEach
    void setUp() {
        generator = new TestProtocolGenerator();
        writer = new ErlangWriter("test_module");
    }
    
    @Nested
    @DisplayName("getProtocol()")
    class GetProtocolTests {
        
        @Test
        @DisplayName("Returns the correct protocol ShapeId")
        void returnsCorrectProtocolId() {
            ShapeId protocol = generator.getProtocol();
            
            assertNotNull(protocol);
            assertEquals("test.protocols", protocol.getNamespace());
            assertEquals("testProtocol", protocol.getName());
        }
    }
    
    @Nested
    @DisplayName("getName()")
    class GetNameTests {
        
        @Test
        @DisplayName("Default getName() returns protocol name")
        void defaultNameReturnsProtocolName() {
            assertEquals("testProtocol", generator.getName());
        }
    }
    
    @Nested
    @DisplayName("getDefaultMethod()")
    class GetDefaultMethodTests {
        
        @Test
        @DisplayName("Default method is POST")
        void defaultMethodIsPost() {
            assertEquals("POST", generator.getDefaultMethod());
        }
    }
    
    @Nested
    @DisplayName("getDefaultUri()")
    class GetDefaultUriTests {
        
        @Test
        @DisplayName("Default URI is /")
        void defaultUriIsRoot() {
            assertEquals("/", generator.getDefaultUri());
        }
    }
    
    @Nested
    @DisplayName("getContentType()")
    class GetContentTypeTests {
        
        @Test
        @DisplayName("Returns configured content type")
        void returnsConfiguredContentType() {
            generator.setContentType("application/json");
            
            ServiceShape service = ServiceShape.builder()
                    .id("test.service#TestService")
                    .version("1.0")
                    .build();
            
            assertEquals("application/json", generator.getContentType(service));
        }
    }
    
    @Nested
    @DisplayName("appliesTo()")
    class AppliesToTests {
        
        @Test
        @DisplayName("Returns false for service without matching protocol trait")
        void returnsFalseForServiceWithoutTrait() {
            ServiceShape service = ServiceShape.builder()
                    .id("test.service#TestService")
                    .version("1.0")
                    .build();
            
            assertFalse(generator.appliesTo(service));
        }
    }
    
    @Nested
    @DisplayName("generateOperation()")
    class GenerateOperationTests {
        
        @Test
        @DisplayName("Generates operation code")
        void generatesOperationCode() throws IOException {
            Path tempDir = Files.createTempDirectory("protocol-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("test.service#TestOperation")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateOperation(operation, writer, context);
            
            assertTrue(generator.wasGenerateOperationCalled());
            String output = writer.toString();
            assertTrue(output.contains("Generated operation: TestOperation"));
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateRequestSerializer()")
    class GenerateRequestSerializerTests {
        
        @Test
        @DisplayName("Generates request serializer code")
        void generatesRequestSerializerCode() throws IOException {
            Path tempDir = Files.createTempDirectory("protocol-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("test.service#TestOperation")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateRequestSerializer(operation, writer, context);
            
            assertTrue(generator.wasGenerateRequestSerializerCalled());
            String output = writer.toString();
            assertTrue(output.contains("Request serializer for: TestOperation"));
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateResponseDeserializer()")
    class GenerateResponseDeserializerTests {
        
        @Test
        @DisplayName("Generates response deserializer code")
        void generatesResponseDeserializerCode() throws IOException {
            Path tempDir = Files.createTempDirectory("protocol-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("test.service#TestOperation")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateResponseDeserializer(operation, writer, context);
            
            assertTrue(generator.wasGenerateResponseDeserializerCalled());
            String output = writer.toString();
            assertTrue(output.contains("Response deserializer for: TestOperation"));
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateHeaders()")
    class GenerateHeadersTests {
        
        @Test
        @DisplayName("Default implementation does nothing")
        void defaultImplementationDoesNothing() throws IOException {
            Path tempDir = Files.createTempDirectory("protocol-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("test.service#TestOperation")
                    .build();
            
            ServiceShape service = ServiceShape.builder()
                    .id("test.service#TestService")
                    .version("1.0")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            // Default implementation should not throw
            assertDoesNotThrow(() -> 
                    generator.generateHeaders(operation, service, writer, context));
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateErrorParser()")
    class GenerateErrorParserTests {
        
        @Test
        @DisplayName("Default implementation does nothing")
        void defaultImplementationDoesNothing() throws IOException {
            Path tempDir = Files.createTempDirectory("protocol-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("test.service#TestOperation")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            // Default implementation should not throw
            assertDoesNotThrow(() -> 
                    generator.generateErrorParser(operation, writer, context));
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("AWS Protocol ShapeIds")
    class AwsProtocolShapeIdsTests {
        
        @Test
        @DisplayName("AWS JSON 1.0 protocol ShapeId is valid")
        void awsJson10ShapeIdIsValid() {
            ShapeId shapeId = ShapeId.from("aws.protocols#awsJson1_0");
            assertEquals("aws.protocols", shapeId.getNamespace());
            assertEquals("awsJson1_0", shapeId.getName());
        }
        
        @Test
        @DisplayName("AWS JSON 1.1 protocol ShapeId is valid")
        void awsJson11ShapeIdIsValid() {
            ShapeId shapeId = ShapeId.from("aws.protocols#awsJson1_1");
            assertEquals("aws.protocols", shapeId.getNamespace());
            assertEquals("awsJson1_1", shapeId.getName());
        }
        
        @Test
        @DisplayName("AWS Query protocol ShapeId is valid")
        void awsQueryShapeIdIsValid() {
            ShapeId shapeId = ShapeId.from("aws.protocols#awsQuery");
            assertEquals("aws.protocols", shapeId.getNamespace());
            assertEquals("awsQuery", shapeId.getName());
        }
        
        @Test
        @DisplayName("EC2 Query protocol ShapeId is valid")
        void ec2QueryShapeIdIsValid() {
            ShapeId shapeId = ShapeId.from("aws.protocols#ec2Query");
            assertEquals("aws.protocols", shapeId.getNamespace());
            assertEquals("ec2Query", shapeId.getName());
        }
        
        @Test
        @DisplayName("REST-XML protocol ShapeId is valid")
        void restXmlShapeIdIsValid() {
            ShapeId shapeId = ShapeId.from("aws.protocols#restXml");
            assertEquals("aws.protocols", shapeId.getNamespace());
            assertEquals("restXml", shapeId.getName());
        }
        
        @Test
        @DisplayName("REST-JSON 1 protocol ShapeId is valid")
        void restJson1ShapeIdIsValid() {
            ShapeId shapeId = ShapeId.from("aws.protocols#restJson1");
            assertEquals("aws.protocols", shapeId.getNamespace());
            assertEquals("restJson1", shapeId.getName());
        }
    }
    
    /**
     * Creates a test context for use in tests.
     */
    private ErlangContext createTestContext(Path outputDir) {
        Model model = Model.builder()
                .addShape(ServiceShape.builder()
                        .id("test.service#TestService")
                        .version("1.0")
                        .build())
                .build();
        
        ErlangSettings settings = ErlangSettings.builder()
                .service(ShapeId.from("test.service#TestService"))
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
