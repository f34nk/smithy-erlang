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
import software.amazon.smithy.model.shapes.StructureShape;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for RestXmlProtocolGenerator.
 */
class RestXmlProtocolGeneratorTest {
    
    private RestXmlProtocolGenerator generator;
    private ErlangWriter writer;
    
    @BeforeEach
    void setUp() {
        generator = new RestXmlProtocolGenerator();
        writer = new ErlangWriter("test_module");
    }
    
    @Nested
    @DisplayName("getProtocol()")
    class GetProtocolTests {
        
        @Test
        @DisplayName("Returns aws.protocols#restXml")
        void returnsRestXmlProtocol() {
            assertEquals(RestXmlProtocolGenerator.REST_XML, generator.getProtocol());
        }
        
        @Test
        @DisplayName("Protocol ShapeId has correct namespace")
        void protocolHasCorrectNamespace() {
            assertEquals("aws.protocols", generator.getProtocol().getNamespace());
            assertEquals("restXml", generator.getProtocol().getName());
        }
    }
    
    @Nested
    @DisplayName("getName()")
    class GetNameTests {
        
        @Test
        @DisplayName("Returns 'restXml'")
        void returnsRestXmlName() {
            assertEquals("restXml", generator.getName());
        }
    }
    
    @Nested
    @DisplayName("getDefaultMethod()")
    class GetDefaultMethodTests {
        
        @Test
        @DisplayName("Returns null (method from @http trait)")
        void returnsNull() {
            assertNull(generator.getDefaultMethod());
        }
    }
    
    @Nested
    @DisplayName("getDefaultUri()")
    class GetDefaultUriTests {
        
        @Test
        @DisplayName("Returns null (URI from @http trait)")
        void returnsNull() {
            assertNull(generator.getDefaultUri());
        }
    }
    
    @Nested
    @DisplayName("getContentType()")
    class GetContentTypeTests {
        
        @Test
        @DisplayName("Returns application/xml")
        void returnsApplicationXml() {
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#TestService")
                    .version("1.0")
                    .build();
            
            assertEquals("application/xml", generator.getContentType(service));
        }
    }
    
    @Nested
    @DisplayName("generateRequestSerializer()")
    class GenerateRequestSerializerTests {
        
        @Test
        @DisplayName("Uses aws_xml:encode for serialization")
        void usesAwsXmlEncode() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-xml-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetObject")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateRequestSerializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_xml:encode"), "Should use aws_xml:encode");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateResponseDeserializer()")
    class GenerateResponseDeserializerTests {
        
        @Test
        @DisplayName("Uses aws_xml:decode for deserialization")
        void usesAwsXmlDecode() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-xml-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetObject")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateResponseDeserializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_xml:decode"), "Should use aws_xml:decode");
            assertTrue(output.contains("xml_decode_error"), "Should handle decode errors");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateErrorParser()")
    class GenerateErrorParserTests {
        
        @Test
        @DisplayName("Parses Error/Code and Error/Message from XML")
        void parsesXmlError() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-xml-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetObject")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateErrorParser(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("<<\"Error\">>"), "Should look for Error element");
            assertTrue(output.contains("<<\"Code\">>"), "Should extract Code");
            assertTrue(output.contains("<<\"Message\">>"), "Should extract Message");
            assertTrue(output.contains("aws_error"), "Should return aws_error tuple");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateOperation()")
    class GenerateOperationTests {
        
        @Test
        @DisplayName("Generates 2-arity and 3-arity functions")
        void generatesBothArities() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-xml-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#S3")
                    .version("2006-03-01")
                    .addOperation(ShapeId.from("com.example#GetObject"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetObject")
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("get_object(Client, Input) ->"), 
                    "Should generate 2-arity function");
            assertTrue(output.contains("get_object(Client, Input, Options)"), 
                    "Should generate 3-arity function");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Generates retry logic")
        void generatesRetryLogic() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-xml-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#S3")
                    .version("2006-03-01")
                    .addOperation(ShapeId.from("com.example#GetObject"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetObject")
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_retry:with_retry"), 
                    "Should use aws_retry for retry logic");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Generates SigV4 signing")
        void generatesSigV4Signing() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-xml-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#S3")
                    .version("2006-03-01")
                    .addOperation(ShapeId.from("com.example#GetObject"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetObject")
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_sigv4:sign_request"), 
                    "Should use SigV4 for signing");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Detects S3 service and uses aws_s3:build_url")
        void detectsS3Service() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-xml-test");
            
            StructureShape inputShape = StructureShape.builder()
                    .id("com.example#GetObjectInput")
                    .addMember("Bucket", ShapeId.from("smithy.api#String"), 
                            b -> b.addTrait(new software.amazon.smithy.model.traits.HttpLabelTrait()))
                    .addMember("Key", ShapeId.from("smithy.api#String"),
                            b -> b.addTrait(new software.amazon.smithy.model.traits.HttpLabelTrait()))
                    .build();
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.amazonaws.s3#S3")
                    .version("2006-03-01")
                    .addOperation(ShapeId.from("com.amazonaws.s3#GetObject"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.amazonaws.s3#GetObject")
                    .input(inputShape.getId())
                    .addTrait(software.amazon.smithy.model.traits.HttpTrait.builder()
                            .method("GET")
                            .uri(software.amazon.smithy.model.pattern.UriPattern.parse("/{Bucket}/{Key+}"))
                            .code(200)
                            .build())
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .addShape(inputShape)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_s3:build_url"), 
                    "Should use aws_s3:build_url for S3 operations");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("S3 Detection")
    class S3DetectionTests {
        
        @Test
        @DisplayName("Detects S3 by service name containing 's3'")
        void detectsS3ByName() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-xml-test");
            
            StructureShape inputShape = StructureShape.builder()
                    .id("com.example#ListBucketsInput")
                    .addMember("Bucket", ShapeId.from("smithy.api#String"),
                            b -> b.addTrait(new software.amazon.smithy.model.traits.HttpLabelTrait()))
                    .build();
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#MyS3Clone")
                    .version("1.0")
                    .addOperation(ShapeId.from("com.example#ListBuckets"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#ListBuckets")
                    .input(inputShape.getId())
                    .addTrait(software.amazon.smithy.model.traits.HttpTrait.builder()
                            .method("GET")
                            .uri(software.amazon.smithy.model.pattern.UriPattern.parse("/{Bucket}"))
                            .code(200)
                            .build())
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .addShape(inputShape)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            // Should detect S3-like service and use S3 URL builder
            assertTrue(output.contains("aws_s3:build_url") || output.contains("S3-specific"), 
                    "Should detect S3-like service");
            
            cleanupDir(tempDir);
        }
    }
    
    /**
     * Creates a test context with default service.
     */
    private ErlangContext createTestContext(Path outputDir) {
        ServiceShape service = ServiceShape.builder()
                .id("com.example#TestService")
                .version("1.0")
                .build();
        
        Model model = Model.builder()
                .addShape(service)
                .build();
        
        return createTestContextWithModel(outputDir, model, service);
    }
    
    /**
     * Creates a test context with a model and service.
     */
    private ErlangContext createTestContextWithModel(Path outputDir, Model model, ServiceShape service) {
        ErlangSettings settings = ErlangSettings.builder()
                .service(service.getId())
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
                .service(service)
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
