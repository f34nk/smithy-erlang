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
 * Unit tests for AwsQueryProtocolGenerator.
 */
class AwsQueryProtocolGeneratorTest {
    
    private AwsQueryProtocolGenerator generator;
    private ErlangWriter writer;
    
    @BeforeEach
    void setUp() {
        generator = new AwsQueryProtocolGenerator();
        writer = new ErlangWriter("test_module");
    }
    
    @Nested
    @DisplayName("getProtocol()")
    class GetProtocolTests {
        
        @Test
        @DisplayName("Returns aws.protocols#awsQuery")
        void returnsAwsQueryProtocol() {
            assertEquals(AwsQueryProtocolGenerator.AWS_QUERY, generator.getProtocol());
        }
        
        @Test
        @DisplayName("Protocol ShapeId has correct namespace")
        void protocolHasCorrectNamespace() {
            assertEquals("aws.protocols", generator.getProtocol().getNamespace());
            assertEquals("awsQuery", generator.getProtocol().getName());
        }
    }
    
    @Nested
    @DisplayName("getName()")
    class GetNameTests {
        
        @Test
        @DisplayName("Returns 'awsQuery'")
        void returnsAwsQueryName() {
            assertEquals("awsQuery", generator.getName());
        }
    }
    
    @Nested
    @DisplayName("getDefaultMethod()")
    class GetDefaultMethodTests {
        
        @Test
        @DisplayName("Returns POST")
        void returnsPost() {
            assertEquals("POST", generator.getDefaultMethod());
        }
    }
    
    @Nested
    @DisplayName("getDefaultUri()")
    class GetDefaultUriTests {
        
        @Test
        @DisplayName("Returns /")
        void returnsSlash() {
            assertEquals("/", generator.getDefaultUri());
        }
    }
    
    @Nested
    @DisplayName("getContentType()")
    class GetContentTypeTests {
        
        @Test
        @DisplayName("Returns application/x-www-form-urlencoded")
        void returnsFormUrlEncoded() {
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#TestService")
                    .version("1.0")
                    .build();
            
            assertEquals("application/x-www-form-urlencoded", generator.getContentType(service));
        }
    }
    
    @Nested
    @DisplayName("generateRequestSerializer()")
    class GenerateRequestSerializerTests {
        
        @Test
        @DisplayName("Uses aws_query:encode with Action parameter")
        void usesAwsQueryEncode() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-query-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#SendMessage")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateRequestSerializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("Action = <<\"SendMessage\">>"), "Should set Action parameter");
            assertTrue(output.contains("aws_query:encode"), "Should use aws_query:encode");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Includes input in encoding")
        void includesInputInEncoding() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-query-test");
            
            StructureShape inputShape = StructureShape.builder()
                    .id("com.example#SendMessageInput")
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#SendMessage")
                    .input(inputShape.getId())
                    .build();
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#SQS")
                    .version("2012-11-05")
                    .addOperation(ShapeId.from("com.example#SendMessage"))
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(inputShape)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator.generateRequestSerializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_query:encode(Action, Input)"), 
                    "Should encode Action with Input");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateResponseDeserializer()")
    class GenerateResponseDeserializerTests {
        
        @Test
        @DisplayName("Uses aws_xml:decode for deserialization")
        void usesAwsXmlDecode() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-query-test");
            
            StructureShape outputShape = StructureShape.builder()
                    .id("com.example#SendMessageOutput")
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#SendMessage")
                    .output(outputShape.getId())
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateResponseDeserializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_xml:decode"), "Should use aws_xml:decode");
            assertTrue(output.contains("SendMessageResult"), "Should look for OperationNameResult");
            assertTrue(output.contains("SendMessageResponse"), "Should look for OperationNameResponse");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Returns empty map for no output")
        void returnsEmptyMapForNoOutput() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-query-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#DeleteQueue")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateResponseDeserializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("{ok, #{}}"), "Should return empty map");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateErrorParser()")
    class GenerateErrorParserTests {
        
        @Test
        @DisplayName("Parses ErrorResponse/Error structure")
        void parsesErrorResponse() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-query-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#SendMessage")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateErrorParser(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("<<\"ErrorResponse\">>"), "Should look for ErrorResponse");
            assertTrue(output.contains("<<\"Error\">>"), "Should look for Error");
            assertTrue(output.contains("<<\"Code\">>"), "Should extract Code");
            assertTrue(output.contains("<<\"Message\">>"), "Should extract Message");
            assertTrue(output.contains("aws_error"), "Should return aws_error tuple");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateHeaders()")
    class GenerateHeadersTests {
        
        @Test
        @DisplayName("Sets Content-Type to form-urlencoded")
        void setsContentType() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-query-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#SQS")
                    .version("2012-11-05")
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#SendMessage")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateHeaders(operation, service, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("application/x-www-form-urlencoded"), 
                    "Should set Content-Type header");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateOperation()")
    class GenerateOperationTests {
        
        @Test
        @DisplayName("Generates 2-arity and 3-arity functions")
        void generatesBothArities() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-query-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#SQS")
                    .version("2012-11-05")
                    .addOperation(ShapeId.from("com.example#SendMessage"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#SendMessage")
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("send_message(Client, Input) ->"), 
                    "Should generate 2-arity function");
            assertTrue(output.contains("send_message(Client, Input, Options)"), 
                    "Should generate 3-arity function");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Uses POST to / endpoint")
        void usesPostToRoot() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-query-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#SQS")
                    .version("2012-11-05")
                    .addOperation(ShapeId.from("com.example#SendMessage"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#SendMessage")
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("Method = <<\"POST\">>"), "Should use POST method");
            assertTrue(output.contains("Path = <<\"/\">>"), "Should use / path");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Generates retry logic")
        void generatesRetryLogic() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-query-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#SQS")
                    .version("2012-11-05")
                    .addOperation(ShapeId.from("com.example#SendMessage"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#SendMessage")
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
            Path tempDir = Files.createTempDirectory("aws-query-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#SQS")
                    .version("2012-11-05")
                    .addOperation(ShapeId.from("com.example#SendMessage"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#SendMessage")
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
