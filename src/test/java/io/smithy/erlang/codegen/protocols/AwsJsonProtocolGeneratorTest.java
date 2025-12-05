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
 * Unit tests for AwsJsonProtocolGenerator.
 */
class AwsJsonProtocolGeneratorTest {
    
    private AwsJsonProtocolGenerator generator10;
    private AwsJsonProtocolGenerator generator11;
    private ErlangWriter writer;
    
    @BeforeEach
    void setUp() {
        generator10 = new AwsJsonProtocolGenerator();
        generator11 = AwsJsonProtocolGenerator.awsJson11();
        writer = new ErlangWriter("test_module");
    }
    
    @Nested
    @DisplayName("getProtocol()")
    class GetProtocolTests {
        
        @Test
        @DisplayName("Returns awsJson1_0 for default constructor")
        void returnsAwsJson10ForDefaultConstructor() {
            assertEquals(AwsJsonProtocolGenerator.AWS_JSON_1_0, generator10.getProtocol());
        }
        
        @Test
        @DisplayName("Returns awsJson1_1 for awsJson11() factory")
        void returnsAwsJson11ForFactory() {
            assertEquals(AwsJsonProtocolGenerator.AWS_JSON_1_1, generator11.getProtocol());
        }
        
        @Test
        @DisplayName("Protocol ShapeId has correct namespace")
        void protocolHasCorrectNamespace() {
            assertEquals("aws.protocols", generator10.getProtocol().getNamespace());
            assertEquals("aws.protocols", generator11.getProtocol().getNamespace());
        }
    }
    
    @Nested
    @DisplayName("getName()")
    class GetNameTests {
        
        @Test
        @DisplayName("Returns 'awsJson1.0' for JSON 1.0")
        void returnsAwsJson10Name() {
            assertEquals("awsJson1.0", generator10.getName());
        }
        
        @Test
        @DisplayName("Returns 'awsJson1.1' for JSON 1.1")
        void returnsAwsJson11Name() {
            assertEquals("awsJson1.1", generator11.getName());
        }
    }
    
    @Nested
    @DisplayName("getDefaultMethod()")
    class GetDefaultMethodTests {
        
        @Test
        @DisplayName("Returns POST for AWS JSON protocol")
        void returnsPost() {
            assertEquals("POST", generator10.getDefaultMethod());
            assertEquals("POST", generator11.getDefaultMethod());
        }
    }
    
    @Nested
    @DisplayName("getDefaultUri()")
    class GetDefaultUriTests {
        
        @Test
        @DisplayName("Returns / for AWS JSON protocol")
        void returnsRootPath() {
            assertEquals("/", generator10.getDefaultUri());
            assertEquals("/", generator11.getDefaultUri());
        }
    }
    
    @Nested
    @DisplayName("getContentType()")
    class GetContentTypeTests {
        
        @Test
        @DisplayName("Returns application/x-amz-json-1.0 for JSON 1.0")
        void returnsJson10ContentType() {
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#TestService")
                    .version("1.0")
                    .build();
            
            assertEquals("application/x-amz-json-1.0", generator10.getContentType(service));
        }
        
        @Test
        @DisplayName("Returns application/x-amz-json-1.1 for JSON 1.1")
        void returnsJson11ContentType() {
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#TestService")
                    .version("1.0")
                    .build();
            
            assertEquals("application/x-amz-json-1.1", generator11.getContentType(service));
        }
    }
    
    @Nested
    @DisplayName("generateHeaders()")
    class GenerateHeadersTests {
        
        @Test
        @DisplayName("Generates X-Amz-Target header")
        void generatesXAmzTargetHeader() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#TestService")
                    .version("2023-01-01")
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            ErlangContext context = createTestContext(tempDir, service);
            
            generator10.generateHeaders(operation, service, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("X-Amz-Target"), "Should contain X-Amz-Target header");
            assertTrue(output.contains("TestService_2023-01-01.GetItem"), 
                    "Should have correct target format");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Generates Content-Type header with correct version")
        void generatesContentTypeHeader() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#TestService")
                    .version("2023-01-01")
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            ErlangContext context = createTestContext(tempDir, service);
            
            generator11.generateHeaders(operation, service, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("Content-Type"), "Should contain Content-Type header");
            assertTrue(output.contains("application/x-amz-json-1.1"), 
                    "Should have JSON 1.1 content type");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateRequestSerializer()")
    class GenerateRequestSerializerTests {
        
        @Test
        @DisplayName("Generates jsx:encode for operations with input")
        void generatesJsxEncodeWithInput() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            StructureShape inputShape = StructureShape.builder()
                    .id("com.example#GetItemInput")
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .input(inputShape.getId())
                    .build();
            
            Model model = Model.builder()
                    .addShape(inputShape)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model);
            
            generator10.generateRequestSerializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("jsx:encode(Input)"), 
                    "Should encode input with jsx");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Generates empty JSON for operations without input")
        void generatesEmptyJsonWithoutInput() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#ListItems")
                    .build();
            
            Model model = Model.builder()
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model);
            
            generator10.generateRequestSerializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("<<\"{}\">>"), 
                    "Should use empty JSON object for no input");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateResponseDeserializer()")
    class GenerateResponseDeserializerTests {
        
        @Test
        @DisplayName("Generates jsx:decode for operations with output")
        void generatesJsxDecodeWithOutput() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            StructureShape outputShape = StructureShape.builder()
                    .id("com.example#GetItemOutput")
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .output(outputShape.getId())
                    .build();
            
            Model model = Model.builder()
                    .addShape(outputShape)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model);
            
            generator10.generateResponseDeserializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("jsx:decode(ResponseBody"), 
                    "Should decode response with jsx");
            assertTrue(output.contains("return_maps"), 
                    "Should use return_maps option");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Returns empty map for operations without output")
        void returnsEmptyMapWithoutOutput() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#DeleteItem")
                    .build();
            
            Model model = Model.builder()
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model);
            
            generator10.generateResponseDeserializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("{ok, #{}}"), 
                    "Should return empty map for no output");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateErrorParser()")
    class GenerateErrorParserTests {
        
        @Test
        @DisplayName("Parses __type field for error code")
        void parsesTypeFieldForErrorCode() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            Model model = Model.builder()
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model);
            
            generator10.generateErrorParser(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("__type"), 
                    "Should look for __type field");
            assertTrue(output.contains("aws_error"), 
                    "Should return aws_error tuple");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Handles message and Message fields")
        void handlesMessageFields() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            Model model = Model.builder()
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model);
            
            generator10.generateErrorParser(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("<<\"message\">>"), 
                    "Should check for lowercase message");
            assertTrue(output.contains("<<\"Message\">>"), 
                    "Should check for uppercase Message");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateOperation()")
    class GenerateOperationTests {
        
        @Test
        @DisplayName("Generates 2-arity and 3-arity functions")
        void generatesBothArities() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#TestService")
                    .version("2023-01-01")
                    .addOperation(ShapeId.from("com.example#GetItem"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator10.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            
            // Check for 2-arity function
            assertTrue(output.contains("get_item(Client, Input) ->"), 
                    "Should generate 2-arity function");
            
            // Check for 3-arity function with options
            assertTrue(output.contains("get_item(Client, Input, Options)"), 
                    "Should generate 3-arity function");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Generates retry logic")
        void generatesRetryLogic() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#TestService")
                    .version("2023-01-01")
                    .addOperation(ShapeId.from("com.example#GetItem"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator10.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_retry:with_retry"), 
                    "Should use aws_retry for retry logic");
            assertTrue(output.contains("enable_retry"), 
                    "Should check enable_retry option");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Generates SigV4 signing")
        void generatesSigV4Signing() throws IOException {
            Path tempDir = Files.createTempDirectory("aws-json-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#TestService")
                    .version("2023-01-01")
                    .addOperation(ShapeId.from("com.example#GetItem"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator10.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_sigv4:sign_request"), 
                    "Should use SigV4 for signing");
            
            cleanupDir(tempDir);
        }
    }
    
    /**
     * Creates a test context with a service.
     */
    private ErlangContext createTestContext(Path outputDir, ServiceShape service) {
        Model model = Model.builder()
                .addShape(service)
                .build();
        
        return createTestContextWithModel(outputDir, model, service);
    }
    
    /**
     * Creates a test context with a model.
     */
    private ErlangContext createTestContextWithModel(Path outputDir, Model model) {
        ServiceShape service = ServiceShape.builder()
                .id("com.example#TestService")
                .version("1.0")
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
