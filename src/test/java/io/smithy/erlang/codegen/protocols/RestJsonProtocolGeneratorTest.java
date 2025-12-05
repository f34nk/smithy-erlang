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
 * Unit tests for RestJsonProtocolGenerator.
 */
class RestJsonProtocolGeneratorTest {
    
    private RestJsonProtocolGenerator generator;
    private ErlangWriter writer;
    
    @BeforeEach
    void setUp() {
        generator = new RestJsonProtocolGenerator();
        writer = new ErlangWriter("test_module");
    }
    
    @Nested
    @DisplayName("getProtocol()")
    class GetProtocolTests {
        
        @Test
        @DisplayName("Returns aws.protocols#restJson1")
        void returnsRestJsonProtocol() {
            assertEquals(RestJsonProtocolGenerator.REST_JSON_1, generator.getProtocol());
        }
        
        @Test
        @DisplayName("Protocol ShapeId has correct namespace")
        void protocolHasCorrectNamespace() {
            assertEquals("aws.protocols", generator.getProtocol().getNamespace());
            assertEquals("restJson1", generator.getProtocol().getName());
        }
    }
    
    @Nested
    @DisplayName("getName()")
    class GetNameTests {
        
        @Test
        @DisplayName("Returns 'restJson1'")
        void returnsRestJsonName() {
            assertEquals("restJson1", generator.getName());
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
        @DisplayName("Returns application/json")
        void returnsApplicationJson() {
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#TestService")
                    .version("1.0")
                    .build();
            
            assertEquals("application/json", generator.getContentType(service));
        }
    }
    
    @Nested
    @DisplayName("generateRequestSerializer()")
    class GenerateRequestSerializerTests {
        
        @Test
        @DisplayName("Uses jsx:encode for serialization")
        void usesJsxEncode() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-json-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#CreateItem")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateRequestSerializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("jsx:encode"), "Should use jsx:encode");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateResponseDeserializer()")
    class GenerateResponseDeserializerTests {
        
        @Test
        @DisplayName("Uses jsx:decode for deserialization")
        void usesJsxDecode() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-json-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateResponseDeserializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("jsx:decode"), "Should use jsx:decode");
            assertTrue(output.contains("return_maps"), "Should use return_maps option");
            assertTrue(output.contains("json_decode_error"), "Should handle decode errors");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Handles empty response body")
        void handlesEmptyBody() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-json-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateResponseDeserializer(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("<<>>"), "Should handle empty body");
            assertTrue(output.contains("<<\"{}\">>"), "Should handle empty JSON object");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateErrorParser()")
    class GenerateErrorParserTests {
        
        @Test
        @DisplayName("Parses message and __type fields")
        void parsesMessageAndType() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-json-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateErrorParser(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("<<\"message\">>"), "Should look for 'message'");
            assertTrue(output.contains("<<\"Message\">>"), "Should look for 'Message'");
            assertTrue(output.contains("<<\"__type\">>"), "Should look for '__type'");
            assertTrue(output.contains("<<\"code\">>"), "Should look for 'code'");
            assertTrue(output.contains("<<\"Code\">>"), "Should look for 'Code'");
            assertTrue(output.contains("aws_error"), "Should return aws_error tuple");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Handles empty error body")
        void handlesEmptyErrorBody() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-json-test");
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .build();
            
            ErlangContext context = createTestContext(tempDir);
            
            generator.generateErrorParser(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("<<\"No error body\">>"), "Should handle empty error body");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("generateOperation()")
    class GenerateOperationTests {
        
        @Test
        @DisplayName("Generates 2-arity and 3-arity functions")
        void generatesBothArities() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-json-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#ApiGateway")
                    .version("2015-07-09")
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
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("get_item(Client, Input) ->"), 
                    "Should generate 2-arity function");
            assertTrue(output.contains("get_item(Client, Input, Options)"), 
                    "Should generate 3-arity function");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Generates retry logic")
        void generatesRetryLogic() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-json-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#ApiGateway")
                    .version("2015-07-09")
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
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_retry:with_retry"), 
                    "Should use aws_retry for retry logic");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Generates SigV4 signing")
        void generatesSigV4Signing() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-json-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#ApiGateway")
                    .version("2015-07-09")
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
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("aws_sigv4:sign_request"), 
                    "Should use SigV4 for signing");
            
            cleanupDir(tempDir);
        }
        
        @Test
        @DisplayName("Handles success status codes 200-299")
        void handlesSuccessStatusCodes() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-json-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#ApiGateway")
                    .version("2015-07-09")
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
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("StatusCode >= 200"), "Should check for 200+");
            assertTrue(output.contains("StatusCode < 300"), "Should check for <300");
            
            cleanupDir(tempDir);
        }
    }
    
    @Nested
    @DisplayName("HTTP Methods")
    class HttpMethodTests {
        
        @Test
        @DisplayName("Generates GET without body")
        void generatesGetWithoutBody() throws IOException {
            Path tempDir = Files.createTempDirectory("rest-json-test");
            
            ServiceShape service = ServiceShape.builder()
                    .id("com.example#ApiGateway")
                    .version("2015-07-09")
                    .addOperation(ShapeId.from("com.example#GetItem"))
                    .build();
            
            OperationShape operation = OperationShape.builder()
                    .id("com.example#GetItem")
                    .addTrait(software.amazon.smithy.model.traits.HttpTrait.builder()
                            .method("GET")
                            .uri(software.amazon.smithy.model.pattern.UriPattern.parse("/items/{id}"))
                            .code(200)
                            .build())
                    .build();
            
            Model model = Model.builder()
                    .addShape(service)
                    .addShape(operation)
                    .build();
            
            ErlangContext context = createTestContextWithModel(tempDir, model, service);
            
            generator.generateOperation(operation, writer, context);
            
            String output = writer.toString();
            assertTrue(output.contains("<<\"GET\">>"), "Should detect GET method");
            
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
