package io.smithy.erlang.codegen.generators;

import io.smithy.erlang.codegen.*;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.codegen.core.WriterDelegator;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.EnumDefinition;
import software.amazon.smithy.model.traits.EnumTrait;
import software.amazon.smithy.model.traits.PaginatedTrait;
import software.amazon.smithy.model.traits.RequiredTrait;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for ServiceGenerator.
 */
class ServiceGeneratorTest {
    
    private static final ShapeId TEST_SERVICE_ID = ShapeId.from("com.example#TestService");
    private static final ShapeId TEST_OPERATION_ID = ShapeId.from("com.example#TestOperation");
    private static final ShapeId TEST_INPUT_ID = ShapeId.from("com.example#TestInput");
    private static final ShapeId TEST_OUTPUT_ID = ShapeId.from("com.example#TestOutput");
    
    private Model model;
    private ErlangSettings settings;
    private ErlangContext context;
    
    @TempDir
    Path tempDir;
    
    @BeforeEach
    void setUp() {
        // Create basic model
        model = createBasicModel();
        
        settings = ErlangSettings.builder()
                .service(TEST_SERVICE_ID)
                .moduleName("test_service")
                .build();
        
        context = createContext(model, settings);
    }
    
    private ErlangContext createContext(Model testModel, ErlangSettings testSettings) {
        FileManifest fileManifest = FileManifest.create(tempDir);
        SymbolProvider symbolProvider = new EnhancedErlangSymbolProvider(testModel, testSettings);
        WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                fileManifest, symbolProvider, ErlangWriter.factory());
        
        return ErlangContext.builder()
                .model(testModel)
                .settings(testSettings)
                .symbolProvider(symbolProvider)
                .fileManifest(fileManifest)
                .writerDelegator(writerDelegator)
                .integrations(Collections.emptyList())
                .build();
    }
    
    private Model createBasicModel() {
        return Model.assembler()
                .addShape(ServiceShape.builder()
                        .id(TEST_SERVICE_ID)
                        .version("1.0")
                        .addOperation(TEST_OPERATION_ID)
                        .build())
                .addShape(OperationShape.builder()
                        .id(TEST_OPERATION_ID)
                        .input(TEST_INPUT_ID)
                        .output(TEST_OUTPUT_ID)
                        .build())
                .addShape(StructureShape.builder()
                        .id(TEST_INPUT_ID)
                        .addMember("name", ShapeId.from("smithy.api#String"))
                        .build())
                .addShape(StructureShape.builder()
                        .id(TEST_OUTPUT_ID)
                        .addMember("result", ShapeId.from("smithy.api#String"))
                        .build())
                .assemble()
                .unwrap();
    }
    
    // ========== Constructor Tests ==========
    
    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {
        
        @Test
        @DisplayName("Creates generator with service and context")
        void testConstructor() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            assertNotNull(generator);
            assertEquals(service, generator.getService());
            assertEquals(context, generator.getContext());
        }
        
        @Test
        @DisplayName("Throws NullPointerException for null service")
        void testNullService() {
            assertThrows(NullPointerException.class, 
                    () -> new ServiceGenerator(null, context));
        }
        
        @Test
        @DisplayName("Throws NullPointerException for null context")
        void testNullContext() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            assertThrows(NullPointerException.class, 
                    () -> new ServiceGenerator(service, null));
        }
    }
    
    // ========== Operation Collection Tests ==========
    
    @Nested
    @DisplayName("Operation Collection")
    class OperationCollectionTests {
        
        @Test
        @DisplayName("Collects all service operations")
        void testCollectsOperations() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            List<OperationShape> operations = generator.getOperations();
            
            assertEquals(1, operations.size());
            assertEquals(TEST_OPERATION_ID, operations.get(0).getId());
        }
    }
    
    // ========== Structure Collection Tests ==========
    
    @Nested
    @DisplayName("Structure Collection")
    class StructureCollectionTests {
        
        @Test
        @DisplayName("Collects input and output structures")
        void testCollectsStructures() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            List<StructureShape> structures = generator.getStructures();
            
            assertEquals(2, structures.size());
            assertTrue(structures.stream().anyMatch(s -> s.getId().equals(TEST_INPUT_ID)));
            assertTrue(structures.stream().anyMatch(s -> s.getId().equals(TEST_OUTPUT_ID)));
        }
    }
    
    // ========== Validation Collection Tests ==========
    
    @Nested
    @DisplayName("Validation Collection")
    class ValidationCollectionTests {
        
        @Test
        @DisplayName("Does not collect structures without required fields")
        void testNoValidationForOptionalOnly() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            List<StructureShape> toValidate = generator.getInputStructuresToValidate();
            
            assertTrue(toValidate.isEmpty(), "Should not collect structures without required fields");
        }
    }
    
    // ========== Module Name Tests ==========
    
    @Nested
    @DisplayName("Module Name")
    class ModuleNameTests {
        
        @Test
        @DisplayName("Uses explicit module name from settings")
        void testExplicitModuleName() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            assertEquals("test_service", generator.getModuleName());
        }
        
        @Test
        @DisplayName("Derives module name from service when not set")
        void testDerivedModuleName() {
            ErlangSettings noModuleSettings = ErlangSettings.builder()
                    .service(TEST_SERVICE_ID)
                    .build();
            
            ErlangContext noModuleContext = createContext(model, noModuleSettings);
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, noModuleContext);
            
            assertEquals("test_service", generator.getModuleName());
        }
    }
    
    // ========== Generation Tests ==========
    
    @Nested
    @DisplayName("Generation")
    class GenerationTests {
        
        @Test
        @DisplayName("Generates module file")
        void testGeneratesModuleFile() throws Exception {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            generator.generate();
            
            // Flush the delegator
            context.writerDelegator().flushWriters();
            
            Path generatedFile = tempDir.resolve("test_service.erl");
            assertTrue(Files.exists(generatedFile), "Should generate module file");
        }
        
        @Test
        @DisplayName("Generated file contains module declaration")
        void testContainsModuleDeclaration() throws Exception {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            generator.generate();
            context.writerDelegator().flushWriters();
            
            Path generatedFile = tempDir.resolve("test_service.erl");
            String content = Files.readString(generatedFile);
            
            assertTrue(content.contains("-module(test_service)"), 
                    "Should contain module declaration");
        }
        
        @Test
        @DisplayName("Generated file contains exports")
        void testContainsExports() throws Exception {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            generator.generate();
            context.writerDelegator().flushWriters();
            
            Path generatedFile = tempDir.resolve("test_service.erl");
            String content = Files.readString(generatedFile);
            
            assertTrue(content.contains("-export(["), "Should contain exports");
            assertTrue(content.contains("new/1"), "Should export new/1");
            assertTrue(content.contains("test_operation/2"), "Should export operation");
        }
        
        @Test
        @DisplayName("Generated file contains dialyzer suppression")
        void testContainsDialyzerSuppression() throws Exception {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            generator.generate();
            context.writerDelegator().flushWriters();
            
            Path generatedFile = tempDir.resolve("test_service.erl");
            String content = Files.readString(generatedFile);
            
            assertTrue(content.contains("-dialyzer([no_contracts, no_match])"), 
                    "Should contain dialyzer suppression");
        }
        
        @Test
        @DisplayName("Generated file contains service comment")
        void testContainsServiceComment() throws Exception {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            generator.generate();
            context.writerDelegator().flushWriters();
            
            Path generatedFile = tempDir.resolve("test_service.erl");
            String content = Files.readString(generatedFile);
            
            assertTrue(content.contains("Generated Smithy client for TestService"), 
                    "Should contain service comment");
        }
    }
    
    // ========== Export Generation Tests ==========
    
    @Nested
    @DisplayName("Export Generation")
    class ExportGenerationTests {
        
        @Test
        @DisplayName("Exports operation with retry arity")
        void testExportsRetryArity() throws Exception {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            generator.generate();
            context.writerDelegator().flushWriters();
            
            Path generatedFile = tempDir.resolve("test_service.erl");
            String content = Files.readString(generatedFile);
            
            assertTrue(content.contains("test_operation/3"), 
                    "Should export operation with retry arity /3");
        }
        
        @Test
        @DisplayName("Exports type definitions for structures")
        void testExportsTypeDefinitions() throws Exception {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            generator.generate();
            context.writerDelegator().flushWriters();
            
            Path generatedFile = tempDir.resolve("test_service.erl");
            String content = Files.readString(generatedFile);
            
            assertTrue(content.contains("-export_type(["), 
                    "Should contain type exports");
            assertTrue(content.contains("test_input/0"), 
                    "Should export input type");
            assertTrue(content.contains("test_output/0"), 
                    "Should export output type");
        }
    }
    
    // ========== Accessor Tests ==========
    
    @Nested
    @DisplayName("Accessors")
    class AccessorTests {
        
        @Test
        @DisplayName("getService returns the service shape")
        void testGetService() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            assertSame(service, generator.getService());
        }
        
        @Test
        @DisplayName("getContext returns the context")
        void testGetContext() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            assertSame(context, generator.getContext());
        }
        
        @Test
        @DisplayName("getOperations returns unmodifiable list")
        void testGetOperationsUnmodifiable() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            List<OperationShape> operations = generator.getOperations();
            
            assertThrows(UnsupportedOperationException.class, () -> operations.clear());
        }
        
        @Test
        @DisplayName("getStructures returns unmodifiable list")
        void testGetStructuresUnmodifiable() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            List<StructureShape> structures = generator.getStructures();
            
            assertThrows(UnsupportedOperationException.class, () -> structures.clear());
        }
        
        @Test
        @DisplayName("getUnions returns unmodifiable list")
        void testGetUnionsUnmodifiable() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            List<UnionShape> unions = generator.getUnions();
            
            assertThrows(UnsupportedOperationException.class, () -> unions.clear());
        }
        
        @Test
        @DisplayName("getEnums returns unmodifiable list")
        void testGetEnumsUnmodifiable() {
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            ServiceGenerator generator = new ServiceGenerator(service, context);
            
            List<StringShape> enums = generator.getEnums();
            
            assertThrows(UnsupportedOperationException.class, () -> enums.clear());
        }
    }
}
