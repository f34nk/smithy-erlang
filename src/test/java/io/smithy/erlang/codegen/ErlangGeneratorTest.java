package io.smithy.erlang.codegen;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.io.TempDir;
import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.codegen.core.WriterDelegator;
import software.amazon.smithy.codegen.core.directed.DirectedCodegen;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.*;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for ErlangGenerator.
 */
class ErlangGeneratorTest {
    
    private static final ShapeId TEST_SERVICE_ID = ShapeId.from("com.example#TestService");
    private static final ShapeId TEST_OPERATION_ID = ShapeId.from("com.example#TestOperation");
    
    private ErlangGenerator generator;
    private Model model;
    private ErlangSettings settings;
    
    @TempDir
    Path tempDir;
    
    @BeforeEach
    void setUp() {
        generator = new ErlangGenerator();
        
        // Create a minimal model with a service
        model = Model.assembler()
                .addShape(ServiceShape.builder()
                        .id(TEST_SERVICE_ID)
                        .version("1.0")
                        .addOperation(TEST_OPERATION_ID)
                        .build())
                .addShape(OperationShape.builder()
                        .id(TEST_OPERATION_ID)
                        .build())
                .assemble()
                .unwrap();
        
        settings = ErlangSettings.builder()
                .service(TEST_SERVICE_ID)
                .moduleName("test_service")
                .build();
    }
    
    // ========== Basic Tests ==========
    
    @Nested
    @DisplayName("Basic Tests")
    class BasicTests {
        
        @Test
        @DisplayName("ErlangGenerator is a DirectedCodegen")
        void testIsDirectedCodegen() {
            assertTrue(generator instanceof DirectedCodegen);
        }
        
        @Test
        @DisplayName("ErlangGenerator can be instantiated")
        void testInstantiation() {
            ErlangGenerator gen = new ErlangGenerator();
            assertNotNull(gen);
        }
    }
    
    // ========== Symbol Provider Tests ==========
    
    @Nested
    @DisplayName("Symbol Provider")
    class SymbolProviderTests {
        
        @Test
        @DisplayName("Creates symbol provider from model and settings")
        void testCreateSymbolProvider() {
            // Use the EnhancedErlangSymbolProvider directly for testing
            io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider provider = 
                    new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            
            assertNotNull(provider);
        }
        
        @Test
        @DisplayName("Symbol provider resolves service shape")
        void testSymbolProviderResolvesService() {
            io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider provider = 
                    new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            
            ServiceShape service = model.expectShape(TEST_SERVICE_ID, ServiceShape.class);
            var symbol = provider.toSymbol(service);
            
            assertNotNull(symbol);
            assertEquals("test_service", symbol.getName());
        }
        
        @Test
        @DisplayName("Symbol provider resolves operation shape")
        void testSymbolProviderResolvesOperation() {
            io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider provider = 
                    new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            
            OperationShape operation = model.expectShape(TEST_OPERATION_ID, OperationShape.class);
            var symbol = provider.toSymbol(operation);
            
            assertNotNull(symbol);
            assertEquals("test_operation", symbol.getName());
        }
    }
    
    // ========== Context Creation Tests ==========
    
    @Nested
    @DisplayName("Context Creation")
    class ContextCreationTests {
        
        @Test
        @DisplayName("Context can be built with all components")
        void testContextBuilding() {
            FileManifest fileManifest = FileManifest.create(tempDir);
            SymbolProvider symbolProvider = new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                    fileManifest, symbolProvider, ErlangWriter.factory());
            
            ErlangContext context = ErlangContext.builder()
                    .model(model)
                    .settings(settings)
                    .symbolProvider(symbolProvider)
                    .fileManifest(fileManifest)
                    .writerDelegator(writerDelegator)
                    .integrations(Collections.emptyList())
                    .build();
            
            assertNotNull(context);
            assertEquals(model, context.model());
            assertEquals(settings, context.settings());
            assertSame(symbolProvider, context.symbolProvider());
            assertSame(fileManifest, context.fileManifest());
            assertSame(writerDelegator, context.writerDelegator());
        }
        
        @Test
        @DisplayName("Context includes integrations")
        void testContextWithIntegrations() {
            FileManifest fileManifest = FileManifest.create(tempDir);
            SymbolProvider symbolProvider = new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                    fileManifest, symbolProvider, ErlangWriter.factory());
            
            List<ErlangIntegration> integrations = new ArrayList<>();
            integrations.add(new TestIntegration("Integration1"));
            integrations.add(new TestIntegration("Integration2"));
            
            ErlangContext context = ErlangContext.builder()
                    .model(model)
                    .settings(settings)
                    .symbolProvider(symbolProvider)
                    .fileManifest(fileManifest)
                    .writerDelegator(writerDelegator)
                    .integrations(integrations)
                    .build();
            
            assertEquals(2, context.integrations().size());
        }
    }
    
    // ========== WriterDelegator Tests ==========
    
    @Nested
    @DisplayName("WriterDelegator")
    class WriterDelegatorTests {
        
        @Test
        @DisplayName("WriterDelegator can be created with ErlangWriter factory")
        void testWriterDelegatorCreation() {
            FileManifest fileManifest = FileManifest.create(tempDir);
            SymbolProvider symbolProvider = new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            
            WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                    fileManifest, symbolProvider, ErlangWriter.factory());
            
            assertNotNull(writerDelegator);
        }
        
        @Test
        @DisplayName("WriterDelegator creates ErlangWriter for files")
        void testWriterDelegatorCreatesWriter() {
            FileManifest fileManifest = FileManifest.create(tempDir);
            SymbolProvider symbolProvider = new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            
            WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                    fileManifest, symbolProvider, ErlangWriter.factory());
            
            writerDelegator.useFileWriter("test_module.erl", writer -> {
                assertNotNull(writer);
                assertEquals("test_module", writer.getModuleName());
            });
        }
    }
    
    // ========== Integration Execution Tests ==========
    
    @Nested
    @DisplayName("Integration Execution")
    class IntegrationExecutionTests {
        
        @Test
        @DisplayName("Integrations receive context in preprocessModel")
        void testIntegrationPreprocessModel() {
            FileManifest fileManifest = FileManifest.create(tempDir);
            SymbolProvider symbolProvider = new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                    fileManifest, symbolProvider, ErlangWriter.factory());
            
            TrackingIntegration integration = new TrackingIntegration();
            List<ErlangIntegration> integrations = Collections.singletonList(integration);
            
            ErlangContext context = ErlangContext.builder()
                    .model(model)
                    .settings(settings)
                    .symbolProvider(symbolProvider)
                    .fileManifest(fileManifest)
                    .writerDelegator(writerDelegator)
                    .integrations(integrations)
                    .build();
            
            // Simulate what customizeBeforeShapeGeneration does
            for (ErlangIntegration i : context.integrations()) {
                i.preprocessModel(context);
            }
            
            assertTrue(integration.preprocessCalled);
            assertSame(context, integration.receivedContext);
        }
        
        @Test
        @DisplayName("Integrations receive context in postprocessGeneration")
        void testIntegrationPostprocessGeneration() {
            FileManifest fileManifest = FileManifest.create(tempDir);
            SymbolProvider symbolProvider = new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                    fileManifest, symbolProvider, ErlangWriter.factory());
            
            TrackingIntegration integration = new TrackingIntegration();
            List<ErlangIntegration> integrations = Collections.singletonList(integration);
            
            ErlangContext context = ErlangContext.builder()
                    .model(model)
                    .settings(settings)
                    .symbolProvider(symbolProvider)
                    .fileManifest(fileManifest)
                    .writerDelegator(writerDelegator)
                    .integrations(integrations)
                    .build();
            
            // Simulate what customizeAfterShapeGeneration does
            for (ErlangIntegration i : context.integrations()) {
                i.postprocessGeneration(context);
            }
            
            assertTrue(integration.postprocessCalled);
            assertSame(context, integration.receivedContext);
        }
        
        @Test
        @DisplayName("Multiple integrations are all executed")
        void testMultipleIntegrationsExecuted() {
            FileManifest fileManifest = FileManifest.create(tempDir);
            SymbolProvider symbolProvider = new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                    fileManifest, symbolProvider, ErlangWriter.factory());
            
            TrackingIntegration integration1 = new TrackingIntegration();
            TrackingIntegration integration2 = new TrackingIntegration();
            List<ErlangIntegration> integrations = List.of(integration1, integration2);
            
            ErlangContext context = ErlangContext.builder()
                    .model(model)
                    .settings(settings)
                    .symbolProvider(symbolProvider)
                    .fileManifest(fileManifest)
                    .writerDelegator(writerDelegator)
                    .integrations(integrations)
                    .build();
            
            // Simulate preprocessing
            for (ErlangIntegration i : context.integrations()) {
                i.preprocessModel(context);
            }
            
            assertTrue(integration1.preprocessCalled);
            assertTrue(integration2.preprocessCalled);
        }
    }
    
    // ========== Service Shape Tests ==========
    
    @Nested
    @DisplayName("Service Shape")
    class ServiceShapeTests {
        
        @Test
        @DisplayName("Context provides service shape")
        void testContextServiceShape() {
            FileManifest fileManifest = FileManifest.create(tempDir);
            SymbolProvider symbolProvider = new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                    fileManifest, symbolProvider, ErlangWriter.factory());
            
            ErlangContext context = ErlangContext.builder()
                    .model(model)
                    .settings(settings)
                    .symbolProvider(symbolProvider)
                    .fileManifest(fileManifest)
                    .writerDelegator(writerDelegator)
                    .integrations(Collections.emptyList())
                    .build();
            
            ServiceShape service = context.serviceShape();
            
            assertNotNull(service);
            assertEquals(TEST_SERVICE_ID, service.getId());
        }
        
        @Test
        @DisplayName("Context provides service ID")
        void testContextServiceId() {
            FileManifest fileManifest = FileManifest.create(tempDir);
            SymbolProvider symbolProvider = new io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider(model, settings);
            WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                    fileManifest, symbolProvider, ErlangWriter.factory());
            
            ErlangContext context = ErlangContext.builder()
                    .model(model)
                    .settings(settings)
                    .symbolProvider(symbolProvider)
                    .fileManifest(fileManifest)
                    .writerDelegator(writerDelegator)
                    .integrations(Collections.emptyList())
                    .build();
            
            assertEquals(TEST_SERVICE_ID, context.service());
        }
    }
    
    // ========== Helper Classes ==========
    
    /**
     * Test integration for verifying integration parameters.
     */
    private static class TestIntegration implements ErlangIntegration {
        private final String name;
        
        TestIntegration(String name) {
            this.name = name;
        }
        
        @Override
        public String name() {
            return name;
        }
    }
    
    /**
     * Integration that tracks method calls for testing.
     */
    private static class TrackingIntegration implements ErlangIntegration {
        boolean preprocessCalled = false;
        boolean postprocessCalled = false;
        ErlangContext receivedContext = null;
        
        @Override
        public String name() {
            return "TrackingIntegration";
        }
        
        @Override
        public void preprocessModel(ErlangContext context) {
            preprocessCalled = true;
            receivedContext = context;
        }
        
        @Override
        public void postprocessGeneration(ErlangContext context) {
            postprocessCalled = true;
            receivedContext = context;
        }
    }
}
