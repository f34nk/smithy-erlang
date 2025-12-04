package io.smithy.erlang.codegen;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.codegen.core.Symbol;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.codegen.core.WriterDelegator;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.Shape;
import software.amazon.smithy.model.shapes.ShapeId;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for ErlangContext.
 */
class ErlangContextTest {
    
    private static final ShapeId TEST_SERVICE_ID = ShapeId.from("com.example#TestService");
    
    private Model model;
    private ErlangSettings settings;
    private SymbolProvider symbolProvider;
    private FileManifest fileManifest;
    private WriterDelegator<ErlangWriter> writerDelegator;
    
    @TempDir
    Path tempDir;
    
    @BeforeEach
    void setUp() {
        // Create a minimal model with a service
        model = Model.assembler()
                .addShape(ServiceShape.builder()
                        .id(TEST_SERVICE_ID)
                        .version("1.0")
                        .build())
                .assemble()
                .unwrap();
        
        settings = ErlangSettings.builder()
                .service(TEST_SERVICE_ID)
                .moduleName("test_service")
                .build();
        
        // Simple symbol provider for testing
        symbolProvider = new SymbolProvider() {
            @Override
            public Symbol toSymbol(Shape shape) {
                return Symbol.builder()
                        .name(shape.getId().getName())
                        .namespace("test", "/")
                        .build();
            }
        };
        
        fileManifest = FileManifest.create(tempDir);
        
        // Create writer delegator using the factory
        writerDelegator = new WriterDelegator<>(fileManifest, symbolProvider, ErlangWriter.factory());
    }
    
    // ========== Builder Tests ==========
    
    @Test
    @DisplayName("Builder creates context with all required fields")
    void testBuilderWithRequiredFields() {
        ErlangContext context = ErlangContext.builder()
                .model(model)
                .settings(settings)
                .symbolProvider(symbolProvider)
                .fileManifest(fileManifest)
                .writerDelegator(writerDelegator)
                .build();
        
        assertNotNull(context);
        assertEquals(model, context.model());
        assertEquals(settings, context.settings());
        assertEquals(symbolProvider, context.symbolProvider());
        assertEquals(fileManifest, context.fileManifest());
        assertEquals(writerDelegator, context.writerDelegator());
    }
    
    @Test
    @DisplayName("Builder creates context with optional integrations")
    void testBuilderWithIntegrations() {
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
    
    @Test
    @DisplayName("Builder defaults integrations to empty list")
    void testBuilderDefaultsIntegrations() {
        ErlangContext context = ErlangContext.builder()
                .model(model)
                .settings(settings)
                .symbolProvider(symbolProvider)
                .fileManifest(fileManifest)
                .writerDelegator(writerDelegator)
                .build();
        
        assertNotNull(context.integrations());
        assertTrue(context.integrations().isEmpty());
    }
    
    @Test
    @DisplayName("Builder throws when model is missing")
    void testBuilderRequiresModel() {
        assertThrows(NullPointerException.class, () -> {
            ErlangContext.builder()
                    .settings(settings)
                    .symbolProvider(symbolProvider)
                    .fileManifest(fileManifest)
                    .writerDelegator(writerDelegator)
                    .build();
        });
    }
    
    @Test
    @DisplayName("Builder throws when settings is missing")
    void testBuilderRequiresSettings() {
        assertThrows(NullPointerException.class, () -> {
            ErlangContext.builder()
                    .model(model)
                    .symbolProvider(symbolProvider)
                    .fileManifest(fileManifest)
                    .writerDelegator(writerDelegator)
                    .build();
        });
    }
    
    @Test
    @DisplayName("Builder throws when symbolProvider is missing")
    void testBuilderRequiresSymbolProvider() {
        assertThrows(NullPointerException.class, () -> {
            ErlangContext.builder()
                    .model(model)
                    .settings(settings)
                    .fileManifest(fileManifest)
                    .writerDelegator(writerDelegator)
                    .build();
        });
    }
    
    @Test
    @DisplayName("Builder throws when fileManifest is missing")
    void testBuilderRequiresFileManifest() {
        assertThrows(NullPointerException.class, () -> {
            ErlangContext.builder()
                    .model(model)
                    .settings(settings)
                    .symbolProvider(symbolProvider)
                    .writerDelegator(writerDelegator)
                    .build();
        });
    }
    
    @Test
    @DisplayName("Builder throws when writerDelegator is missing")
    void testBuilderRequiresWriterDelegator() {
        assertThrows(NullPointerException.class, () -> {
            ErlangContext.builder()
                    .model(model)
                    .settings(settings)
                    .symbolProvider(symbolProvider)
                    .fileManifest(fileManifest)
                    .build();
        });
    }
    
    // ========== Accessor Tests ==========
    
    @Test
    @DisplayName("model() returns the Smithy model")
    void testModelAccessor() {
        ErlangContext context = createContext();
        
        assertSame(model, context.model());
        assertTrue(context.model().getShape(TEST_SERVICE_ID).isPresent());
    }
    
    @Test
    @DisplayName("settings() returns the settings")
    void testSettingsAccessor() {
        ErlangContext context = createContext();
        
        assertSame(settings, context.settings());
        assertEquals("test_service", context.settings().moduleName());
    }
    
    @Test
    @DisplayName("symbolProvider() returns the symbol provider")
    void testSymbolProviderAccessor() {
        ErlangContext context = createContext();
        
        assertSame(symbolProvider, context.symbolProvider());
    }
    
    @Test
    @DisplayName("fileManifest() returns the file manifest")
    void testFileManifestAccessor() {
        ErlangContext context = createContext();
        
        assertSame(fileManifest, context.fileManifest());
    }
    
    // ========== Convenience Method Tests ==========
    
    @Test
    @DisplayName("service() returns the service ShapeId")
    void testServiceConvenience() {
        ErlangContext context = createContext();
        
        assertEquals(TEST_SERVICE_ID, context.service());
    }
    
    @Test
    @DisplayName("serviceShape() returns the service shape from model")
    void testServiceShapeConvenience() {
        ErlangContext context = createContext();
        
        ServiceShape service = context.serviceShape();
        
        assertNotNull(service);
        assertEquals(TEST_SERVICE_ID, service.getId());
        assertEquals("1.0", service.getVersion());
    }
    
    // ========== Integrations Tests ==========
    
    @Test
    @DisplayName("integrations() returns unmodifiable list")
    void testIntegrationsUnmodifiable() {
        List<ErlangIntegration> integrations = new ArrayList<>();
        integrations.add(new TestIntegration("Test"));
        
        ErlangContext context = ErlangContext.builder()
                .model(model)
                .settings(settings)
                .symbolProvider(symbolProvider)
                .fileManifest(fileManifest)
                .writerDelegator(writerDelegator)
                .integrations(integrations)
                .build();
        
        assertThrows(UnsupportedOperationException.class, () -> {
            context.integrations().add(new TestIntegration("Another"));
        });
    }
    
    @Test
    @DisplayName("integrations() is independent of original list")
    void testIntegrationsIndependent() {
        List<ErlangIntegration> integrations = new ArrayList<>();
        integrations.add(new TestIntegration("Original"));
        
        ErlangContext context = ErlangContext.builder()
                .model(model)
                .settings(settings)
                .symbolProvider(symbolProvider)
                .fileManifest(fileManifest)
                .writerDelegator(writerDelegator)
                .integrations(integrations)
                .build();
        
        // Modify original list
        integrations.add(new TestIntegration("Added"));
        
        // Context should not be affected
        assertEquals(1, context.integrations().size());
    }
    
    // ========== Integration Interface Tests ==========
    
    @Test
    @DisplayName("Integration default name returns class name")
    void testIntegrationDefaultName() {
        ErlangIntegration integration = new TestIntegration("CustomName");
        
        // Override returns custom name
        assertEquals("CustomName", integration.name());
    }
    
    @Test
    @DisplayName("Integration default priority is zero")
    void testIntegrationDefaultPriority() {
        ErlangIntegration integration = new ErlangIntegration() {};
        
        assertEquals(0, integration.priority());
    }
    
    @Test
    @DisplayName("Integration preprocessModel is no-op by default")
    void testIntegrationPreprocessModelDefault() {
        ErlangIntegration integration = new ErlangIntegration() {};
        ErlangContext context = createContext();
        
        // Should not throw
        integration.preprocessModel(context);
    }
    
    @Test
    @DisplayName("Integration postprocessGeneration is no-op by default")
    void testIntegrationPostprocessGenerationDefault() {
        ErlangIntegration integration = new ErlangIntegration() {};
        ErlangContext context = createContext();
        
        // Should not throw
        integration.postprocessGeneration(context);
    }
    
    // ========== Helper Methods ==========
    
    private ErlangContext createContext() {
        return ErlangContext.builder()
                .model(model)
                .settings(settings)
                .symbolProvider(symbolProvider)
                .fileManifest(fileManifest)
                .writerDelegator(writerDelegator)
                .build();
    }
    
    /**
     * Test integration implementation for unit tests.
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
}
