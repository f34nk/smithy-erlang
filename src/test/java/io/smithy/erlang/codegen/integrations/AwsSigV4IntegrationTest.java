package io.smithy.erlang.codegen.integrations;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangIntegration;
import io.smithy.erlang.codegen.ErlangSettings;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import software.amazon.smithy.build.MockManifest;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.codegen.core.WriterDelegator;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.node.ObjectNode;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

import java.util.List;
import java.util.Optional;
import java.util.ServiceLoader;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for AwsSigV4Integration.
 */
class AwsSigV4IntegrationTest {
    
    private static final ShapeId SERVICE_WITH_SIGV4 = ShapeId.from("com.example#SigV4Service");
    private static final ShapeId SERVICE_WITHOUT_SIGV4 = ShapeId.from("com.example#PlainService");
    
    private AwsSigV4Integration integration;
    
    @BeforeEach
    void setUp() {
        integration = new AwsSigV4Integration();
    }
    
    // ========== Basic Tests ==========
    
    @Nested
    @DisplayName("Integration Identity")
    class IntegrationIdentityTests {
        
        @Test
        @DisplayName("Has correct name")
        void testName() {
            assertEquals("AwsSigV4Integration", integration.name());
        }
        
        @Test
        @DisplayName("Has high priority")
        void testPriority() {
            assertEquals(64, integration.priority());
        }
        
        @Test
        @DisplayName("Implements ErlangIntegration")
        void testImplementsInterface() {
            assertTrue(integration instanceof ErlangIntegration);
        }
    }
    
    // ========== SigV4 Trait Detection Tests ==========
    
    @Nested
    @DisplayName("SigV4 Trait Detection")
    class SigV4TraitDetectionTests {
        
        @Test
        @DisplayName("Detects SigV4 trait on service")
        void testDetectsSigV4Trait() {
            Model model = createModelWithSigV4Service();
            ServiceShape service = model.expectShape(SERVICE_WITH_SIGV4, ServiceShape.class);
            
            assertTrue(integration.hasSigV4Trait(service));
        }
        
        @Test
        @DisplayName("Does not detect SigV4 on plain service")
        void testNoSigV4OnPlainService() {
            Model model = createPlainServiceModel();
            ServiceShape service = model.expectShape(SERVICE_WITHOUT_SIGV4, ServiceShape.class);
            
            assertFalse(integration.hasSigV4Trait(service));
        }
        
        @Test
        @DisplayName("Extracts signing name from trait")
        void testExtractsSigningName() {
            Model model = createModelWithSigV4Service();
            ServiceShape service = model.expectShape(SERVICE_WITH_SIGV4, ServiceShape.class);
            
            Optional<String> signingName = integration.getSigningName(service);
            
            assertTrue(signingName.isPresent());
            assertEquals("myservice", signingName.get());
        }
    }
    
    // ========== Preprocessing Tests ==========
    
    @Nested
    @DisplayName("Model Preprocessing")
    class PreprocessingTests {
        
        @Test
        @DisplayName("Copies modules for SigV4 service")
        void testCopiesModulesForSigV4Service() {
            Model model = createModelWithSigV4Service();
            MockManifest manifest = new MockManifest();
            ErlangContext context = createContext(model, SERVICE_WITH_SIGV4, manifest);
            
            // Run preprocessing
            integration.preprocessModel(context);
            
            // Check that modules were written
            assertTrue(manifest.hasFile("aws_sigv4.erl") || manifest.getFiles().size() > 0,
                    "Should attempt to copy modules");
        }
        
        @Test
        @DisplayName("Does not throw for plain service")
        void testNoThrowForPlainService() {
            Model model = createPlainServiceModel();
            MockManifest manifest = new MockManifest();
            ErlangContext context = createContext(model, SERVICE_WITHOUT_SIGV4, manifest);
            
            // Should not throw
            assertDoesNotThrow(() -> integration.preprocessModel(context));
        }
    }
    
    // ========== SPI Discovery Tests ==========
    
    @Nested
    @DisplayName("SPI Discovery")
    class SpiDiscoveryTests {
        
        @Test
        @DisplayName("Can be loaded via ServiceLoader")
        void testServiceLoaderDiscovery() {
            ServiceLoader<ErlangIntegration> loader = ServiceLoader.load(ErlangIntegration.class);
            
            boolean found = false;
            for (ErlangIntegration loadedIntegration : loader) {
                if (loadedIntegration instanceof AwsSigV4Integration) {
                    found = true;
                    break;
                }
            }
            
            assertTrue(found, "AwsSigV4Integration should be discoverable via SPI");
        }
    }
    
    // ========== Helper Methods ==========
    
    /**
     * Creates a model with a service that has the aws.auth#sigv4 trait.
     */
    private Model createModelWithSigV4Service() {
        // Build the model from a Smithy IDL string with the sigv4 trait
        String modelString = 
            "$version: \"2.0\"\n" +
            "\n" +
            "metadata validators = []\n" +
            "\n" +
            "namespace com.example\n" +
            "\n" +
            "use aws.auth#sigv4\n" +
            "\n" +
            "@sigv4(name: \"myservice\")\n" +
            "service SigV4Service {\n" +
            "    version: \"1.0\"\n" +
            "}\n";
        
        return Model.assembler()
                .addUnparsedModel("test.smithy", modelString)
                .discoverModels()
                .assemble()
                .unwrap();
    }
    
    /**
     * Creates a model with a plain service (no SigV4).
     */
    private Model createPlainServiceModel() {
        ServiceShape service = ServiceShape.builder()
                .id(SERVICE_WITHOUT_SIGV4)
                .version("1.0")
                .build();
        
        return Model.assembler()
                .addShape(service)
                .assemble()
                .unwrap();
    }
    
    /**
     * Creates an ErlangContext for testing.
     */
    private ErlangContext createContext(Model model, ShapeId serviceId, MockManifest manifest) {
        ErlangSettings settings = ErlangSettings.builder()
                .service(serviceId)
                .moduleName("test_module")
                .build();
        
        SymbolProvider symbolProvider = new EnhancedErlangSymbolProvider(model, settings);
        
        WriterDelegator<ErlangWriter> delegator = new WriterDelegator<>(
                manifest,
                symbolProvider,
                (filename, namespace) -> new ErlangWriter("test")
        );
        
        return ErlangContext.builder()
                .model(model)
                .settings(settings)
                .symbolProvider(symbolProvider)
                .fileManifest(manifest)
                .integrations(List.of())
                .writerDelegator(delegator)
                .build();
    }
}
