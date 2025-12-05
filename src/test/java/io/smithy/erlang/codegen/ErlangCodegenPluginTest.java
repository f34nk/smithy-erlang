package io.smithy.erlang.codegen;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.build.MockManifest;
import software.amazon.smithy.build.PluginContext;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.node.ObjectNode;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.shapes.StructureShape;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for ErlangCodegenPlugin.
 */
class ErlangCodegenPluginTest {
    
    private static final ShapeId TEST_SERVICE_ID = ShapeId.from("com.example#TestService");
    private static final ShapeId TEST_OPERATION_ID = ShapeId.from("com.example#TestOperation");
    private static final ShapeId TEST_INPUT_ID = ShapeId.from("com.example#TestInput");
    private static final ShapeId TEST_OUTPUT_ID = ShapeId.from("com.example#TestOutput");
    
    private ErlangCodegenPlugin plugin;
    private Model model;
    
    @TempDir
    Path tempDir;
    
    @BeforeEach
    void setUp() {
        plugin = new ErlangCodegenPlugin();
        model = createTestModel();
    }
    
    private Model createTestModel() {
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
    
    // ========== Plugin Name Tests ==========
    
    @Nested
    @DisplayName("Plugin Name")
    class PluginNameTests {
        
        @Test
        @DisplayName("Plugin name is erlang-codegen")
        void testPluginName() {
            assertEquals("erlang-codegen", plugin.getName());
        }
        
    }
    
    // ========== Plugin Instantiation Tests ==========
    
    @Nested
    @DisplayName("Plugin Instantiation")
    class InstantiationTests {
        
        @Test
        @DisplayName("Plugin can be instantiated")
        void testInstantiation() {
            assertNotNull(plugin);
        }
        
        @Test
        @DisplayName("Multiple instances are independent")
        void testMultipleInstances() {
            ErlangCodegenPlugin plugin1 = new ErlangCodegenPlugin();
            ErlangCodegenPlugin plugin2 = new ErlangCodegenPlugin();
            
            assertNotSame(plugin1, plugin2);
            assertEquals(plugin1.getName(), plugin2.getName());
        }
    }
    
    // ========== Settings Tests ==========
    
    @Nested
    @DisplayName("Settings Configuration")
    class SettingsTests {
        
        @Test
        @DisplayName("Creates ErlangSettings from plugin configuration")
        void testSettingsCreation() {
            ObjectNode settingsNode = Node.objectNodeBuilder()
                    .withMember("service", "com.example#TestService")
                    .withMember("module", "test_module")
                    .withMember("outputDir", "src/gen")
                    .build();
            
            ErlangSettings settings = ErlangSettings.from(settingsNode);
            
            assertEquals(TEST_SERVICE_ID, settings.service());
            assertEquals("test_module", settings.moduleName());
            assertEquals("src/gen", settings.outputDir());
        }
        
        @Test
        @DisplayName("Settings with defaults")
        void testSettingsDefaults() {
            ObjectNode settingsNode = Node.objectNodeBuilder()
                    .withMember("service", "com.example#TestService")
                    .build();
            
            ErlangSettings settings = ErlangSettings.from(settingsNode);
            
            assertEquals(TEST_SERVICE_ID, settings.service());
            assertNull(settings.moduleName());
            assertEquals("src/generated", settings.outputDir()); // default
        }
    }
    
    // ========== Integration Tests ==========
    
    @Nested
    @DisplayName("Plugin Execution")
    class ExecutionTests {
        
        @Test
        @DisplayName("Plugin executes without throwing")
        void testExecutionDoesNotThrow() {
            ObjectNode settingsNode = Node.objectNodeBuilder()
                    .withMember("service", "com.example#TestService")
                    .withMember("module", "test_service")
                    .build();
            
            MockManifest manifest = new MockManifest();
            
            PluginContext context = PluginContext.builder()
                    .model(model)
                    .fileManifest(manifest)
                    .settings(settingsNode)
                    .build();
            
            // Should not throw
            assertDoesNotThrow(() -> plugin.execute(context));
        }
    }
    
}
