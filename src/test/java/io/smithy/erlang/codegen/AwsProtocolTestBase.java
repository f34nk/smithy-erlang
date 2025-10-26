package io.smithy.erlang.codegen;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import software.amazon.smithy.build.PluginContext;
import software.amazon.smithy.build.MockManifest;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.node.ObjectNode;

import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Base class for AWS protocol tests.
 * Provides common setup and utilities for testing AWS-specific features.
 */
public abstract class AwsProtocolTestBase {
    
    protected Model model;
    protected MockManifest manifest;
    protected PluginContext context;
    
    /**
     * Get the path to the test model file.
     * Subclasses should override this to specify their test model.
     */
    protected abstract String getModelPath();
    
    /**
     * Get the service shape ID for testing.
     * Default implementation can be overridden by subclasses.
     */
    protected String getServiceShapeId() {
        return "com.example.s3.test#S3Test";
    }
    
    /**
     * Get the module name for generated code.
     * Default implementation can be overridden by subclasses.
     */
    protected String getModuleName() {
        return "s3_test_client";
    }
    
    @BeforeEach
    public void setUp() {
        // Load the test model
        String modelPath = getModelPath();
        assertNotNull(modelPath, "Model path must not be null");
        
        try {
            model = Model.assembler()
                    .discoverModels(getClass().getClassLoader())  // Discover AWS traits from classpath
                    .addImport(getClass().getResource(modelPath))
                    .assemble()
                    .unwrap();
            
            assertNotNull(model, "Model should be loaded");
        } catch (Exception e) {
            System.err.println("Failed to load model: " + e.getMessage());
            if (e.getCause() != null) {
                System.err.println("Cause: " + e.getCause().getMessage());
            }
            throw e;
        }
        
        // Create mock manifest for capturing generated code
        manifest = new MockManifest();
        
        // Create plugin settings
        ObjectNode settings = Node.objectNodeBuilder()
                .withMember("service", getServiceShapeId())
                .withMember("module", getModuleName())
                .build();
        
        // Create plugin context
        context = PluginContext.builder()
                .model(model)
                .fileManifest(manifest)
                .settings(settings)
                .build();
    }
    
    /**
     * Run the code generator with the test model.
     */
    protected void runGenerator() {
        ErlangClientPlugin plugin = new ErlangClientPlugin();
        plugin.execute(context);
    }
    
    /**
     * Get the generated content of a file.
     */
    protected String getGeneratedFile(String filename) {
        return manifest.getFileString(filename).orElse(null);
    }
    
    /**
     * Assert that a file was generated.
     */
    protected void assertFileGenerated(String filename) {
        assertTrue(manifest.hasFile(filename),
                "File should be generated: " + filename);
    }
    
    /**
     * Assert that generated code contains a specific string.
     */
    protected void assertGeneratedCodeContains(String filename, String expected) {
        String content = getGeneratedFile(filename);
        assertNotNull(content, "File should exist: " + filename);
        assertTrue(content.contains(expected),
                "Generated code should contain: " + expected + "\nBut file " + filename + " contains:\n" + content);
    }
    
    /**
     * Assert that generated code does not contain a specific string.
     */
    protected void assertGeneratedCodeNotContains(String filename, String unexpected) {
        String content = getGeneratedFile(filename);
        assertNotNull(content, "File should exist: " + filename);
        assertFalse(content.contains(unexpected),
                "Generated code should not contain: " + unexpected);
    }
    
    @Test
    public void testModelLoads() {
        assertNotNull(model, "Model should load successfully");
        assertTrue(model.getShapeIds().size() > 0, "Model should contain shapes");
    }
    
    // NOTE: Generator tests are commented out until protocol support is complete
    // Subclasses can uncomment and use these when needed
    
    /*
    @Test
    public void testGeneratorRuns() {
        assertDoesNotThrow(() -> runGenerator(), 
                "Generator should run without exceptions");
    }
    
    @Test
    public void testClientFileGenerated() {
        runGenerator();
        String clientFile = getModuleName() + ".erl";
        assertFileGenerated(clientFile);
    }
    
    @Test
    public void testTypesFileGenerated() {
        runGenerator();
        String typesFile = getModuleName() + "_types.erl";
        assertFileGenerated(typesFile);
    }
    */
}
