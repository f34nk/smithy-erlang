package io.smithy.erlang.codegen;

import software.amazon.smithy.build.PluginContext;
import software.amazon.smithy.build.SmithyBuildPlugin;
import software.amazon.smithy.codegen.core.directed.CodegenDirector;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.ObjectNode;

import java.util.logging.Logger;

/**
 * Smithy Build plugin for generating Erlang client code using DirectedCodegen.
 * 
 * <p>This plugin uses Smithy's {@link CodegenDirector} pattern for structured
 * code generation. It replaces the legacy {@link ErlangClientPlugin} with a
 * modern architecture that supports:
 * <ul>
 *   <li>Extensibility via {@link ErlangIntegration}</li>
 *   <li>Shape-specific generators</li>
 *   <li>Standard model transformations</li>
 *   <li>SPI-based integration discovery</li>
 * </ul>
 * 
 * <h2>Plugin Configuration</h2>
 * <p>Configure in {@code smithy-build.json}:
 * <pre>
 * {
 *   "plugins": {
 *     "erlang-codegen": {
 *       "service": "com.example#MyService",
 *       "module": "my_service",
 *       "outputDir": "src/generated",
 *       "protocol": "aws.protocols#restJson1"
 *     }
 *   }
 * }
 * </pre>
 * 
 * <h2>Plugin Name</h2>
 * <p>This plugin is registered as {@code "erlang-codegen"}. The legacy plugin
 * {@code "erlang-client-codegen"} remains available for backward compatibility.
 * 
 * @see ErlangGenerator
 * @see ErlangIntegration
 * @see ErlangContext
 */
public final class ErlangCodegenPlugin implements SmithyBuildPlugin {
    
    private static final Logger LOGGER = Logger.getLogger(ErlangCodegenPlugin.class.getName());
    
    /**
     * Gets the name of this plugin.
     * 
     * <p>This plugin is registered as "erlang-codegen" to distinguish it from
     * the legacy "erlang-client-codegen" plugin.
     *
     * @return The plugin name
     */
    @Override
    public String getName() {
        return "erlang-codegen";
    }
    
    /**
     * Executes the Erlang code generation.
     * 
     * <p>This method sets up and runs the {@link CodegenDirector} with:
     * <ul>
     *   <li>{@link ErlangGenerator} as the directed codegen implementation</li>
     *   <li>{@link ErlangIntegration} for SPI discovery</li>
     *   <li>Model transformations for better code generation</li>
     * </ul>
     *
     * @param pluginContext The plugin context from Smithy build
     */
    @Override
    public void execute(PluginContext pluginContext) {
        LOGGER.info("Executing Erlang code generation (DirectedCodegen)");
        
        Model model = pluginContext.getModel();
        ObjectNode settingsNode = pluginContext.getSettings();
        
        // Create settings from plugin configuration
        ErlangSettings settings = ErlangSettings.from(settingsNode);
        
        LOGGER.info("Generating Erlang client for service: " + settings.service());
        LOGGER.info("Output directory: " + settings.outputDir());
        if (settings.moduleName() != null) {
            LOGGER.info("Module name: " + settings.moduleName());
        }
        
        // Create and configure the CodegenDirector
        CodegenDirector<ErlangWriter, ErlangIntegration, ErlangContext, ErlangSettings> director = 
                new CodegenDirector<>();
        
        // Set the DirectedCodegen implementation
        director.directedCodegen(new ErlangGenerator());
        
        // Set the SmithyIntegration class for SPI discovery
        director.integrationClass(ErlangIntegration.class);
        
        // Set context from plugin
        director.fileManifest(pluginContext.getFileManifest());
        director.model(model);
        director.settings(settings);
        director.service(settings.service());
        
        // Apply standard model transformations
        // This applies transforms like flattening namespaces and removing deprecated shapes
        director.performDefaultCodegenTransforms();
        
        // Sort members for deterministic output
        director.sortMembers();
        
        // Run the generator
        director.run();
        
        LOGGER.info("Erlang code generation completed successfully");
    }
}
