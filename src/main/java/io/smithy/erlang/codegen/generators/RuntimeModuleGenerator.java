package io.smithy.erlang.codegen.generators;

import io.smithy.erlang.codegen.ErlangContext;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Generates (copies) runtime support modules.
 * 
 * <p>Runtime modules are pre-written Erlang modules that provide
 * common functionality needed by generated clients:
 * <ul>
 *   <li>{@code aws_sigv4.erl} - AWS Signature Version 4 signing</li>
 *   <li>{@code aws_credentials.erl} - Credential provider chain</li>
 *   <li>{@code aws_config.erl} - AWS configuration (region, etc.)</li>
 *   <li>{@code aws_retry.erl} - Retry logic with exponential backoff</li>
 *   <li>{@code aws_xml.erl} - XML encoding/decoding</li>
 *   <li>{@code aws_query.erl} - Query protocol encoding</li>
 *   <li>{@code aws_s3.erl} - S3-specific helpers</li>
 *   <li>{@code aws_endpoints.erl} - Endpoint resolution</li>
 * </ul>
 * 
 * <p>These modules are bundled as resources and copied to the output
 * directory during code generation.
 * 
 * <p>Example usage:
 * <pre>
 * RuntimeModuleGenerator.copyModule("aws_sigv4.erl", context);
 * </pre>
 * 
 * @see ErlangContext
 */
public final class RuntimeModuleGenerator {
    
    private static final Logger LOGGER = Logger.getLogger(RuntimeModuleGenerator.class.getName());
    
    /** Resource path prefix for runtime modules. */
    private static final String RESOURCE_PREFIX = "/";
    
    /** List of available runtime modules. */
    public static final String[] AVAILABLE_MODULES = {
        "aws_sigv4.erl",
        "aws_credentials.erl",
        "aws_config.erl",
        "aws_retry.erl",
        "aws_xml.erl",
        "aws_query.erl",
        "aws_s3.erl",
        "aws_endpoints.erl"
    };
    
    private final ErlangContext context;
    
    /**
     * Creates a new runtime module generator.
     *
     * @param context The code generation context
     */
    public RuntimeModuleGenerator(ErlangContext context) {
        this.context = Objects.requireNonNull(context, "context is required");
    }
    
    /**
     * Gets the code generation context.
     *
     * @return The context
     */
    public ErlangContext getContext() {
        return context;
    }
    
    /**
     * Copies all runtime modules to the output directory.
     * 
     * <p>This method copies all available runtime modules. Use
     * {@link #copyModule(String)} if you only need specific modules.
     */
    public void generateAll() {
        for (String module : AVAILABLE_MODULES) {
            copyModule(module);
        }
    }
    
    /**
     * Copies a specific runtime module to the output directory.
     *
     * @param moduleName The module name (e.g., "aws_sigv4.erl")
     * @return true if the module was copied successfully
     */
    public boolean copyModule(String moduleName) {
        return copyModule(moduleName, context);
    }
    
    /**
     * Static method to copy a runtime module.
     *
     * @param moduleName The module name (e.g., "aws_sigv4.erl")
     * @param context The code generation context
     * @return true if the module was copied successfully
     */
    public static boolean copyModule(String moduleName, ErlangContext context) {
        Objects.requireNonNull(moduleName, "moduleName is required");
        Objects.requireNonNull(context, "context is required");
        
        LOGGER.fine("Copying runtime module: " + moduleName);
        
        String resourcePath = RESOURCE_PREFIX + moduleName;
        
        try (InputStream is = RuntimeModuleGenerator.class.getResourceAsStream(resourcePath)) {
            if (is == null) {
                LOGGER.warning("Runtime module not found in resources: " + moduleName);
                return false;
            }
            
            String content = new String(is.readAllBytes(), StandardCharsets.UTF_8);
            
            // Write to output using file manifest
            context.fileManifest().writeFile(moduleName, content);
            
            LOGGER.fine("Copied runtime module: " + moduleName);
            return true;
            
        } catch (IOException e) {
            LOGGER.log(Level.WARNING, "Failed to copy runtime module: " + moduleName, e);
            return false;
        }
    }
    
    /**
     * Copies a runtime module to a specific output directory.
     *
     * @param moduleName The module name (e.g., "aws_sigv4.erl")
     * @param outputDir The output directory path
     * @return true if the module was copied successfully
     */
    public static boolean copyModuleToDir(String moduleName, Path outputDir) {
        Objects.requireNonNull(moduleName, "moduleName is required");
        Objects.requireNonNull(outputDir, "outputDir is required");
        
        LOGGER.fine("Copying runtime module " + moduleName + " to " + outputDir);
        
        String resourcePath = RESOURCE_PREFIX + moduleName;
        
        try (InputStream is = RuntimeModuleGenerator.class.getResourceAsStream(resourcePath)) {
            if (is == null) {
                LOGGER.warning("Runtime module not found in resources: " + moduleName);
                return false;
            }
            
            // Ensure output directory exists
            Files.createDirectories(outputDir);
            
            Path outputPath = outputDir.resolve(moduleName);
            Files.write(outputPath, is.readAllBytes());
            
            LOGGER.fine("Copied runtime module: " + moduleName + " -> " + outputPath);
            return true;
            
        } catch (IOException e) {
            LOGGER.log(Level.WARNING, "Failed to copy runtime module: " + moduleName, e);
            return false;
        }
    }
    
    /**
     * Checks if a runtime module is available.
     *
     * @param moduleName The module name to check
     * @return true if the module exists in resources
     */
    public static boolean isModuleAvailable(String moduleName) {
        String resourcePath = RESOURCE_PREFIX + moduleName;
        return RuntimeModuleGenerator.class.getResourceAsStream(resourcePath) != null;
    }
    
    /**
     * Gets the list of available runtime modules.
     *
     * @return Array of module names
     */
    public static String[] getAvailableModules() {
        return AVAILABLE_MODULES.clone();
    }
}
