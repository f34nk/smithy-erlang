package io.smithy.erlang.codegen;

/**
 * Integration interface for extending Erlang code generation.
 * 
 * <p>Implementations of this interface can customize the code generation
 * process by hooking into various extension points. Integrations are
 * discovered via Java SPI (Service Provider Interface).
 * 
 * <p>Note: Full {@code SmithyIntegration} interface compliance will be added
 * in Step 3.1 when {@code ErlangWriter} is enhanced to extend 
 * {@code SymbolWriter}.
 * 
 * <p>To create a custom integration:
 * <ol>
 *   <li>Implement this interface</li>
 *   <li>Register the implementation in META-INF/services/io.smithy.erlang.codegen.ErlangIntegration</li>
 * </ol>
 * 
 * <p>Example:
 * <pre>
 * public class MyIntegration implements ErlangIntegration {
 *     {@literal @}Override
 *     public String name() {
 *         return "MyIntegration";
 *     }
 *     
 *     {@literal @}Override
 *     public byte priority() {
 *         return 0; // Default priority
 *     }
 * }
 * </pre>
 */
public interface ErlangIntegration {
    
    /**
     * Gets the name of this integration.
     * 
     * <p>The name is used for logging and debugging purposes.
     * By default, returns the canonical class name.
     *
     * @return Integration name
     */
    default String name() {
        return getClass().getCanonicalName();
    }
    
    /**
     * Gets the priority of this integration.
     * 
     * <p>Higher priority integrations run first. The default priority is 0.
     * Use positive values for integrations that should run early, and
     * negative values for integrations that should run late.
     *
     * @return Priority value (higher = runs first)
     */
    default byte priority() {
        return 0;
    }
    
    /**
     * Called before code generation begins.
     * 
     * <p>This hook can be used to modify the model, settings, or perform
     * any setup needed before generation.
     *
     * @param context The code generation context
     */
    default void preprocessModel(ErlangContext context) {
        // Default: no-op
    }
    
    /**
     * Called after code generation completes.
     * 
     * <p>This hook can be used to perform any cleanup or post-processing
     * after all code has been generated.
     *
     * @param context The code generation context
     */
    default void postprocessGeneration(ErlangContext context) {
        // Default: no-op
    }
}
