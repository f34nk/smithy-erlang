package io.smithy.erlang.codegen;

import software.amazon.smithy.codegen.core.SmithyIntegration;

/**
 * Integration interface for extending Erlang code generation.
 * 
 * <p>This interface extends Smithy's {@link SmithyIntegration} to provide
 * type-safe integration points for Erlang code generation. Implementations
 * can customize the code generation process by hooking into various extension
 * points.
 * 
 * <p>Integrations are discovered via Java SPI (Service Provider Interface).
 * To create a custom integration:
 * <ol>
 *   <li>Implement this interface</li>
 *   <li>Register the implementation in 
 *       {@code META-INF/services/io.smithy.erlang.codegen.ErlangIntegration}</li>
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
 * 
 * <h2>Available Extension Points</h2>
 * <ul>
 *   <li>{@link #name()} - Returns the integration name for logging</li>
 *   <li>{@link #priority()} - Controls integration execution order</li>
 *   <li>{@link #preprocessModel(ErlangContext)} - Called before generation</li>
 *   <li>{@link #postprocessGeneration(ErlangContext)} - Called after generation</li>
 * </ul>
 * 
 * @see SmithyIntegration
 * @see ErlangContext
 * @see ErlangSettings
 */
public interface ErlangIntegration 
        extends SmithyIntegration<ErlangSettings, ErlangWriter, ErlangContext> {
    
    /**
     * Gets the name of this integration.
     * 
     * <p>The name is used for logging and debugging purposes.
     * By default, returns the canonical class name.
     *
     * @return Integration name
     */
    @Override
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
    @Override
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
