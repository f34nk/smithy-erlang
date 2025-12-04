package io.smithy.erlang.codegen;

import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Context object for Erlang code generation.
 * 
 * <p>This class provides a central access point for all code generation 
 * dependencies, following the Smithy Development Guide recommendations.
 * 
 * <p>Note: Full {@code CodegenContext} interface compliance will be added
 * in Step 3.1 when {@code ErlangWriter} is enhanced to extend 
 * {@code SymbolWriter}.
 * 
 * <p>The context is typically created by the {@code ErlangGenerator} during
 * the code generation process and passed to all shape generators and integrations.
 * 
 * <p>Example usage:
 * <pre>
 * ErlangContext context = ErlangContext.builder()
 *     .model(model)
 *     .settings(settings)
 *     .symbolProvider(symbolProvider)
 *     .fileManifest(fileManifest)
 *     .build();
 * 
 * // Access components
 * Model model = context.model();
 * ErlangSettings settings = context.settings();
 * </pre>
 * 
 * @see ErlangSettings
 * @see ErlangIntegration
 */
public final class ErlangContext {
    
    private final Model model;
    private final ErlangSettings settings;
    private final SymbolProvider symbolProvider;
    private final FileManifest fileManifest;
    private final List<ErlangIntegration> integrations;
    
    private ErlangContext(Builder builder) {
        this.model = Objects.requireNonNull(builder.model, "model is required");
        this.settings = Objects.requireNonNull(builder.settings, "settings is required");
        this.symbolProvider = Objects.requireNonNull(builder.symbolProvider, "symbolProvider is required");
        this.fileManifest = Objects.requireNonNull(builder.fileManifest, "fileManifest is required");
        this.integrations = builder.integrations != null 
                ? Collections.unmodifiableList(new java.util.ArrayList<>(builder.integrations)) 
                : Collections.emptyList();
    }
    
    /**
     * Gets the Smithy model being generated.
     *
     * @return The model (never null)
     */
    public Model model() {
        return model;
    }
    
    /**
     * Gets the code generation settings.
     *
     * @return The settings (never null)
     */
    public ErlangSettings settings() {
        return settings;
    }
    
    /**
     * Gets the symbol provider for mapping shapes to symbols.
     *
     * @return The symbol provider (never null)
     */
    public SymbolProvider symbolProvider() {
        return symbolProvider;
    }
    
    /**
     * Gets the file manifest for writing generated files.
     *
     * @return The file manifest (never null)
     */
    public FileManifest fileManifest() {
        return fileManifest;
    }
    
    /**
     * Gets the list of active integrations.
     * 
     * <p>Integrations are discovered via SPI and can customize
     * the code generation process.
     *
     * @return Unmodifiable list of integrations (never null, may be empty)
     */
    public List<ErlangIntegration> integrations() {
        return integrations;
    }
    
    /**
     * Gets the service shape being generated.
     * 
     * <p>This is a convenience method that looks up the service shape
     * from the model using the service ID from settings.
     *
     * @return The service shape
     * @throws software.amazon.smithy.model.shapes.ShapeNotFoundException if service not found
     */
    public ServiceShape serviceShape() {
        return model.expectShape(settings.service(), ServiceShape.class);
    }
    
    /**
     * Gets the service shape ID being generated.
     * 
     * <p>This is a convenience method that returns the service ID from settings.
     *
     * @return The service shape ID
     */
    public ShapeId service() {
        return settings.service();
    }
    
    /**
     * Creates a new builder for ErlangContext.
     *
     * @return A new builder instance
     */
    public static Builder builder() {
        return new Builder();
    }
    
    /**
     * Builder for creating ErlangContext instances.
     */
    public static final class Builder {
        private Model model;
        private ErlangSettings settings;
        private SymbolProvider symbolProvider;
        private FileManifest fileManifest;
        private List<ErlangIntegration> integrations;
        
        private Builder() {}
        
        /**
         * Sets the Smithy model.
         *
         * @param model The model (required)
         * @return This builder
         */
        public Builder model(Model model) {
            this.model = model;
            return this;
        }
        
        /**
         * Sets the code generation settings.
         *
         * @param settings The settings (required)
         * @return This builder
         */
        public Builder settings(ErlangSettings settings) {
            this.settings = settings;
            return this;
        }
        
        /**
         * Sets the symbol provider.
         *
         * @param symbolProvider The symbol provider (required)
         * @return This builder
         */
        public Builder symbolProvider(SymbolProvider symbolProvider) {
            this.symbolProvider = symbolProvider;
            return this;
        }
        
        /**
         * Sets the file manifest.
         *
         * @param fileManifest The file manifest (required)
         * @return This builder
         */
        public Builder fileManifest(FileManifest fileManifest) {
            this.fileManifest = fileManifest;
            return this;
        }
        
        /**
         * Sets the list of integrations.
         *
         * @param integrations The integrations (optional)
         * @return This builder
         */
        public Builder integrations(List<ErlangIntegration> integrations) {
            this.integrations = integrations;
            return this;
        }
        
        /**
         * Builds the ErlangContext instance.
         *
         * @return The built context
         * @throws NullPointerException if required fields are not set
         */
        public ErlangContext build() {
            return new ErlangContext(this);
        }
    }
}
