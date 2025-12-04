package io.smithy.erlang.codegen;

import software.amazon.smithy.model.node.ObjectNode;
import software.amazon.smithy.model.shapes.ShapeId;

import java.util.Objects;
import java.util.Optional;

/**
 * Settings for the Erlang code generator.
 * 
 * <p>This class follows the Smithy Development Guide recommendations for
 * code generator settings, providing a clean API with immutable configuration.
 * 
 * <p>Settings can be created from a smithy-build.json plugin configuration:
 * <pre>
 * {
 *   "plugins": {
 *     "erlang-client-codegen": {
 *       "service": "com.example#MyService",
 *       "module": "my_service",
 *       "outputDir": "src/generated",
 *       "protocol": "aws.protocols#restJson1",
 *       "edition": "2025"
 *     }
 *   }
 * }
 * </pre>
 * 
 * @see ErlangClientSettings (deprecated, use this class instead)
 */
public final class ErlangSettings {
    
    private static final String DEFAULT_OUTPUT_DIR = "src/generated";
    private static final String DEFAULT_EDITION = "2025";
    
    private final ShapeId service;
    private final String moduleName;
    private final String outputDir;
    private final String protocol;
    private final String edition;
    
    private ErlangSettings(Builder builder) {
        this.service = Objects.requireNonNull(builder.service, "service is required");
        this.moduleName = builder.moduleName;
        this.outputDir = builder.outputDir != null ? builder.outputDir : DEFAULT_OUTPUT_DIR;
        this.protocol = builder.protocol;
        this.edition = builder.edition != null ? builder.edition : DEFAULT_EDITION;
    }
    
    /**
     * Creates settings from a configuration ObjectNode.
     * 
     * <p>This factory method parses the plugin configuration from smithy-build.json.
     *
     * @param node Configuration object from smithy-build.json
     * @return ErlangSettings instance
     * @throws software.amazon.smithy.model.node.ExpectationNotMetException if required fields are missing
     */
    public static ErlangSettings from(ObjectNode node) {
        Builder builder = builder();
        
        // Required field - will throw if missing
        node.getStringMember("service")
                .map(n -> ShapeId.from(n.getValue()))
                .ifPresent(builder::service);
        
        // Optional fields with defaults
        node.getStringMember("module")
                .map(n -> n.getValue())
                .ifPresent(builder::moduleName);
        
        node.getStringMember("outputDir")
                .map(n -> n.getValue())
                .ifPresent(builder::outputDir);
        
        node.getStringMember("protocol")
                .map(n -> n.getValue())
                .ifPresent(builder::protocol);
        
        node.getStringMember("edition")
                .map(n -> n.getValue())
                .ifPresent(builder::edition);
        
        return builder.build();
    }
    
    /**
     * Gets the service shape ID to generate a client for.
     *
     * @return The service shape ID (never null)
     */
    public ShapeId service() {
        return service;
    }
    
    /**
     * Gets the Erlang module name for the generated client.
     * 
     * <p>If not set, the module name should be derived from the service name.
     *
     * @return The module name, or null if not explicitly set
     */
    public String moduleName() {
        return moduleName;
    }
    
    /**
     * Gets the output directory for generated files.
     *
     * @return The output directory (defaults to "src/generated")
     */
    public String outputDir() {
        return outputDir;
    }
    
    /**
     * Gets the protocol to use for code generation.
     * 
     * <p>If not set, the protocol should be auto-detected from service traits.
     *
     * @return The protocol shape ID as a string, or null for auto-detection
     */
    public String protocol() {
        return protocol;
    }
    
    /**
     * Gets the Smithy edition being used.
     *
     * @return The edition (defaults to "2025")
     */
    public String edition() {
        return edition;
    }
    
    /**
     * Gets the protocol as an Optional.
     * 
     * <p>This method is provided for API compatibility with code that
     * prefers Optional handling of nullable values.
     *
     * @return Optional containing the protocol, or empty if not set
     */
    public Optional<String> getProtocol() {
        return Optional.ofNullable(protocol);
    }
    
    /**
     * Creates a new builder for ErlangSettings.
     *
     * @return A new builder instance
     */
    public static Builder builder() {
        return new Builder();
    }
    
    /**
     * Creates a builder initialized with values from this settings instance.
     *
     * @return A new builder with copied values
     */
    public Builder toBuilder() {
        return builder()
                .service(service)
                .moduleName(moduleName)
                .outputDir(outputDir)
                .protocol(protocol)
                .edition(edition);
    }
    
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ErlangSettings that = (ErlangSettings) o;
        return Objects.equals(service, that.service) &&
               Objects.equals(moduleName, that.moduleName) &&
               Objects.equals(outputDir, that.outputDir) &&
               Objects.equals(protocol, that.protocol) &&
               Objects.equals(edition, that.edition);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(service, moduleName, outputDir, protocol, edition);
    }
    
    @Override
    public String toString() {
        return "ErlangSettings{" +
               "service=" + service +
               ", moduleName='" + moduleName + '\'' +
               ", outputDir='" + outputDir + '\'' +
               ", protocol='" + protocol + '\'' +
               ", edition='" + edition + '\'' +
               '}';
    }
    
    /**
     * Builder for creating ErlangSettings instances.
     */
    public static final class Builder {
        private ShapeId service;
        private String moduleName;
        private String outputDir;
        private String protocol;
        private String edition;
        
        private Builder() {}
        
        /**
         * Sets the service shape ID.
         *
         * @param service The service shape ID (required)
         * @return This builder
         */
        public Builder service(ShapeId service) {
            this.service = service;
            return this;
        }
        
        /**
         * Sets the Erlang module name.
         *
         * @param moduleName The module name
         * @return This builder
         */
        public Builder moduleName(String moduleName) {
            this.moduleName = moduleName;
            return this;
        }
        
        /**
         * Sets the output directory.
         *
         * @param outputDir The output directory
         * @return This builder
         */
        public Builder outputDir(String outputDir) {
            this.outputDir = outputDir;
            return this;
        }
        
        /**
         * Sets the protocol.
         *
         * @param protocol The protocol shape ID as string
         * @return This builder
         */
        public Builder protocol(String protocol) {
            this.protocol = protocol;
            return this;
        }
        
        /**
         * Sets the Smithy edition.
         *
         * @param edition The edition string
         * @return This builder
         */
        public Builder edition(String edition) {
            this.edition = edition;
            return this;
        }
        
        /**
         * Builds the ErlangSettings instance.
         *
         * @return The built settings
         * @throws NullPointerException if service is not set
         */
        public ErlangSettings build() {
            return new ErlangSettings(this);
        }
    }
}
