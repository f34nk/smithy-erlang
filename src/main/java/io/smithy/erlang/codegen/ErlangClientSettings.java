package io.smithy.erlang.codegen;

import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.codegen.core.CodegenContext;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.ObjectNode;
import software.amazon.smithy.model.shapes.ShapeId;

import java.util.Objects;
import java.util.Optional;

/**
 * Settings for the Erlang client code generator.
 * These settings are configured in smithy-build.json under the plugin configuration.
 * 
 * @deprecated Use {@link ErlangSettings} instead. This class is maintained for
 *             backward compatibility with {@link ErlangClientPlugin}.
 * @see ErlangSettings
 */
@Deprecated
public final class ErlangClientSettings {
    
    private static final String SERVICE = "service";
    private static final String MODULE = "module";
    private static final String OUTPUT_DIR = "outputDir";
    private static final String PROTOCOL = "protocol";
    private static final String EDITION = "edition";
    
    private final ShapeId service;
    private final String module;
    private final String outputDir;
    private final String protocol;
    private final String edition;
    
    private ErlangClientSettings(Builder builder) {
        this.service = Objects.requireNonNull(builder.service, "service is required");
        this.module = Objects.requireNonNull(builder.module, "module is required");
        this.outputDir = builder.outputDir != null ? builder.outputDir : "build/smithy/erlang";
        this.protocol = builder.protocol;
        this.edition = builder.edition != null ? builder.edition : "2025";
    }
    
    /**
     * Creates settings from a configuration object.
     *
     * @param config Configuration object from smithy-build.json
     * @param model  The Smithy model
     * @param manifest File manifest for output
     * @return ErlangClientSettings instance
     */
    public static ErlangClientSettings from(ObjectNode config, Model model, FileManifest manifest) {
        Builder builder = builder();
        
        config.getStringMember(SERVICE)
                .map(node -> ShapeId.from(node.getValue()))
                .ifPresent(builder::service);
        
        config.getStringMember(MODULE)
                .map(node -> node.getValue())
                .ifPresent(builder::module);
        
        config.getStringMember(OUTPUT_DIR)
                .map(node -> node.getValue())
                .ifPresent(builder::outputDir);
        
        config.getStringMember(PROTOCOL)
                .map(node -> node.getValue())
                .ifPresent(builder::protocol);
        
        config.getStringMember(EDITION)
                .map(node -> node.getValue())
                .ifPresent(builder::edition);
        
        return builder.build();
    }
    
    public static Builder builder() {
        return new Builder();
    }
    
    public ShapeId getService() {
        return service;
    }
    
    public String getModule() {
        return module;
    }
    
    public String getOutputDir() {
        return outputDir;
    }
    
    public Optional<String> getProtocol() {
        return Optional.ofNullable(protocol);
    }
    
    public String getEdition() {
        return edition;
    }
    
    public static final class Builder {
        private ShapeId service;
        private String module;
        private String outputDir;
        private String protocol;
        private String edition;
        
        private Builder() {}
        
        public Builder service(ShapeId service) {
            this.service = service;
            return this;
        }
        
        public Builder module(String module) {
            this.module = module;
            return this;
        }
        
        public Builder outputDir(String outputDir) {
            this.outputDir = outputDir;
            return this;
        }
        
        public Builder protocol(String protocol) {
            this.protocol = protocol;
            return this;
        }
        
        public Builder edition(String edition) {
            this.edition = edition;
            return this;
        }
        
        public ErlangClientSettings build() {
            return new ErlangClientSettings(this);
        }
    }
}
