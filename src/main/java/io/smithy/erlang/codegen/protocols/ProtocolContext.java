package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.ErlangSymbolProvider;
import io.smithy.erlang.codegen.ErlangWriter;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;

/**
 * Context holder for protocol code generation.
 * 
 * This class encapsulates all the context information needed during
 * protocol-specific code generation, making it easier to pass around
 * and reducing method parameter lists.
 * 
 * @deprecated Use {@link io.smithy.erlang.codegen.ErlangContext} instead.
 *             This class is maintained for backward compatibility with
 *             {@link io.smithy.erlang.codegen.ErlangClientPlugin}.
 * @see io.smithy.erlang.codegen.ErlangContext
 */
@Deprecated
public final class ProtocolContext {
    
    private final Model model;
    private final ServiceShape service;
    private final OperationShape operation;
    private final ErlangSymbolProvider symbolProvider;
    private final ErlangWriter writer;
    private final Protocol protocol;
    
    private ProtocolContext(Builder builder) {
        this.model = builder.model;
        this.service = builder.service;
        this.operation = builder.operation;
        this.symbolProvider = builder.symbolProvider;
        this.writer = builder.writer;
        this.protocol = builder.protocol;
    }
    
    /**
     * Gets the Smithy model.
     */
    public Model model() {
        return model;
    }
    
    /**
     * Gets the service shape.
     */
    public ServiceShape service() {
        return service;
    }
    
    /**
     * Gets the operation shape.
     */
    public OperationShape operation() {
        return operation;
    }
    
    /**
     * Gets the symbol provider.
     */
    public ErlangSymbolProvider symbolProvider() {
        return symbolProvider;
    }
    
    /**
     * Gets the writer for code generation.
     */
    public ErlangWriter writer() {
        return writer;
    }
    
    /**
     * Gets the protocol being used.
     */
    public Protocol protocol() {
        return protocol;
    }
    
    /**
     * Creates a new builder for ProtocolContext.
     */
    public static Builder builder() {
        return new Builder();
    }
    
    /**
     * Builder for ProtocolContext.
     */
    public static final class Builder {
        private Model model;
        private ServiceShape service;
        private OperationShape operation;
        private ErlangSymbolProvider symbolProvider;
        private ErlangWriter writer;
        private Protocol protocol;
        
        private Builder() {
        }
        
        /**
         * Sets the Smithy model.
         */
        public Builder model(Model model) {
            this.model = model;
            return this;
        }
        
        /**
         * Sets the service shape.
         */
        public Builder service(ServiceShape service) {
            this.service = service;
            return this;
        }
        
        /**
         * Sets the operation shape.
         */
        public Builder operation(OperationShape operation) {
            this.operation = operation;
            return this;
        }
        
        /**
         * Sets the symbol provider.
         */
        public Builder symbolProvider(ErlangSymbolProvider symbolProvider) {
            this.symbolProvider = symbolProvider;
            return this;
        }
        
        /**
         * Sets the writer.
         */
        public Builder writer(ErlangWriter writer) {
            this.writer = writer;
            return this;
        }
        
        /**
         * Sets the protocol.
         */
        public Builder protocol(Protocol protocol) {
            this.protocol = protocol;
            return this;
        }
        
        /**
         * Builds the ProtocolContext.
         *
         * @throws IllegalStateException if any required field is null
         */
        public ProtocolContext build() {
            if (model == null) {
                throw new IllegalStateException("model is required");
            }
            if (service == null) {
                throw new IllegalStateException("service is required");
            }
            if (symbolProvider == null) {
                throw new IllegalStateException("symbolProvider is required");
            }
            if (writer == null) {
                throw new IllegalStateException("writer is required");
            }
            if (protocol == null) {
                throw new IllegalStateException("protocol is required");
            }
            // operation can be null for service-level generation
            
            return new ProtocolContext(this);
        }
    }
}
