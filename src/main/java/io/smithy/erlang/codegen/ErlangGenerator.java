package io.smithy.erlang.codegen;

import java.util.logging.Logger;

import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.codegen.core.WriterDelegator;
import software.amazon.smithy.codegen.core.directed.CreateContextDirective;
import software.amazon.smithy.codegen.core.directed.CreateSymbolProviderDirective;
import software.amazon.smithy.codegen.core.directed.CustomizeDirective;
import software.amazon.smithy.codegen.core.directed.DirectedCodegen;
import software.amazon.smithy.codegen.core.directed.GenerateEnumDirective;
import software.amazon.smithy.codegen.core.directed.GenerateErrorDirective;
import software.amazon.smithy.codegen.core.directed.GenerateIntEnumDirective;
import software.amazon.smithy.codegen.core.directed.GenerateServiceDirective;
import software.amazon.smithy.codegen.core.directed.GenerateStructureDirective;
import software.amazon.smithy.codegen.core.directed.GenerateUnionDirective;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.Shape;
import software.amazon.smithy.model.shapes.StructureShape;
import software.amazon.smithy.model.shapes.UnionShape;

/**
 * Directed code generator for Erlang.
 * 
 * <p>This class implements Smithy's {@link DirectedCodegen} interface to provide
 * a structured approach to code generation. It delegates to shape-specific
 * generators for different Smithy shapes.
 * 
 * <p>The generator follows the Smithy Development Guide recommendations:
 * <ul>
 *   <li>Creates a type-safe {@link ErlangContext} for code generation</li>
 *   <li>Uses {@link EnhancedErlangSymbolProvider} for symbol resolution</li>
 *   <li>Delegates shape generation to dedicated generators</li>
 *   <li>Supports {@link ErlangIntegration} for extensibility</li>
 * </ul>
 * 
 * <h2>Code Generation Flow</h2>
 * <ol>
 *   <li>{@link #createSymbolProvider} - Creates symbol provider</li>
 *   <li>{@link #createContext} - Creates code generation context</li>
 *   <li>{@link #customizeBeforeShapeGeneration} - Pre-generation hook</li>
 *   <li>Shape generation methods called for each shape</li>
 *   <li>{@link #customizeAfterShapeGeneration} - Post-generation hook</li>
 * </ol>
 * 
 * <h2>Usage</h2>
 * <p>This generator is typically invoked via {@code CodegenDirector}:
 * <pre>
 * CodegenDirector&lt;ErlangWriter, ErlangIntegration, ErlangContext, ErlangSettings&gt; runner 
 *     = new CodegenDirector<>();
 * runner.directedCodegen(new ErlangGenerator());
 * runner.integrationClass(ErlangIntegration.class);
 * runner.model(model);
 * runner.settings(settings);
 * runner.run();
 * </pre>
 * 
 * @see DirectedCodegen
 * @see ErlangContext
 * @see ErlangIntegration
 */
public final class ErlangGenerator 
        implements DirectedCodegen<ErlangContext, ErlangSettings, ErlangIntegration> {
    
    private static final Logger LOGGER = Logger.getLogger(ErlangGenerator.class.getName());
    
    /**
     * Creates a symbol provider for the code generation session.
     * 
     * <p>This method is called first during code generation to create
     * the symbol provider that maps Smithy shapes to Erlang symbols.
     *
     * @param directive The directive containing model and settings
     * @return The symbol provider for this session
     */
    @Override
    public SymbolProvider createSymbolProvider(CreateSymbolProviderDirective<ErlangSettings> directive) {
        LOGGER.fine("Creating symbol provider for service: " + directive.settings().service());
        return new EnhancedErlangSymbolProvider(directive.model(), directive.settings());
    }
    
    /**
     * Creates the code generation context.
     * 
     * <p>This method is called after the symbol provider is created. It
     * builds the {@link ErlangContext} that holds all code generation
     * dependencies and is passed to all shape generators.
     *
     * @param directive The directive containing all required components
     * @return The code generation context
     */
    @Override
    public ErlangContext createContext(CreateContextDirective<ErlangSettings, ErlangIntegration> directive) {
        LOGGER.fine("Creating code generation context");
        
        WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                directive.fileManifest(),
                directive.symbolProvider(),
                ErlangWriter.factory()
        );
        
        return ErlangContext.builder()
                .model(directive.model())
                .settings(directive.settings())
                .symbolProvider(directive.symbolProvider())
                .fileManifest(directive.fileManifest())
                .writerDelegator(writerDelegator)
                .integrations(directive.integrations())
                .build();
    }
    
    /**
     * Generates code for a service shape.
     * 
     * <p>This is the main entry point for client code generation.
     * It uses {@link ClientModuleWriter} to generate the complete
     * Erlang client module including types, operations, and helpers.
     *
     * @param directive The directive containing service shape and context
     */
    @Override
    public void generateService(GenerateServiceDirective<ErlangContext, ErlangSettings> directive) {
        ServiceShape service = directive.shape();
        ErlangContext context = directive.context();
        
        LOGGER.info("Generating service: " + service.getId());
        
        try {
            // Generate the client module
            ClientModuleWriter writer = ClientModuleWriter.fromContext(context);
            writer.generate();
            writer.copyRuntimeModules();
            
            LOGGER.info("Service generation completed: " + service.getId());
        } catch (Exception e) {
            throw new RuntimeException("Failed to generate service: " + service.getId(), e);
        }
    }
    
    /**
     * Generates code for a structure shape.
     * 
     * <p>Structure types in Erlang are typically represented as maps
     * or records. Currently, structures are generated inline within
     * the client module. This method can be expanded to generate
     * separate type modules.
     *
     * @param directive The directive containing structure shape and context
     */
    @Override
    public void generateStructure(GenerateStructureDirective<ErlangContext, ErlangSettings> directive) {
        StructureShape structure = directive.shape();
        
        LOGGER.finest("Processing structure: " + structure.getId());
        
        // Structure types are currently generated inline in client module
        // Could be extracted to separate types module in future
    }
    
    /**
     * Generates code for a union shape.
     * 
     * <p>Union types in Erlang are represented as tagged tuples,
     * e.g., {@code {variant_name, Value}}.
     *
     * @param directive The directive containing union shape and context
     */
    @Override
    public void generateUnion(GenerateUnionDirective<ErlangContext, ErlangSettings> directive) {
        UnionShape union = directive.shape();
        
        LOGGER.finest("Processing union: " + union.getId());
        
        // Union types are currently generated inline in client module
        // Could be extracted to UnionGenerator in future
    }
    
    /**
     * Generates code for an enum shape.
     * 
     * <p>Enum types in Erlang are represented as atoms or binaries
     * with type specifications listing valid values.
     *
     * @param directive The directive containing enum shape and context
     */
    @Override
    public void generateEnumShape(GenerateEnumDirective<ErlangContext, ErlangSettings> directive) {
        Shape enumShape = directive.shape();
        
        LOGGER.finest("Processing enum: " + enumShape.getId());
        
        // Enum types are currently generated inline in client module
        // Could be extracted to EnumGenerator in future
    }
    
    /**
     * Generates code for an integer enum shape.
     * 
     * <p>Integer enums are similar to regular enums but with integer
     * values. In Erlang, these are typically represented as integers
     * with type specifications.
     *
     * @param directive The directive containing int enum shape and context
     */
    @Override
    public void generateIntEnumShape(GenerateIntEnumDirective<ErlangContext, ErlangSettings> directive) {
        Shape intEnum = directive.shape();
        
        LOGGER.finest("Processing int enum: " + intEnum.getId());
        
        // Integer enums are currently generated inline
        // Could be extracted to IntEnumGenerator in future
    }
    
    /**
     * Generates code for an error shape.
     * 
     * <p>Error shapes in Erlang are represented as structured error
     * tuples, e.g., {@code {error, {error_type, Details}}}.
     *
     * @param directive The directive containing error shape and context
     */
    @Override
    public void generateError(GenerateErrorDirective<ErlangContext, ErlangSettings> directive) {
        StructureShape errorShape = directive.shape();
        
        LOGGER.finest("Processing error: " + errorShape.getId());
        
        // Error shapes are currently generated inline
        // Could be extracted to ErrorGenerator in future
    }
    
    /**
     * Hook called before any shape generation begins.
     * 
     * <p>This is the ideal place to:
     * <ul>
     *   <li>Copy runtime modules (aws_sigv4.erl, etc.)</li>
     *   <li>Generate common types or utilities</li>
     *   <li>Perform any setup required before generation</li>
     * </ul>
     *
     * @param directive The directive containing context and settings
     */
    @Override
    public void customizeBeforeShapeGeneration(CustomizeDirective<ErlangContext, ErlangSettings> directive) {
        LOGGER.fine("Running pre-generation customization");
        
        // Run integration pre-processing
        for (ErlangIntegration integration : directive.context().integrations()) {
            LOGGER.finest("Running preprocessModel for: " + integration.name());
            integration.preprocessModel(directive.context());
        }
        
        // Note: Runtime module copying is still handled by ErlangClientPlugin
        // Will be moved here in future refactoring phases
    }
    
    /**
     * Hook called after all shape generation completes.
     * 
     * <p>This is the ideal place to:
     * <ul>
     *   <li>Finalize generated files</li>
     *   <li>Generate aggregated outputs</li>
     *   <li>Run post-processing on generated code</li>
     * </ul>
     *
     * @param directive The directive containing context and settings
     */
    public void customizeAfterShapeGeneration(CustomizeDirective<ErlangContext, ErlangSettings> directive) {
        LOGGER.fine("Running post-generation customization");
        
        // Run integration post-processing
        for (ErlangIntegration integration : directive.context().integrations()) {
            LOGGER.finest("Running postprocessGeneration for: " + integration.name());
            integration.postprocessGeneration(directive.context());
        }
        
        // Flush all writers
        directive.context().writerDelegator().flushWriters();
    }
}
