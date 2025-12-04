package io.smithy.erlang.codegen.generators;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.MemberShape;
import software.amazon.smithy.model.shapes.Shape;
import software.amazon.smithy.model.shapes.StructureShape;
import software.amazon.smithy.model.traits.ErrorTrait;

import java.util.Objects;
import java.util.logging.Logger;

/**
 * Generates Erlang code for error shapes.
 * 
 * <p>Error shapes in Smithy are structure shapes with the @error trait.
 * In Erlang, they are represented as error tuples that can be pattern
 * matched in client code.
 * 
 * <p>Example Smithy error:
 * <pre>
 * {@literal @}error("client")
 * structure ValidationError {
 *     message: String,
 *     field: String
 * }
 * </pre>
 * 
 * <p>Generated Erlang representation:
 * <pre>
 * -type validation_error() :: #{
 *     message => binary(),
 *     field => binary()
 * }.
 * </pre>
 * 
 * <p>Errors are typically returned as:
 * {@code {error, {validation_error, Details}}}
 * 
 * @see ErlangContext
 */
public final class ErrorGenerator {
    
    private static final Logger LOGGER = Logger.getLogger(ErrorGenerator.class.getName());
    
    private final StructureShape errorShape;
    private final ErlangContext context;
    private final Model model;
    private final ErrorTrait errorTrait;
    
    /**
     * Creates a new error generator.
     *
     * @param errorShape The structure shape with @error trait
     * @param context The code generation context
     * @throws IllegalArgumentException if the shape doesn't have ErrorTrait
     */
    public ErrorGenerator(StructureShape errorShape, ErlangContext context) {
        this.errorShape = Objects.requireNonNull(errorShape, "errorShape is required");
        this.context = Objects.requireNonNull(context, "context is required");
        this.model = context.model();
        
        if (!errorShape.hasTrait(ErrorTrait.class)) {
            throw new IllegalArgumentException("Shape must have @error trait: " + errorShape.getId());
        }
        this.errorTrait = errorShape.expectTrait(ErrorTrait.class);
    }
    
    /**
     * Gets the error shape being generated.
     *
     * @return The structure shape with ErrorTrait
     */
    public StructureShape getErrorShape() {
        return errorShape;
    }
    
    /**
     * Gets the Erlang type name for this error.
     *
     * @return The snake_case type name
     */
    public String getTypeName() {
        return EnhancedErlangSymbolProvider.toErlangName(errorShape.getId().getName());
    }
    
    /**
     * Gets the error category (client or server).
     *
     * @return true if this is a client error, false for server error
     */
    public boolean isClientError() {
        return errorTrait.isClientError();
    }
    
    /**
     * Gets the error category string.
     *
     * @return "client" or "server"
     */
    public String getErrorCategory() {
        return errorTrait.getValue();
    }
    
    /**
     * Generates the complete error code: type definition and helper functions.
     * 
     * @param writer The writer to output code to
     */
    public void generate(ErlangWriter writer) {
        LOGGER.fine("Generating error: " + errorShape.getId());
        
        generateTypeDefinition(writer);
        generateParseFunction(writer);
    }
    
    /**
     * Generates the Erlang type definition for the error.
     * 
     * @param writer The writer to output code to
     */
    public void generateTypeDefinition(ErlangWriter writer) {
        String typeName = getTypeName();
        
        writer.write("");
        writer.writeComment("Error type for " + errorShape.getId().getName());
        writer.writeComment("Error category: " + getErrorCategory());
        
        // Generate as map type
        writer.write("-type $L() :: #{", typeName);
        
        int i = 0;
        int memberCount = errorShape.getAllMembers().size();
        for (MemberShape member : errorShape.getAllMembers().values()) {
            String fieldName = EnhancedErlangSymbolProvider.toErlangName(member.getMemberName());
            Shape targetShape = model.expectShape(member.getTarget());
            String fieldType = getErlangType(targetShape);
            
            writer.writeInline("    $L => $L", fieldName, fieldType);
            
            if (i < memberCount - 1) {
                writer.write(",");
            } else {
                writer.write("");
            }
            i++;
        }
        
        writer.write("}.");
    }
    
    /**
     * Generates a parse function to convert error response to typed error.
     * 
     * @param writer The writer to output code to
     */
    public void generateParseFunction(ErlangWriter writer) {
        String typeName = getTypeName();
        String functionName = "parse_" + typeName;
        
        writer.writeComment("Parse error response to " + errorShape.getId().getName());
        writer.writeSpec(functionName, "(map()) -> " + typeName + "()");
        
        writer.write("$L(ErrorMap) ->", functionName);
        writer.indent();
        writer.write("#{");
        
        int i = 0;
        int memberCount = errorShape.getAllMembers().size();
        for (MemberShape member : errorShape.getAllMembers().values()) {
            String erlangFieldName = EnhancedErlangSymbolProvider.toErlangName(member.getMemberName());
            String originalFieldName = member.getMemberName();
            
            writer.writeInline("    $L => maps:get(<<\"$L\">>, ErrorMap, undefined)", 
                    erlangFieldName, originalFieldName);
            
            if (i < memberCount - 1) {
                writer.write(",");
            } else {
                writer.write("");
            }
            i++;
        }
        
        writer.write("}.");
        writer.dedent();
        
        writer.write("");
    }
    
    /**
     * Gets the Erlang type representation for a shape.
     */
    private String getErlangType(Shape shape) {
        if (shape.isStringShape()) {
            return "binary() | undefined";
        } else if (shape.isIntegerShape() || shape.isLongShape() ||
                   shape.isShortShape() || shape.isByteShape()) {
            return "integer() | undefined";
        } else if (shape.isFloatShape() || shape.isDoubleShape()) {
            return "float() | undefined";
        } else if (shape.isBooleanShape()) {
            return "boolean() | undefined";
        } else if (shape.isListShape()) {
            return "list() | undefined";
        } else if (shape.isMapShape()) {
            return "map() | undefined";
        }
        return "term()";
    }
}
