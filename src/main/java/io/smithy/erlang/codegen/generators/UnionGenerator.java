package io.smithy.erlang.codegen.generators;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.MemberShape;
import software.amazon.smithy.model.shapes.Shape;
import software.amazon.smithy.model.shapes.UnionShape;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.logging.Logger;

/**
 * Generates Erlang code for union shapes.
 * 
 * <p>Union shapes in Erlang are represented as tagged tuples. Each union
 * variant becomes a tuple of {@code {tag, value}} where tag is an atom.
 * 
 * <p>Example Smithy union:
 * <pre>
 * union StorageType {
 *     s3: S3Storage,
 *     glacier: GlacierStorage
 * }
 * </pre>
 * 
 * <p>Generated Erlang type:
 * <pre>
 * -type storage_type() :: {s3, #s3_storage{}} | {glacier, #glacier_storage{}}.
 * </pre>
 * 
 * <p>This generator also produces encoding and decoding functions for
 * converting between Erlang tagged tuples and JSON representations.
 * 
 * @see ErlangContext
 */
public final class UnionGenerator {
    
    private static final Logger LOGGER = Logger.getLogger(UnionGenerator.class.getName());
    
    private final UnionShape union;
    private final ErlangContext context;
    private final Model model;
    private final SymbolProvider symbolProvider;
    
    /**
     * Creates a new union generator.
     *
     * @param union The union shape to generate
     * @param context The code generation context
     */
    public UnionGenerator(UnionShape union, ErlangContext context) {
        this.union = Objects.requireNonNull(union, "union is required");
        this.context = Objects.requireNonNull(context, "context is required");
        this.model = context.model();
        this.symbolProvider = context.symbolProvider();
    }
    
    /**
     * Gets the union shape being generated.
     *
     * @return The union shape
     */
    public UnionShape getUnion() {
        return union;
    }
    
    /**
     * Gets the Erlang type name for this union.
     *
     * @return The snake_case type name
     */
    public String getTypeName() {
        return EnhancedErlangSymbolProvider.toErlangName(union.getId().getName());
    }
    
    /**
     * Generates the complete union code: type definition and helper functions.
     * 
     * @param writer The writer to output code to
     */
    public void generate(ErlangWriter writer) {
        LOGGER.fine("Generating union: " + union.getId());
        
        generateTypeDefinition(writer);
        generateEncodingFunction(writer);
        generateDecodingFunction(writer);
    }
    
    /**
     * Generates the Erlang type definition for the union.
     * 
     * @param writer The writer to output code to
     */
    public void generateTypeDefinition(ErlangWriter writer) {
        String typeName = getTypeName();
        
        writer.write("");
        writer.writeComment("Union type for " + union.getId().getName());
        writer.writeComment("Represented as tagged tuples: {variant_name, variant_data}");
        
        // Start type definition
        writer.write("-type $L() :: ", typeName);
        
        // Generate union variants
        List<MemberShape> members = new ArrayList<>(union.getAllMembers().values());
        for (int i = 0; i < members.size(); i++) {
            MemberShape member = members.get(i);
            String variantName = EnhancedErlangSymbolProvider.toErlangName(member.getMemberName());
            Shape targetShape = model.expectShape(member.getTarget());
            
            // Get the type representation for the variant
            String variantType = getErlangType(targetShape);
            
            // Add pipe separator for all but first value
            if (i > 0) {
                writer.writeInline("    | ");
            } else {
                writer.writeInline("    ");
            }
            
            writer.writeInline("{$L, $L}", variantName, variantType);
            
            if (i < members.size() - 1) {
                writer.write("");
            }
        }
        
        // Add unknown variant for forward compatibility
        writer.write("");
        writer.writeInline("    | {unknown, term()}.");
        writer.write("");
    }
    
    /**
     * Generates the encoding function for converting union to JSON.
     * 
     * @param writer The writer to output code to
     */
    public void generateEncodingFunction(ErlangWriter writer) {
        String unionName = getTypeName();
        String functionName = "encode_" + unionName;
        
        writer.writeComment("Encode " + union.getId().getName() + " union to JSON");
        writer.writeSpec(functionName, unionName + "() | {unknown, term()}", "map()");
        
        // Generate function clauses for each variant
        List<MemberShape> members = new ArrayList<>(union.getAllMembers().values());
        for (MemberShape member : members) {
            String variantName = EnhancedErlangSymbolProvider.toErlangName(member.getMemberName());
            String originalMemberName = member.getMemberName();
            
            // Function clause: encode_storage_type({s3, Data}) ->
            writer.write("$L({$L, Data}) ->", functionName, variantName);
            writer.indent();
            writer.write("#{<<\"$L\">> => Data};", originalMemberName);
            writer.dedent();
        }
        
        // Add unknown variant handler for forward compatibility
        writer.write("$L({unknown, Data}) ->", functionName);
        writer.indent();
        writer.write("#{<<\"unknown\">> => Data}.");
        writer.dedent();
        
        writer.write("");
    }
    
    /**
     * Generates the decoding function for converting JSON to union.
     * 
     * @param writer The writer to output code to
     */
    public void generateDecodingFunction(ErlangWriter writer) {
        String unionName = getTypeName();
        String functionName = "decode_" + unionName;
        
        writer.writeComment("Decode JSON to " + union.getId().getName() + " union");
        writer.writeSpec(functionName, "map()", unionName + "() | {unknown, term()}");
        
        // Generate function clauses for each variant
        List<MemberShape> members = new ArrayList<>(union.getAllMembers().values());
        for (MemberShape member : members) {
            String variantName = EnhancedErlangSymbolProvider.toErlangName(member.getMemberName());
            String originalMemberName = member.getMemberName();
            
            // Function clause: decode_storage_type(#{<<"s3">> := Data}) ->
            writer.write("$L(#{<<\"$L\">> := Data}) ->", functionName, originalMemberName);
            writer.indent();
            writer.write("{$L, Data};", variantName);
            writer.dedent();
        }
        
        // Add unknown variant handler for forward compatibility
        writer.writeComment("Handle unknown variants for forward compatibility");
        writer.write("$L(UnknownVariant) ->", functionName);
        writer.indent();
        writer.write("{unknown, UnknownVariant}.");
        writer.dedent();
        
        writer.write("");
    }
    
    /**
     * Gets the Erlang type representation for a shape.
     */
    private String getErlangType(Shape shape) {
        if (shape.isStructureShape()) {
            String recordName = EnhancedErlangSymbolProvider.toErlangName(shape.getId().getName());
            return "#" + recordName + "{}";
        } else if (shape.isStringShape()) {
            return "binary()";
        } else if (shape.isIntegerShape() || shape.isLongShape() || 
                   shape.isShortShape() || shape.isByteShape()) {
            return "integer()";
        } else if (shape.isFloatShape() || shape.isDoubleShape()) {
            return "float()";
        } else if (shape.isBooleanShape()) {
            return "boolean()";
        } else if (shape.isBlobShape()) {
            return "binary()";
        } else if (shape.isTimestampShape()) {
            return "calendar:datetime()";
        } else if (shape.isListShape()) {
            return "list()";
        } else if (shape.isMapShape()) {
            return "map()";
        }
        return "term()";
    }
}
