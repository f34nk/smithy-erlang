package io.smithy.erlang.codegen.generators;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.model.shapes.IntEnumShape;
import software.amazon.smithy.model.shapes.MemberShape;

import java.util.*;
import java.util.logging.Logger;

/**
 * Generates Erlang code for integer enum shapes.
 * 
 * <p>Integer enum shapes in Smithy 2.0 are represented in Erlang as
 * integer types with encoding/decoding functions that map between
 * symbolic names and integer values.
 * 
 * <p>Example Smithy intEnum:
 * <pre>
 * intEnum Priority {
 *     LOW = 1
 *     MEDIUM = 2
 *     HIGH = 3
 * }
 * </pre>
 * 
 * <p>Generated Erlang type:
 * <pre>
 * -type priority() :: 1 | 2 | 3.
 * </pre>
 * 
 * <p>The generator also produces encoding and decoding functions for
 * mapping between integer values and atom names when needed.
 * 
 * @see ErlangContext
 */
public final class IntEnumGenerator {
    
    private static final Logger LOGGER = Logger.getLogger(IntEnumGenerator.class.getName());
    
    private final IntEnumShape intEnumShape;
    private final ErlangContext context;
    
    /**
     * Creates a new integer enum generator.
     *
     * @param intEnumShape The int enum shape
     * @param context The code generation context
     */
    public IntEnumGenerator(IntEnumShape intEnumShape, ErlangContext context) {
        this.intEnumShape = Objects.requireNonNull(intEnumShape, "intEnumShape is required");
        this.context = Objects.requireNonNull(context, "context is required");
    }
    
    /**
     * Gets the int enum shape being generated.
     *
     * @return The int enum shape
     */
    public IntEnumShape getIntEnumShape() {
        return intEnumShape;
    }
    
    /**
     * Gets the Erlang type name for this int enum.
     *
     * @return The snake_case type name
     */
    public String getTypeName() {
        return EnhancedErlangSymbolProvider.toErlangName(intEnumShape.getId().getName());
    }
    
    /**
     * Gets the enum members (name to value mappings).
     *
     * @return Map of member names to integer values
     */
    public Map<String, Integer> getEnumValues() {
        return intEnumShape.getEnumValues();
    }
    
    /**
     * Generates the complete int enum code: type definition and helper functions.
     * 
     * @param writer The writer to output code to
     */
    public void generate(ErlangWriter writer) {
        LOGGER.fine("Generating int enum: " + intEnumShape.getId());
        
        generateTypeDefinition(writer);
        generateEncodingFunction(writer);
        generateDecodingFunction(writer);
    }
    
    /**
     * Generates the Erlang type definition for the int enum.
     * 
     * @param writer The writer to output code to
     */
    public void generateTypeDefinition(ErlangWriter writer) {
        String typeName = getTypeName();
        
        writer.write("");
        writer.writeComment("Integer enum type for " + intEnumShape.getId().getName());
        
        // Start type definition
        writer.write("-type $L() :: ", typeName);
        
        // Get sorted values for deterministic output
        List<Integer> values = new ArrayList<>(intEnumShape.getEnumValues().values());
        Collections.sort(values);
        
        for (int i = 0; i < values.size(); i++) {
            // Add pipe separator for all but first value
            if (i > 0) {
                writer.writeInline("    | ");
            } else {
                writer.writeInline("    ");
            }
            
            writer.writeInline("$L", values.get(i));
            
            if (i < values.size() - 1) {
                writer.write("");
            }
        }
        
        // Close type definition
        writer.write(".");
    }
    
    /**
     * Generates the encoding function (atom to integer).
     * 
     * @param writer The writer to output code to
     */
    public void generateEncodingFunction(ErlangWriter writer) {
        String enumName = getTypeName();
        String functionName = "encode_" + enumName;
        
        writer.writeComment("Encode " + intEnumShape.getId().getName() + " atom to integer value");
        writer.writeSpec(functionName, "atom()", "integer()");
        
        Map<String, Integer> enumValues = intEnumShape.getEnumValues();
        List<String> names = new ArrayList<>(enumValues.keySet());
        Collections.sort(names);
        
        for (int i = 0; i < names.size(); i++) {
            String memberName = names.get(i);
            int value = enumValues.get(memberName);
            String atomName = EnhancedErlangSymbolProvider.toErlangName(memberName);
            
            // Use period for last clause, semicolon for others
            if (i == names.size() - 1) {
                writer.write("$L($L) -> $L.", functionName, atomName, value);
            } else {
                writer.write("$L($L) -> $L;", functionName, atomName, value);
            }
        }
        
        writer.write("");
    }
    
    /**
     * Generates the decoding function (integer to atom).
     * 
     * @param writer The writer to output code to
     */
    public void generateDecodingFunction(ErlangWriter writer) {
        String enumName = getTypeName();
        String functionName = "decode_" + enumName;
        
        writer.writeComment("Decode integer value to " + intEnumShape.getId().getName() + " atom");
        writer.writeSpec(functionName, "integer()", "{ok, atom()} | {error, {invalid_int_enum_value, integer()}}");
        
        Map<String, Integer> enumValues = intEnumShape.getEnumValues();
        List<String> names = new ArrayList<>(enumValues.keySet());
        Collections.sort(names);
        
        for (String memberName : names) {
            int value = enumValues.get(memberName);
            String atomName = EnhancedErlangSymbolProvider.toErlangName(memberName);
            
            writer.write("$L($L) -> {ok, $L};", functionName, value, atomName);
        }
        
        // Add catch-all clause for invalid values
        writer.writeComment("Catch-all for invalid int enum values");
        writer.write("$L(Other) -> {error, {invalid_int_enum_value, Other}}.", functionName);
        
        writer.write("");
    }
}
