package io.smithy.erlang.codegen.generators;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.model.shapes.StringShape;
import software.amazon.smithy.model.traits.EnumDefinition;
import software.amazon.smithy.model.traits.EnumTrait;

import java.util.List;
import java.util.Objects;
import java.util.logging.Logger;

/**
 * Generates Erlang code for enum shapes.
 * 
 * <p>Enum shapes in Erlang are represented as atom types with encoding/decoding
 * functions for JSON wire format conversion.
 * 
 * <p>Example Smithy enum:
 * <pre>
 * {@literal @}enum([{value: "STANDARD"}, {value: "GLACIER"}])
 * string StorageClass
 * </pre>
 * 
 * <p>Generated Erlang type:
 * <pre>
 * -type storage_class() :: standard | glacier.
 * </pre>
 * 
 * <p>The generator also produces encoding and decoding functions:
 * <pre>
 * encode_storage_class(standard) -> &lt;&lt;"STANDARD"&gt;&gt;.
 * decode_storage_class(&lt;&lt;"STANDARD"&gt;&gt;) -> {ok, standard}.
 * </pre>
 * 
 * @see ErlangContext
 */
public final class EnumGenerator {
    
    private static final Logger LOGGER = Logger.getLogger(EnumGenerator.class.getName());
    
    private final StringShape enumShape;
    private final ErlangContext context;
    private final EnumTrait enumTrait;
    
    /**
     * Creates a new enum generator.
     *
     * @param enumShape The string shape with @enum trait
     * @param context The code generation context
     * @throws IllegalArgumentException if the shape doesn't have EnumTrait
     */
    public EnumGenerator(StringShape enumShape, ErlangContext context) {
        this.enumShape = Objects.requireNonNull(enumShape, "enumShape is required");
        this.context = Objects.requireNonNull(context, "context is required");
        
        if (!enumShape.hasTrait(EnumTrait.class)) {
            throw new IllegalArgumentException("Shape must have @enum trait: " + enumShape.getId());
        }
        this.enumTrait = enumShape.expectTrait(EnumTrait.class);
    }
    
    /**
     * Gets the enum shape being generated.
     *
     * @return The string shape with EnumTrait
     */
    public StringShape getEnumShape() {
        return enumShape;
    }
    
    /**
     * Gets the Erlang type name for this enum.
     *
     * @return The snake_case type name
     */
    public String getTypeName() {
        return EnhancedErlangSymbolProvider.toErlangName(enumShape.getId().getName());
    }
    
    /**
     * Gets the enum values.
     *
     * @return List of enum definitions
     */
    public List<EnumDefinition> getValues() {
        return enumTrait.getValues();
    }
    
    /**
     * Generates the complete enum code: type definition and helper functions.
     * 
     * @param writer The writer to output code to
     */
    public void generate(ErlangWriter writer) {
        LOGGER.fine("Generating enum: " + enumShape.getId());
        
        generateTypeDefinition(writer);
        generateEncodingFunction(writer);
        generateDecodingFunction(writer);
    }
    
    /**
     * Generates the Erlang type definition for the enum.
     * 
     * @param writer The writer to output code to
     */
    public void generateTypeDefinition(ErlangWriter writer) {
        String typeName = getTypeName();
        
        writer.writeBlankLine();
        writer.writeComment("Enum type for " + enumShape.getId().getName());
        
        // Start type definition
        writer.write("-type $L() :: ", typeName);
        
        // Generate enum values
        List<EnumDefinition> values = enumTrait.getValues();
        for (int i = 0; i < values.size(); i++) {
            EnumDefinition value = values.get(i);
            String atomName = getAtomName(value);
            
            // Add pipe separator for all but first value
            if (i > 0) {
                writer.writeInline("    | ");
            } else {
                writer.writeInline("    ");
            }
            
            writer.writeInline("$L", atomName);
            
            if (i < values.size() - 1) {
                writer.writeBlankLine();
            }
        }
        
        // Close type definition
        writer.write(".");
    }
    
    /**
     * Generates the encoding function for converting enum atom to binary.
     * 
     * @param writer The writer to output code to
     */
    public void generateEncodingFunction(ErlangWriter writer) {
        String enumName = getTypeName();
        String functionName = "encode_" + enumName;
        
        writer.writeComment("Encode " + enumShape.getId().getName() + " enum to JSON string");
        writer.writeSpec(functionName, enumName + "()", "binary()");
        
        // Generate function clauses for each enum value
        List<EnumDefinition> values = enumTrait.getValues();
        for (int i = 0; i < values.size(); i++) {
            EnumDefinition value = values.get(i);
            String atomName = getAtomName(value);
            String wireValue = value.getValue();
            
            // Use period for last clause, semicolon for others
            if (i == values.size() - 1) {
                writer.write("$L($L) -> <<\"$L\">>.", functionName, atomName, wireValue);
            } else {
                writer.write("$L($L) -> <<\"$L\">>;", functionName, atomName, wireValue);
            }
        }
        
        writer.writeBlankLine();
    }
    
    /**
     * Generates the decoding function for converting binary to enum atom.
     * 
     * @param writer The writer to output code to
     */
    public void generateDecodingFunction(ErlangWriter writer) {
        String enumName = getTypeName();
        String functionName = "decode_" + enumName;
        
        writer.writeComment("Decode JSON string to " + enumShape.getId().getName() + " enum with validation");
        writer.writeSpec(functionName, "binary()", "{ok, " + enumName + "()} | {error, {invalid_enum_value, binary()}}");
        
        // Generate function clauses for each enum value
        List<EnumDefinition> values = enumTrait.getValues();
        for (EnumDefinition value : values) {
            String atomName = getAtomName(value);
            String wireValue = value.getValue();
            
            writer.write("$L(<<\"$L\">>) -> {ok, $L};", functionName, wireValue, atomName);
        }
        
        // Add catch-all clause for invalid values
        writer.writeComment("Catch-all for invalid enum values");
        writer.write("$L(Other) -> {error, {invalid_enum_value, Other}}.", functionName);
        
        writer.writeBlankLine();
    }
    
    /**
     * Gets the Erlang atom name for an enum value.
     */
    private String getAtomName(EnumDefinition value) {
        return value.getName().isPresent()
                ? EnhancedErlangSymbolProvider.toErlangName(value.getName().get())
                : EnhancedErlangSymbolProvider.toErlangName(value.getValue());
    }
}
