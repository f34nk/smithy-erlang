package io.smithy.erlang.codegen;

import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.*;

/**
 * Simple symbol provider for mapping Smithy shapes to Erlang names and types.
 */
public final class ErlangSymbolProvider {
    
    private final Model model;
    private final ErlangClientSettings settings;
    
    public ErlangSymbolProvider(Model model, ErlangClientSettings settings) {
        this.model = model;
        this.settings = settings;
    }
    
    /**
     * Converts a Smithy name to an Erlang-friendly name.
     * Converts PascalCase to snake_case and ensures lowercase.
     */
    public static String toErlangName(String name) {
        // Convert from PascalCase or camelCase to snake_case
        String snakeCase = name.replaceAll("([a-z])([A-Z])", "$1_$2")
                               .replaceAll("([A-Z])([A-Z][a-z])", "$1_$2")
                               .toLowerCase();
        
        // Escape Erlang reserved words
        if (isReservedWord(snakeCase)) {
            return snakeCase + "_";
        }
        
        return snakeCase;
    }
    
    /**
     * Gets the Erlang type for a Smithy shape.
     */
    public String getErlangType(Shape shape) {
        if (shape instanceof StringShape) {
            return "binary()";
        } else if (shape instanceof IntegerShape || shape instanceof LongShape) {
            return "integer()";
        } else if (shape instanceof FloatShape || shape instanceof DoubleShape) {
            return "float()";
        } else if (shape instanceof BooleanShape) {
            return "boolean()";
        } else if (shape instanceof BlobShape) {
            return "binary()";
        } else if (shape instanceof TimestampShape) {
            return "calendar:datetime()";
        } else if (shape instanceof ListShape) {
            // Use list() without member type in record fields since
            // Erlang doesn't support record type references in typespecs
            return "list()";
        } else if (shape instanceof MapShape) {
            MapShape mapShape = (MapShape) shape;
            Shape keyShape = model.expectShape(mapShape.getKey().getTarget());
            Shape valueShape = model.expectShape(mapShape.getValue().getTarget());
            return "#{" + getErlangType(keyShape) + " => " + getErlangType(valueShape) + "}";
        } else if (shape instanceof StructureShape) {
            return "#" + toErlangName(shape.getId().getName()) + "{}";
        }
        
        return "term()";
    }
    
    /**
     * Checks if a name is an Erlang reserved word.
     */
    private static boolean isReservedWord(String name) {
        return name.equals("after") || name.equals("and") || name.equals("andalso") ||
               name.equals("band") || name.equals("begin") || name.equals("bnot") ||
               name.equals("bor") || name.equals("bsl") || name.equals("bsr") ||
               name.equals("bxor") || name.equals("case") || name.equals("catch") ||
               name.equals("cond") || name.equals("div") || name.equals("end") ||
               name.equals("fun") || name.equals("if") || name.equals("let") ||
               name.equals("not") || name.equals("of") || name.equals("or") ||
               name.equals("orelse") || name.equals("receive") || name.equals("rem") ||
               name.equals("try") || name.equals("when") || name.equals("xor");
    }
}
