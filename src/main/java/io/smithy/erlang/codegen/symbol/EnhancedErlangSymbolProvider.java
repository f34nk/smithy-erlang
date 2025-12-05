package io.smithy.erlang.codegen.symbol;

import io.smithy.erlang.codegen.ErlangSettings;
import software.amazon.smithy.codegen.core.Symbol;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.EnumTrait;

import java.util.Objects;
import java.util.Set;

/**
 * Symbol provider that maps Smithy shapes to Erlang symbols.
 * 
 * <p>This class implements the standard Smithy {@link SymbolProvider} interface,
 * providing a mapping from Smithy shapes to Erlang symbols with appropriate
 * names, types, and file locations.
 * 
 * <p>Key features:
 * <ul>
 *   <li>Converts PascalCase names to snake_case for Erlang</li>
 *   <li>Maps Smithy types to Erlang type specifications</li>
 *   <li>Handles reserved word escaping</li>
 *   <li>Determines correct output files for shapes</li>
 * </ul>
 * 
 * <p>Example usage:
 * <pre>
 * SymbolProvider provider = new EnhancedErlangSymbolProvider(model, settings);
 * Symbol symbol = provider.toSymbol(shape);
 * String erlangName = symbol.getName();
 * String erlangType = symbol.getProperty("erlangType", String.class).orElse("term()");
 * </pre>
 * 
 * @see SymbolProvider
 * @see ErlangReservedWords
 */
public final class EnhancedErlangSymbolProvider implements SymbolProvider {
    
    /** Erlang reserved words that must be escaped. */
    private static final Set<String> RESERVED_WORDS = Set.of(
        "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl",
        "bsr", "bxor", "case", "catch", "cond", "div", "end", "fun",
        "if", "let", "not", "of", "or", "orelse", "receive", "rem",
        "try", "when", "xor"
    );
    
    private final Model model;
    private final ErlangSettings settings;
    
    /**
     * Creates a new symbol provider.
     *
     * @param model The Smithy model
     * @param settings The code generation settings
     */
    public EnhancedErlangSymbolProvider(Model model, ErlangSettings settings) {
        this.model = Objects.requireNonNull(model, "model is required");
        this.settings = Objects.requireNonNull(settings, "settings is required");
    }
    
    /**
     * Converts a Smithy shape to an Erlang symbol.
     * 
     * <p>The resulting symbol includes:
     * <ul>
     *   <li>name - The Erlang-friendly snake_case name</li>
     *   <li>namespace - The output directory</li>
     *   <li>definitionFile - The file where the shape is defined</li>
     *   <li>erlangType property - The Erlang type specification</li>
     * </ul>
     *
     * @param shape The shape to convert
     * @return The corresponding Erlang symbol
     */
    @Override
    public Symbol toSymbol(Shape shape) {
        String name = toErlangName(shape.getId().getName());
        
        Symbol.Builder builder = Symbol.builder()
            .name(name)
            .namespace(getNamespace(), "/")
            .definitionFile(getDefinitionFile(shape));
        
        // Add shape-specific type configuration
        shape.accept(new SymbolVisitor(builder));
        
        return builder.build();
    }
    
    /**
     * Converts a Smithy name to an Erlang-friendly snake_case name.
     * 
     * <p>Conversion rules:
     * <ul>
     *   <li>PascalCase → snake_case (MyService → my_service)</li>
     *   <li>camelCase → snake_case (myService → my_service)</li>
     *   <li>Acronyms handled (S3Storage → s3_storage)</li>
     *   <li>Reserved words escaped with underscore suffix</li>
     * </ul>
     *
     * @param name The name to convert
     * @return The Erlang-friendly name
     */
    public static String toErlangName(String name) {
        if (name == null || name.isEmpty()) {
            return name;
        }
        
        String snakeCase = name
            .replaceAll("([a-z])([A-Z])", "$1_$2")
            .replaceAll("([A-Z])([A-Z][a-z])", "$1_$2")
            .replaceAll("([0-9])([A-Z])", "$1_$2")
            .toLowerCase();
        
        // Escape reserved words
        if (isReservedWord(snakeCase)) {
            return snakeCase + "_";
        }
        
        return snakeCase;
    }
    
    /**
     * Checks if a name is an Erlang reserved word.
     *
     * @param name The name to check
     * @return true if the name is reserved
     */
    public static boolean isReservedWord(String name) {
        return RESERVED_WORDS.contains(name);
    }
    
    /**
     * Gets the set of Erlang reserved words.
     *
     * @return Unmodifiable set of reserved words
     */
    public static Set<String> getReservedWords() {
        return RESERVED_WORDS;
    }
    
    /**
     * Gets the namespace (output directory) for symbols.
     *
     * @return The output directory
     */
    private String getNamespace() {
        return settings.outputDir();
    }
    
    /**
     * Gets the definition file for a shape.
     * 
     * <p>Services are defined in {module}_client.erl,
     * all other shapes in {module}_types.erl.
     *
     * @param shape The shape
     * @return The definition file path
     */
    private String getDefinitionFile(Shape shape) {
        String moduleName = settings.moduleName();
        if (moduleName == null) {
            moduleName = toErlangName(settings.service().getName());
        }
        
        if (shape instanceof ServiceShape) {
            return moduleName + "_client.erl";
        }
        return moduleName + "_types.erl";
    }
    
    /**
     * Visitor for shape-specific symbol configuration.
     * 
     * <p>Sets the "erlangType" property on the symbol builder
     * based on the shape type.
     */
    private class SymbolVisitor extends ShapeVisitor.Default<Void> {
        private final Symbol.Builder builder;
        
        SymbolVisitor(Symbol.Builder builder) {
            this.builder = builder;
        }
        
        @Override
        protected Void getDefault(Shape shape) {
            builder.putProperty("erlangType", "term()");
            return null;
        }
        
        @Override
        public Void stringShape(StringShape shape) {
            if (shape.hasTrait(EnumTrait.class)) {
                builder.putProperty("erlangType", toErlangName(shape.getId().getName()) + "()");
            } else {
                builder.putProperty("erlangType", "binary()");
            }
            return null;
        }
        
        @Override
        public Void integerShape(IntegerShape shape) {
            builder.putProperty("erlangType", "integer()");
            return null;
        }
        
        @Override
        public Void longShape(LongShape shape) {
            builder.putProperty("erlangType", "integer()");
            return null;
        }
        
        @Override
        public Void shortShape(ShortShape shape) {
            builder.putProperty("erlangType", "integer()");
            return null;
        }
        
        @Override
        public Void byteShape(ByteShape shape) {
            builder.putProperty("erlangType", "integer()");
            return null;
        }
        
        @Override
        public Void floatShape(FloatShape shape) {
            builder.putProperty("erlangType", "float()");
            return null;
        }
        
        @Override
        public Void doubleShape(DoubleShape shape) {
            builder.putProperty("erlangType", "float()");
            return null;
        }
        
        @Override
        public Void bigIntegerShape(BigIntegerShape shape) {
            builder.putProperty("erlangType", "integer()");
            return null;
        }
        
        @Override
        public Void bigDecimalShape(BigDecimalShape shape) {
            builder.putProperty("erlangType", "float()");
            return null;
        }
        
        @Override
        public Void booleanShape(BooleanShape shape) {
            builder.putProperty("erlangType", "boolean()");
            return null;
        }
        
        @Override
        public Void blobShape(BlobShape shape) {
            builder.putProperty("erlangType", "binary()");
            return null;
        }
        
        @Override
        public Void timestampShape(TimestampShape shape) {
            builder.putProperty("erlangType", "calendar:datetime()");
            return null;
        }
        
        @Override
        public Void listShape(ListShape shape) {
            // Get member type for more specific list type
            Shape memberShape = model.expectShape(shape.getMember().getTarget());
            String memberType = getErlangTypeForShape(memberShape);
            builder.putProperty("erlangType", "[" + memberType + "]");
            builder.putProperty("memberType", memberType);
            return null;
        }
        
        @Override
        public Void setShape(SetShape shape) {
            Shape memberShape = model.expectShape(shape.getMember().getTarget());
            String memberType = getErlangTypeForShape(memberShape);
            builder.putProperty("erlangType", "[" + memberType + "]");
            builder.putProperty("memberType", memberType);
            return null;
        }
        
        @Override
        public Void mapShape(MapShape shape) {
            Shape keyShape = model.expectShape(shape.getKey().getTarget());
            Shape valueShape = model.expectShape(shape.getValue().getTarget());
            String keyType = getErlangTypeForShape(keyShape);
            String valueType = getErlangTypeForShape(valueShape);
            builder.putProperty("erlangType", "#{" + keyType + " => " + valueType + "}");
            builder.putProperty("keyType", keyType);
            builder.putProperty("valueType", valueType);
            return null;
        }
        
        @Override
        public Void structureShape(StructureShape shape) {
            String name = toErlangName(shape.getId().getName());
            builder.putProperty("erlangType", "#" + name + "{}");
            builder.putProperty("recordName", name);
            return null;
        }
        
        @Override
        public Void unionShape(UnionShape shape) {
            String name = toErlangName(shape.getId().getName());
            builder.putProperty("erlangType", name + "()");
            return null;
        }
        
        @Override
        public Void serviceShape(ServiceShape shape) {
            builder.putProperty("erlangType", "map()");
            return null;
        }
        
        @Override
        public Void operationShape(OperationShape shape) {
            builder.putProperty("erlangType", "fun()");
            return null;
        }
        
        @Override
        public Void resourceShape(ResourceShape shape) {
            builder.putProperty("erlangType", "map()");
            return null;
        }
        
        @Override
        public Void memberShape(MemberShape shape) {
            Shape targetShape = model.expectShape(shape.getTarget());
            targetShape.accept(this);
            return null;
        }
        
        /**
         * Gets the Erlang type for a shape (simplified for nested types).
         */
        private String getErlangTypeForShape(Shape shape) {
            if (shape instanceof StringShape) {
                return shape.hasTrait(EnumTrait.class) 
                    ? toErlangName(shape.getId().getName()) + "()"
                    : "binary()";
            } else if (shape instanceof IntegerShape || shape instanceof LongShape ||
                       shape instanceof ShortShape || shape instanceof ByteShape ||
                       shape instanceof BigIntegerShape) {
                return "integer()";
            } else if (shape instanceof FloatShape || shape instanceof DoubleShape ||
                       shape instanceof BigDecimalShape) {
                return "float()";
            } else if (shape instanceof BooleanShape) {
                return "boolean()";
            } else if (shape instanceof BlobShape) {
                return "binary()";
            } else if (shape instanceof TimestampShape) {
                return "calendar:datetime()";
            } else if (shape instanceof ListShape || shape instanceof SetShape) {
                return "list()";
            } else if (shape instanceof MapShape) {
                return "map()";
            } else if (shape instanceof StructureShape) {
                return "#" + toErlangName(shape.getId().getName()) + "{}";
            } else if (shape instanceof UnionShape) {
                return toErlangName(shape.getId().getName()) + "()";
            }
            return "term()";
        }
    }
}
