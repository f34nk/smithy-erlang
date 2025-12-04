package io.smithy.erlang.codegen.generators;

import io.smithy.erlang.codegen.*;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.codegen.core.WriterDelegator;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.EnumDefinition;
import software.amazon.smithy.model.traits.EnumTrait;
import software.amazon.smithy.model.traits.ErrorTrait;

import java.nio.file.Path;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for shape generators (Union, Enum, IntEnum, Error, RuntimeModule).
 */
class ShapeGeneratorsTest {
    
    private static final ShapeId TEST_SERVICE_ID = ShapeId.from("com.example#TestService");
    
    private ErlangSettings settings;
    private ErlangContext context;
    
    @TempDir
    Path tempDir;
    
    @BeforeEach
    void setUp() {
        settings = ErlangSettings.builder()
                .service(TEST_SERVICE_ID)
                .moduleName("test_service")
                .build();
    }
    
    private ErlangContext createContext(Model testModel) {
        FileManifest fileManifest = FileManifest.create(tempDir);
        SymbolProvider symbolProvider = new EnhancedErlangSymbolProvider(testModel, settings);
        WriterDelegator<ErlangWriter> writerDelegator = new WriterDelegator<>(
                fileManifest, symbolProvider, ErlangWriter.factory());
        
        return ErlangContext.builder()
                .model(testModel)
                .settings(settings)
                .symbolProvider(symbolProvider)
                .fileManifest(fileManifest)
                .writerDelegator(writerDelegator)
                .integrations(Collections.emptyList())
                .build();
    }
    
    // ========== UnionGenerator Tests ==========
    
    @Nested
    @DisplayName("UnionGenerator")
    class UnionGeneratorTests {
        
        @Test
        @DisplayName("Creates generator with union and context")
        void testConstructor() {
            UnionShape union = UnionShape.builder()
                    .id(ShapeId.from("com.example#TestUnion"))
                    .addMember("stringValue", ShapeId.from("smithy.api#String"))
                    .addMember("intValue", ShapeId.from("smithy.api#Integer"))
                    .build();
            
            Model model = Model.assembler()
                    .addShape(union)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            UnionGenerator generator = new UnionGenerator(union, ctx);
            
            assertNotNull(generator);
            assertEquals(union, generator.getUnion());
            assertEquals("test_union", generator.getTypeName());
        }
        
        @Test
        @DisplayName("Throws NullPointerException for null union")
        void testNullUnion() {
            Model model = Model.assembler()
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            assertThrows(NullPointerException.class, () -> new UnionGenerator(null, ctx));
        }
        
        @Test
        @DisplayName("Generates type definition with variants")
        void testGeneratesTypeDefinition() {
            UnionShape union = UnionShape.builder()
                    .id(ShapeId.from("com.example#TestUnion"))
                    .addMember("stringValue", ShapeId.from("smithy.api#String"))
                    .addMember("intValue", ShapeId.from("smithy.api#Integer"))
                    .build();
            
            Model model = Model.assembler()
                    .addShape(union)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            UnionGenerator generator = new UnionGenerator(union, ctx);
            ErlangWriter writer = new ErlangWriter("test_module");
            
            generator.generateTypeDefinition(writer);
            
            String output = writer.toString();
            assertTrue(output.contains("-type test_union()"));
            assertTrue(output.contains("{string_value, binary()}"));
            assertTrue(output.contains("{int_value, integer()}"));
            assertTrue(output.contains("{unknown, term()}"));
        }
        
        @Test
        @DisplayName("Generates encoding function")
        void testGeneratesEncodingFunction() {
            UnionShape union = UnionShape.builder()
                    .id(ShapeId.from("com.example#TestUnion"))
                    .addMember("stringValue", ShapeId.from("smithy.api#String"))
                    .build();
            
            Model model = Model.assembler()
                    .addShape(union)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            UnionGenerator generator = new UnionGenerator(union, ctx);
            ErlangWriter writer = new ErlangWriter("test_module");
            
            generator.generateEncodingFunction(writer);
            
            String output = writer.toString();
            assertTrue(output.contains("encode_test_union"));
            assertTrue(output.contains("encode_test_union({string_value, Data})"));
        }
        
        @Test
        @DisplayName("Generates decoding function")
        void testGeneratesDecodingFunction() {
            UnionShape union = UnionShape.builder()
                    .id(ShapeId.from("com.example#TestUnion"))
                    .addMember("stringValue", ShapeId.from("smithy.api#String"))
                    .build();
            
            Model model = Model.assembler()
                    .addShape(union)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            UnionGenerator generator = new UnionGenerator(union, ctx);
            ErlangWriter writer = new ErlangWriter("test_module");
            
            generator.generateDecodingFunction(writer);
            
            String output = writer.toString();
            assertTrue(output.contains("decode_test_union"));
            assertTrue(output.contains("{string_value, Data}"));
        }
    }
    
    // ========== EnumGenerator Tests ==========
    
    @Nested
    @DisplayName("EnumGenerator")
    class EnumGeneratorTests {
        
        @Test
        @DisplayName("Creates generator with enum and context")
        void testConstructor() {
            StringShape enumShape = StringShape.builder()
                    .id(ShapeId.from("com.example#TestEnum"))
                    .addTrait(EnumTrait.builder()
                            .addEnum(EnumDefinition.builder().value("VALUE1").build())
                            .addEnum(EnumDefinition.builder().value("VALUE2").build())
                            .build())
                    .build();
            
            Model model = Model.assembler()
                    .addShape(enumShape)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            EnumGenerator generator = new EnumGenerator(enumShape, ctx);
            
            assertNotNull(generator);
            assertEquals(enumShape, generator.getEnumShape());
            assertEquals("test_enum", generator.getTypeName());
            assertEquals(2, generator.getValues().size());
        }
        
        @Test
        @DisplayName("Throws for non-enum shape")
        void testThrowsForNonEnum() {
            StringShape nonEnum = StringShape.builder()
                    .id(ShapeId.from("com.example#NotAnEnum"))
                    .build();
            
            Model model = Model.assembler()
                    .addShape(nonEnum)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            assertThrows(IllegalArgumentException.class, () -> new EnumGenerator(nonEnum, ctx));
        }
        
        @Test
        @DisplayName("Generates type definition with values")
        void testGeneratesTypeDefinition() {
            StringShape enumShape = StringShape.builder()
                    .id(ShapeId.from("com.example#StorageClass"))
                    .addTrait(EnumTrait.builder()
                            .addEnum(EnumDefinition.builder().value("STANDARD").build())
                            .addEnum(EnumDefinition.builder().value("GLACIER").build())
                            .build())
                    .build();
            
            Model model = Model.assembler()
                    .addShape(enumShape)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            EnumGenerator generator = new EnumGenerator(enumShape, ctx);
            ErlangWriter writer = new ErlangWriter("test_module");
            
            generator.generateTypeDefinition(writer);
            
            String output = writer.toString();
            assertTrue(output.contains("-type storage_class()"));
            assertTrue(output.contains("standard"));
            assertTrue(output.contains("glacier"));
        }
        
        @Test
        @DisplayName("Generates encoding function")
        void testGeneratesEncodingFunction() {
            StringShape enumShape = StringShape.builder()
                    .id(ShapeId.from("com.example#StorageClass"))
                    .addTrait(EnumTrait.builder()
                            .addEnum(EnumDefinition.builder().value("STANDARD").build())
                            .build())
                    .build();
            
            Model model = Model.assembler()
                    .addShape(enumShape)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            EnumGenerator generator = new EnumGenerator(enumShape, ctx);
            ErlangWriter writer = new ErlangWriter("test_module");
            
            generator.generateEncodingFunction(writer);
            
            String output = writer.toString();
            assertTrue(output.contains("encode_storage_class"));
            assertTrue(output.contains("<<\"STANDARD\">>"));
        }
        
        @Test
        @DisplayName("Generates decoding function with validation")
        void testGeneratesDecodingFunction() {
            StringShape enumShape = StringShape.builder()
                    .id(ShapeId.from("com.example#StorageClass"))
                    .addTrait(EnumTrait.builder()
                            .addEnum(EnumDefinition.builder().value("STANDARD").build())
                            .build())
                    .build();
            
            Model model = Model.assembler()
                    .addShape(enumShape)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            EnumGenerator generator = new EnumGenerator(enumShape, ctx);
            ErlangWriter writer = new ErlangWriter("test_module");
            
            generator.generateDecodingFunction(writer);
            
            String output = writer.toString();
            assertTrue(output.contains("decode_storage_class"));
            assertTrue(output.contains("{ok, standard}"));
            assertTrue(output.contains("{error, {invalid_enum_value, Other}}"));
        }
    }
    
    // ========== IntEnumGenerator Tests ==========
    
    @Nested
    @DisplayName("IntEnumGenerator")
    class IntEnumGeneratorTests {
        
        @Test
        @DisplayName("Creates generator with int enum and context")
        void testConstructor() {
            IntEnumShape intEnum = IntEnumShape.builder()
                    .id(ShapeId.from("com.example#Priority"))
                    .addMember("LOW", 1)
                    .addMember("HIGH", 2)
                    .build();
            
            Model model = Model.assembler()
                    .addShape(intEnum)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            IntEnumGenerator generator = new IntEnumGenerator(intEnum, ctx);
            
            assertNotNull(generator);
            assertEquals(intEnum, generator.getIntEnumShape());
            assertEquals("priority", generator.getTypeName());
            assertEquals(2, generator.getEnumValues().size());
        }
        
        @Test
        @DisplayName("Generates type definition with integer values")
        void testGeneratesTypeDefinition() {
            IntEnumShape intEnum = IntEnumShape.builder()
                    .id(ShapeId.from("com.example#Priority"))
                    .addMember("LOW", 1)
                    .addMember("MEDIUM", 2)
                    .addMember("HIGH", 3)
                    .build();
            
            Model model = Model.assembler()
                    .addShape(intEnum)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            IntEnumGenerator generator = new IntEnumGenerator(intEnum, ctx);
            ErlangWriter writer = new ErlangWriter("test_module");
            
            generator.generateTypeDefinition(writer);
            
            String output = writer.toString();
            assertTrue(output.contains("-type priority()"));
            assertTrue(output.contains("1"));
            assertTrue(output.contains("2"));
            assertTrue(output.contains("3"));
        }
        
        @Test
        @DisplayName("Generates encoding and decoding functions")
        void testGeneratesEncodingAndDecodingFunctions() {
            IntEnumShape intEnum = IntEnumShape.builder()
                    .id(ShapeId.from("com.example#Priority"))
                    .addMember("LOW", 1)
                    .build();
            
            Model model = Model.assembler()
                    .addShape(intEnum)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            IntEnumGenerator generator = new IntEnumGenerator(intEnum, ctx);
            ErlangWriter writer = new ErlangWriter("test_module");
            
            generator.generateEncodingFunction(writer);
            generator.generateDecodingFunction(writer);
            
            String output = writer.toString();
            assertTrue(output.contains("encode_priority"));
            assertTrue(output.contains("decode_priority"));
            assertTrue(output.contains("{ok, low}"));
            assertTrue(output.contains("{error, {invalid_int_enum_value, Other}}"));
        }
    }
    
    // ========== ErrorGenerator Tests ==========
    
    @Nested
    @DisplayName("ErrorGenerator")
    class ErrorGeneratorTests {
        
        @Test
        @DisplayName("Creates generator with error shape and context")
        void testConstructor() {
            StructureShape errorShape = StructureShape.builder()
                    .id(ShapeId.from("com.example#ValidationError"))
                    .addMember("message", ShapeId.from("smithy.api#String"))
                    .addTrait(new ErrorTrait("client"))
                    .build();
            
            Model model = Model.assembler()
                    .addShape(errorShape)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            ErrorGenerator generator = new ErrorGenerator(errorShape, ctx);
            
            assertNotNull(generator);
            assertEquals(errorShape, generator.getErrorShape());
            assertEquals("validation_error", generator.getTypeName());
            assertTrue(generator.isClientError());
            assertEquals("client", generator.getErrorCategory());
        }
        
        @Test
        @DisplayName("Throws for non-error shape")
        void testThrowsForNonError() {
            StructureShape nonError = StructureShape.builder()
                    .id(ShapeId.from("com.example#NotAnError"))
                    .addMember("value", ShapeId.from("smithy.api#String"))
                    .build();
            
            Model model = Model.assembler()
                    .addShape(nonError)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            assertThrows(IllegalArgumentException.class, () -> new ErrorGenerator(nonError, ctx));
        }
        
        @Test
        @DisplayName("Generates type definition as map")
        void testGeneratesTypeDefinition() {
            StructureShape errorShape = StructureShape.builder()
                    .id(ShapeId.from("com.example#ValidationError"))
                    .addMember("message", ShapeId.from("smithy.api#String"))
                    .addMember("field", ShapeId.from("smithy.api#String"))
                    .addTrait(new ErrorTrait("client"))
                    .build();
            
            Model model = Model.assembler()
                    .addShape(errorShape)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            ErrorGenerator generator = new ErrorGenerator(errorShape, ctx);
            ErlangWriter writer = new ErlangWriter("test_module");
            
            generator.generateTypeDefinition(writer);
            
            String output = writer.toString();
            assertTrue(output.contains("-type validation_error()"));
            assertTrue(output.contains("message =>"));
            assertTrue(output.contains("field =>"));
        }
        
        @Test
        @DisplayName("Generates parse function")
        void testGeneratesParseFunction() {
            StructureShape errorShape = StructureShape.builder()
                    .id(ShapeId.from("com.example#ValidationError"))
                    .addMember("message", ShapeId.from("smithy.api#String"))
                    .addTrait(new ErrorTrait("client"))
                    .build();
            
            Model model = Model.assembler()
                    .addShape(errorShape)
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            ErrorGenerator generator = new ErrorGenerator(errorShape, ctx);
            ErlangWriter writer = new ErlangWriter("test_module");
            
            generator.generateParseFunction(writer);
            
            String output = writer.toString();
            assertTrue(output.contains("parse_validation_error"));
            assertTrue(output.contains("maps:get"));
        }
    }
    
    // ========== RuntimeModuleGenerator Tests ==========
    
    @Nested
    @DisplayName("RuntimeModuleGenerator")
    class RuntimeModuleGeneratorTests {
        
        @Test
        @DisplayName("Creates generator with context")
        void testConstructor() {
            Model model = Model.assembler()
                    .addShape(ServiceShape.builder().id(TEST_SERVICE_ID).version("1.0").build())
                    .assemble().unwrap();
            
            ErlangContext ctx = createContext(model);
            RuntimeModuleGenerator generator = new RuntimeModuleGenerator(ctx);
            
            assertNotNull(generator);
            assertSame(ctx, generator.getContext());
        }
        
        @Test
        @DisplayName("Returns list of available modules")
        void testGetAvailableModules() {
            String[] modules = RuntimeModuleGenerator.getAvailableModules();
            
            assertNotNull(modules);
            assertTrue(modules.length > 0);
            
            // Check for key modules
            boolean hasSigV4 = false;
            boolean hasCredentials = false;
            for (String module : modules) {
                if (module.equals("aws_sigv4.erl")) hasSigV4 = true;
                if (module.equals("aws_credentials.erl")) hasCredentials = true;
            }
            assertTrue(hasSigV4, "Should include aws_sigv4.erl");
            assertTrue(hasCredentials, "Should include aws_credentials.erl");
        }
        
        @Test
        @DisplayName("Checks module availability")
        void testIsModuleAvailable() {
            assertTrue(RuntimeModuleGenerator.isModuleAvailable("aws_sigv4.erl"));
            assertFalse(RuntimeModuleGenerator.isModuleAvailable("non_existent_module.erl"));
        }
    }
}
