package io.smithy.erlang.codegen.symbol;

import io.smithy.erlang.codegen.ErlangSettings;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import software.amazon.smithy.codegen.core.Symbol;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.EnumDefinition;
import software.amazon.smithy.model.traits.EnumTrait;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for EnhancedErlangSymbolProvider.
 */
class EnhancedErlangSymbolProviderTest {
    
    private static final ShapeId SERVICE_ID = ShapeId.from("com.example#TestService");
    
    private Model model;
    private ErlangSettings settings;
    private EnhancedErlangSymbolProvider provider;
    
    @BeforeEach
    void setUp() {
        model = Model.assembler()
                .addShape(ServiceShape.builder()
                        .id(SERVICE_ID)
                        .version("1.0")
                        .build())
                .assemble()
                .unwrap();
        
        settings = ErlangSettings.builder()
                .service(SERVICE_ID)
                .moduleName("test_service")
                .outputDir("src/generated")
                .build();
        
        provider = new EnhancedErlangSymbolProvider(model, settings);
    }
    
    // ========== toErlangName Tests ==========
    
    @Nested
    @DisplayName("toErlangName()")
    class ToErlangNameTests {
        
        @Test
        @DisplayName("Converts PascalCase to snake_case")
        void testPascalCaseConversion() {
            assertEquals("my_service", EnhancedErlangSymbolProvider.toErlangName("MyService"));
            assertEquals("get_user", EnhancedErlangSymbolProvider.toErlangName("GetUser"));
            assertEquals("list_all_items", EnhancedErlangSymbolProvider.toErlangName("ListAllItems"));
        }
        
        @Test
        @DisplayName("Converts camelCase to snake_case")
        void testCamelCaseConversion() {
            assertEquals("my_service", EnhancedErlangSymbolProvider.toErlangName("myService"));
            assertEquals("get_user", EnhancedErlangSymbolProvider.toErlangName("getUser"));
        }
        
        @Test
        @DisplayName("Handles acronyms correctly")
        void testAcronymHandling() {
            assertEquals("s3_storage", EnhancedErlangSymbolProvider.toErlangName("S3Storage"));
            assertEquals("awssdk", EnhancedErlangSymbolProvider.toErlangName("AWSSDK"));
            assertEquals("http_request", EnhancedErlangSymbolProvider.toErlangName("HTTPRequest"));
            assertEquals("aws_s3_client", EnhancedErlangSymbolProvider.toErlangName("AwsS3Client"));
        }
        
        @Test
        @DisplayName("Handles digits correctly")
        void testDigitHandling() {
            assertEquals("ec2_instance", EnhancedErlangSymbolProvider.toErlangName("EC2Instance"));
            assertEquals("item_v2", EnhancedErlangSymbolProvider.toErlangName("ItemV2"));
            assertEquals("route53", EnhancedErlangSymbolProvider.toErlangName("Route53"));
        }
        
        @Test
        @DisplayName("Escapes reserved words")
        void testReservedWordEscaping() {
            assertEquals("case_", EnhancedErlangSymbolProvider.toErlangName("case"));
            assertEquals("end_", EnhancedErlangSymbolProvider.toErlangName("end"));
            assertEquals("if_", EnhancedErlangSymbolProvider.toErlangName("if"));
            assertEquals("receive_", EnhancedErlangSymbolProvider.toErlangName("receive"));
            assertEquals("try_", EnhancedErlangSymbolProvider.toErlangName("try"));
            assertEquals("when_", EnhancedErlangSymbolProvider.toErlangName("when"));
        }
        
        @Test
        @DisplayName("Handles null and empty strings")
        void testNullAndEmpty() {
            assertNull(EnhancedErlangSymbolProvider.toErlangName(null));
            assertEquals("", EnhancedErlangSymbolProvider.toErlangName(""));
        }
        
        @Test
        @DisplayName("Handles already snake_case")
        void testAlreadySnakeCase() {
            assertEquals("my_service", EnhancedErlangSymbolProvider.toErlangName("my_service"));
            assertEquals("get_user", EnhancedErlangSymbolProvider.toErlangName("get_user"));
        }
    }
    
    // ========== isReservedWord Tests ==========
    
    @Nested
    @DisplayName("isReservedWord()")
    class IsReservedWordTests {
        
        @Test
        @DisplayName("Identifies reserved words")
        void testReservedWords() {
            assertTrue(EnhancedErlangSymbolProvider.isReservedWord("after"));
            assertTrue(EnhancedErlangSymbolProvider.isReservedWord("and"));
            assertTrue(EnhancedErlangSymbolProvider.isReservedWord("case"));
            assertTrue(EnhancedErlangSymbolProvider.isReservedWord("end"));
            assertTrue(EnhancedErlangSymbolProvider.isReservedWord("fun"));
            assertTrue(EnhancedErlangSymbolProvider.isReservedWord("if"));
            assertTrue(EnhancedErlangSymbolProvider.isReservedWord("receive"));
            assertTrue(EnhancedErlangSymbolProvider.isReservedWord("try"));
            assertTrue(EnhancedErlangSymbolProvider.isReservedWord("when"));
        }
        
        @Test
        @DisplayName("Returns false for non-reserved words")
        void testNonReservedWords() {
            assertFalse(EnhancedErlangSymbolProvider.isReservedWord("my_function"));
            assertFalse(EnhancedErlangSymbolProvider.isReservedWord("user"));
            assertFalse(EnhancedErlangSymbolProvider.isReservedWord("service"));
        }
        
        @Test
        @DisplayName("getReservedWords returns all reserved words")
        void testGetReservedWords() {
            var reserved = EnhancedErlangSymbolProvider.getReservedWords();
            assertTrue(reserved.contains("case"));
            assertTrue(reserved.contains("end"));
            assertTrue(reserved.contains("if"));
            assertEquals(27, reserved.size()); // All Erlang reserved words
        }
    }
    
    // ========== toSymbol Tests ==========
    
    @Nested
    @DisplayName("toSymbol()")
    class ToSymbolTests {
        
        @Test
        @DisplayName("Creates symbol for StringShape")
        void testStringShape() {
            StringShape shape = StringShape.builder()
                    .id("com.example#UserName")
                    .build();
            
            Model modelWithString = Model.assembler()
                    .addShape(shape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithString, settings);
            Symbol symbol = provider.toSymbol(shape);
            
            assertEquals("user_name", symbol.getName());
            assertEquals("binary()", symbol.getProperty("erlangType", String.class).orElse(""));
        }
        
        @Test
        @DisplayName("Creates symbol for enum StringShape")
        void testEnumStringShape() {
            StringShape shape = StringShape.builder()
                    .id("com.example#Status")
                    .addTrait(EnumTrait.builder()
                            .addEnum(EnumDefinition.builder().value("ACTIVE").build())
                            .addEnum(EnumDefinition.builder().value("INACTIVE").build())
                            .build())
                    .build();
            
            Model modelWithEnum = Model.assembler()
                    .addShape(shape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithEnum, settings);
            Symbol symbol = provider.toSymbol(shape);
            
            assertEquals("status", symbol.getName());
            assertEquals("status()", symbol.getProperty("erlangType", String.class).orElse(""));
        }
        
        @Test
        @DisplayName("Creates symbol for IntegerShape")
        void testIntegerShape() {
            IntegerShape shape = IntegerShape.builder()
                    .id("com.example#ItemCount")
                    .build();
            
            Model modelWithInt = Model.assembler()
                    .addShape(shape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithInt, settings);
            Symbol symbol = provider.toSymbol(shape);
            
            assertEquals("item_count", symbol.getName());
            assertEquals("integer()", symbol.getProperty("erlangType", String.class).orElse(""));
        }
        
        @Test
        @DisplayName("Creates symbol for BooleanShape")
        void testBooleanShape() {
            BooleanShape shape = BooleanShape.builder()
                    .id("com.example#IsActive")
                    .build();
            
            Model modelWithBool = Model.assembler()
                    .addShape(shape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithBool, settings);
            Symbol symbol = provider.toSymbol(shape);
            
            assertEquals("is_active", symbol.getName());
            assertEquals("boolean()", symbol.getProperty("erlangType", String.class).orElse(""));
        }
        
        @Test
        @DisplayName("Creates symbol for ListShape")
        void testListShape() {
            StringShape memberShape = StringShape.builder()
                    .id("com.example#ItemName")
                    .build();
            
            ListShape listShape = ListShape.builder()
                    .id("com.example#ItemNames")
                    .member(memberShape.getId())
                    .build();
            
            Model modelWithList = Model.assembler()
                    .addShapes(memberShape, listShape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithList, settings);
            Symbol symbol = provider.toSymbol(listShape);
            
            assertEquals("item_names", symbol.getName());
            assertEquals("[binary()]", symbol.getProperty("erlangType", String.class).orElse(""));
            assertEquals("binary()", symbol.getProperty("memberType", String.class).orElse(""));
        }
        
        @Test
        @DisplayName("Creates symbol for MapShape")
        void testMapShape() {
            StringShape keyShape = StringShape.builder()
                    .id("com.example#Key")
                    .build();
            
            IntegerShape valueShape = IntegerShape.builder()
                    .id("com.example#Value")
                    .build();
            
            MapShape mapShape = MapShape.builder()
                    .id("com.example#ItemMap")
                    .key(keyShape.getId())
                    .value(valueShape.getId())
                    .build();
            
            Model modelWithMap = Model.assembler()
                    .addShapes(keyShape, valueShape, mapShape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithMap, settings);
            Symbol symbol = provider.toSymbol(mapShape);
            
            assertEquals("item_map", symbol.getName());
            assertEquals("#{binary() => integer()}", symbol.getProperty("erlangType", String.class).orElse(""));
            assertEquals("binary()", symbol.getProperty("keyType", String.class).orElse(""));
            assertEquals("integer()", symbol.getProperty("valueType", String.class).orElse(""));
        }
        
        @Test
        @DisplayName("Creates symbol for StructureShape")
        void testStructureShape() {
            StringShape nameShape = StringShape.builder()
                    .id("com.example#Name")
                    .build();
            
            StructureShape structShape = StructureShape.builder()
                    .id("com.example#UserProfile")
                    .addMember("name", nameShape.getId())
                    .build();
            
            Model modelWithStruct = Model.assembler()
                    .addShapes(nameShape, structShape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithStruct, settings);
            Symbol symbol = provider.toSymbol(structShape);
            
            assertEquals("user_profile", symbol.getName());
            assertEquals("#user_profile{}", symbol.getProperty("erlangType", String.class).orElse(""));
            assertEquals("user_profile", symbol.getProperty("recordName", String.class).orElse(""));
        }
        
        @Test
        @DisplayName("Creates symbol for UnionShape")
        void testUnionShape() {
            StringShape strShape = StringShape.builder()
                    .id("com.example#StringValue")
                    .build();
            
            IntegerShape intShape = IntegerShape.builder()
                    .id("com.example#IntValue")
                    .build();
            
            UnionShape unionShape = UnionShape.builder()
                    .id("com.example#DataValue")
                    .addMember("stringValue", strShape.getId())
                    .addMember("intValue", intShape.getId())
                    .build();
            
            Model modelWithUnion = Model.assembler()
                    .addShapes(strShape, intShape, unionShape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithUnion, settings);
            Symbol symbol = provider.toSymbol(unionShape);
            
            assertEquals("data_value", symbol.getName());
            assertEquals("data_value()", symbol.getProperty("erlangType", String.class).orElse(""));
        }
        
        @Test
        @DisplayName("Creates symbol for ServiceShape")
        void testServiceShape() {
            ServiceShape serviceShape = ServiceShape.builder()
                    .id("com.example#MyApiService")
                    .version("1.0")
                    .build();
            
            Model modelWithService = Model.assembler()
                    .addShape(serviceShape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithService, settings);
            Symbol symbol = provider.toSymbol(serviceShape);
            
            assertEquals("my_api_service", symbol.getName());
            assertTrue(symbol.getDefinitionFile().endsWith("_client.erl"));
        }
        
        @Test
        @DisplayName("Creates symbol for BlobShape")
        void testBlobShape() {
            BlobShape shape = BlobShape.builder()
                    .id("com.example#FileData")
                    .build();
            
            Model modelWithBlob = Model.assembler()
                    .addShape(shape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithBlob, settings);
            Symbol symbol = provider.toSymbol(shape);
            
            assertEquals("file_data", symbol.getName());
            assertEquals("binary()", symbol.getProperty("erlangType", String.class).orElse(""));
        }
        
        @Test
        @DisplayName("Creates symbol for TimestampShape")
        void testTimestampShape() {
            TimestampShape shape = TimestampShape.builder()
                    .id("com.example#CreatedAt")
                    .build();
            
            Model modelWithTimestamp = Model.assembler()
                    .addShape(shape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithTimestamp, settings);
            Symbol symbol = provider.toSymbol(shape);
            
            assertEquals("created_at", symbol.getName());
            assertEquals("calendar:datetime()", symbol.getProperty("erlangType", String.class).orElse(""));
        }
        
        @Test
        @DisplayName("Creates symbol for FloatShape")
        void testFloatShape() {
            FloatShape shape = FloatShape.builder()
                    .id("com.example#Price")
                    .build();
            
            Model modelWithFloat = Model.assembler()
                    .addShape(shape)
                    .assemble()
                    .unwrap();
            
            provider = new EnhancedErlangSymbolProvider(modelWithFloat, settings);
            Symbol symbol = provider.toSymbol(shape);
            
            assertEquals("price", symbol.getName());
            assertEquals("float()", symbol.getProperty("erlangType", String.class).orElse(""));
        }
    }
    
    // ========== Definition File Tests ==========
    
    @Nested
    @DisplayName("Definition File")
    class DefinitionFileTests {
        
        @Test
        @DisplayName("ServiceShape uses _client.erl suffix")
        void testServiceDefinitionFile() {
            ServiceShape serviceShape = ServiceShape.builder()
                    .id("com.example#MyService")
                    .version("1.0")
                    .build();
            
            Model m = Model.assembler().addShape(serviceShape).assemble().unwrap();
            provider = new EnhancedErlangSymbolProvider(m, settings);
            
            Symbol symbol = provider.toSymbol(serviceShape);
            assertEquals("test_service_client.erl", symbol.getDefinitionFile());
        }
        
        @Test
        @DisplayName("Other shapes use _types.erl suffix")
        void testOtherDefinitionFile() {
            StringShape stringShape = StringShape.builder()
                    .id("com.example#MyString")
                    .build();
            
            Model m = Model.assembler().addShape(stringShape).assemble().unwrap();
            provider = new EnhancedErlangSymbolProvider(m, settings);
            
            Symbol symbol = provider.toSymbol(stringShape);
            assertEquals("test_service_types.erl", symbol.getDefinitionFile());
        }
        
        @Test
        @DisplayName("Uses service name when module name not set")
        void testModuleNameFromService() {
            ErlangSettings settingsNoModule = ErlangSettings.builder()
                    .service(SERVICE_ID)
                    .outputDir("src/generated")
                    .build();
            
            StringShape stringShape = StringShape.builder()
                    .id("com.example#MyString")
                    .build();
            
            Model m = Model.assembler().addShape(stringShape).assemble().unwrap();
            provider = new EnhancedErlangSymbolProvider(m, settingsNoModule);
            
            Symbol symbol = provider.toSymbol(stringShape);
            assertEquals("test_service_types.erl", symbol.getDefinitionFile());
        }
    }
    
    // ========== Constructor Tests ==========
    
    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {
        
        @Test
        @DisplayName("Throws on null model")
        void testNullModel() {
            assertThrows(NullPointerException.class, () -> {
                new EnhancedErlangSymbolProvider(null, settings);
            });
        }
        
        @Test
        @DisplayName("Throws on null settings")
        void testNullSettings() {
            assertThrows(NullPointerException.class, () -> {
                new EnhancedErlangSymbolProvider(model, null);
            });
        }
    }
}
