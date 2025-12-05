package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import software.amazon.smithy.codegen.core.Symbol;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for ErlangWriter.
 */
class ErlangWriterTest {
    
    // ========== Constructor Tests ==========
    
    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {
        
        @Test
        @DisplayName("Creates writer with module name")
        void testConstructorWithModuleName() {
            ErlangWriter writer = new ErlangWriter("my_module");
            assertEquals("my_module", writer.getModuleName());
        }
        
        @Test
        @DisplayName("Writer has import container")
        void testHasImportContainer() {
            ErlangWriter writer = new ErlangWriter("test");
            assertNotNull(writer.getImportContainer());
            assertTrue(writer.getImportContainer() instanceof ErlangImportContainer);
        }
    }
    
    // ========== Module Structure Tests ==========
    
    @Nested
    @DisplayName("Module Structure")
    class ModuleStructureTests {
        
        @Test
        @DisplayName("Writes module header")
        void testWriteModuleHeader() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeModuleHeader();
            
            String output = writer.toString();
            assertTrue(output.contains("-module(my_module)."));
        }
        
        @Test
        @DisplayName("Writes module header with custom name")
        void testWriteModuleHeaderCustomName() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeModuleHeader("custom_module");
            
            String output = writer.toString();
            assertTrue(output.contains("-module(custom_module)."));
        }
        
        @Test
        @DisplayName("Writes exports")
        void testWriteExports() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeExports("foo/1", "bar/2", "baz/0");
            
            String output = writer.toString();
            assertTrue(output.contains("-export(["));
            assertTrue(output.contains("foo/1"));
            assertTrue(output.contains("bar/2"));
            assertTrue(output.contains("baz/0"));
            assertTrue(output.contains("])."));
        }
        
        @Test
        @DisplayName("Writes empty exports")
        void testWriteEmptyExports() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeExports();
            
            String output = writer.toString();
            assertTrue(output.contains("-export([])."));
        }
        
    }
    
    // ========== Type Definition Tests ==========
    
    @Nested
    @DisplayName("Type Definitions")
    class TypeDefinitionTests {
        
        @Test
        @DisplayName("Writes type definition")
        void testWriteType() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeType("my_type", "binary() | integer()");
            
            String output = writer.toString();
            assertTrue(output.contains("-type my_type() :: binary() | integer()."));
        }
        
        @Test
        @DisplayName("Writes opaque type definition")
        void testWriteOpaqueType() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeOpaqueType("my_type", "binary()");
            
            String output = writer.toString();
            assertTrue(output.contains("-opaque my_type() :: binary()."));
        }
        
        @Test
        @DisplayName("Writes single type export")
        void testWriteExportType() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeExportType("my_type", 0);
            
            String output = writer.toString();
            assertTrue(output.contains("-export_type([my_type/0])."));
        }
        
        @Test
        @DisplayName("Writes multiple type exports")
        void testWriteExportTypes() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeExportTypes("type_a/0", "type_b/1");
            
            String output = writer.toString();
            assertTrue(output.contains("-export_type(["));
            assertTrue(output.contains("type_a/0"));
            assertTrue(output.contains("type_b/1"));
        }
    }
    
    // ========== Function Tests ==========
    
    @Nested
    @DisplayName("Function Definitions")
    class FunctionTests {
        
        @Test
        @DisplayName("Writes function spec with args")
        void testWriteSpecWithArgs() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeSpec("my_func", "binary(), integer()", "{ok, term()}");
            
            String output = writer.toString();
            assertTrue(output.contains("-spec my_func(binary(), integer()) -> {ok, term()}."));
        }
        
        @Test
        @DisplayName("Writes function spec without args")
        void testWriteSpecWithoutArgs() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeSpec("my_func", "term()");
            
            String output = writer.toString();
            assertTrue(output.contains("-spec my_func() -> term()."));
        }
        
        @Test
        @DisplayName("Opens and closes function with params")
        void testOpenCloseFunction() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.openFunction("my_func", "Input, Options");
            writer.write("ok.");
            writer.closeFunction();
            
            String output = writer.toString();
            assertTrue(output.contains("my_func(Input, Options) ->"));
            assertTrue(output.contains("ok."));
        }
        
        @Test
        @DisplayName("Opens function without params")
        void testOpenFunctionNoParams() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.openFunction("my_func");
            writer.write("ok.");
            writer.closeFunction();
            
            String output = writer.toString();
            assertTrue(output.contains("my_func() ->"));
        }
        
        @Test
        @DisplayName("Writes function with runnable body")
        void testWriteFunction() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeFunction("my_func", "Input", () -> {
                writer.write("Result = process(Input),");
                writer.write("{ok, Result}.");
            });
            
            String output = writer.toString();
            assertTrue(output.contains("my_func(Input) ->"));
            assertTrue(output.contains("Result = process(Input),"));
            assertTrue(output.contains("{ok, Result}."));
        }
    }
    
    // ========== Record Tests ==========
    
    @Nested
    @DisplayName("Record Definitions")
    class RecordTests {
        
        @Test
        @DisplayName("Writes record definition")
        void testWriteRecord() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeRecord("my_record", () -> {
                writer.write("field1 :: binary(),");
                writer.write("field2 :: integer()");
            });
            
            String output = writer.toString();
            assertTrue(output.contains("-record(my_record, {"));
            assertTrue(output.contains("field1 :: binary(),"));
            assertTrue(output.contains("field2 :: integer()"));
            assertTrue(output.contains("})."));
        }
    }
    
    // ========== Comment Tests ==========
    
    @Nested
    @DisplayName("Comments")
    class CommentTests {
        
        @Test
        @DisplayName("Writes single line comment")
        void testWriteComment() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeComment("This is a comment");
            
            String output = writer.toString();
            assertTrue(output.contains("%% This is a comment"));
        }
        
        @Test
        @DisplayName("Writes EDoc comment")
        void testWriteEdocComment() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeEdocComment("Function documentation");
            
            String output = writer.toString();
            assertTrue(output.contains("%%% @doc Function documentation"));
        }
        
        @Test
        @DisplayName("Writes multi-line doc comment")
        void testWriteDocComment() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeDocComment("Summary line\nSecond line\nThird line");
            
            String output = writer.toString();
            assertTrue(output.contains("%% @doc Summary line"));
            assertTrue(output.contains("%% Second line"));
            assertTrue(output.contains("%% Third line"));
        }
        
        @Test
        @DisplayName("Writes section comment")
        void testWriteSectionComment() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.writeSectionComment("API Functions");
            
            String output = writer.toString();
            assertTrue(output.contains("=========="));
            assertTrue(output.contains("%% API Functions"));
        }
    }
    
    // ========== Custom Formatter Tests ==========
    
    @Nested
    @DisplayName("Custom Formatters")
    class FormatterTests {
        
        @Test
        @DisplayName("$T formatter returns erlangType property")
        void testTypeFormatter() {
            ErlangWriter writer = new ErlangWriter("my_module");
            
            Symbol symbol = Symbol.builder()
                    .name("my_type")
                    .namespace("test", "/")
                    .putProperty("erlangType", "binary()")
                    .build();
            
            writer.write("-type test() :: $T.", symbol);
            
            String output = writer.toString();
            assertTrue(output.contains("-type test() :: binary()."));
        }
        
        @Test
        @DisplayName("$T formatter returns term() for symbol without property")
        void testTypeFormatterDefault() {
            ErlangWriter writer = new ErlangWriter("my_module");
            
            Symbol symbol = Symbol.builder()
                    .name("my_type")
                    .namespace("test", "/")
                    .build();
            
            writer.write("-type test() :: $T.", symbol);
            
            String output = writer.toString();
            assertTrue(output.contains("-type test() :: term()."));
        }
        
        @Test
        @DisplayName("$T formatter handles non-Symbol values")
        void testTypeFormatterNonSymbol() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.write("-type test() :: $T.", "custom_type");
            
            String output = writer.toString();
            assertTrue(output.contains("-type test() :: custom_type."));
        }
        
        @Test
        @DisplayName("$N formatter returns symbol name")
        void testNameFormatter() {
            ErlangWriter writer = new ErlangWriter("my_module");
            
            Symbol symbol = Symbol.builder()
                    .name("user_record")
                    .namespace("test", "/")
                    .build();
            
            writer.write("-record($N, {}).", symbol);
            
            String output = writer.toString();
            assertTrue(output.contains("-record(user_record, {})."));
        }
        
        @Test
        @DisplayName("$N formatter handles non-Symbol values")
        void testNameFormatterNonSymbol() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.write("-record($N, {}).", "my_record");
            
            String output = writer.toString();
            assertTrue(output.contains("-record(my_record, {})."));
        }
    }
    
    // ========== Include Tests ==========
    
    @Nested
    @DisplayName("Include Directives")
    class IncludeTests {
        
        @Test
        @DisplayName("Adds local include")
        void testAddInclude() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.addInclude("types.hrl");
            
            ErlangImportContainer container = writer.getImportContainer();
            assertTrue(container.getIncludes().contains("types.hrl"));
        }
        
        @Test
        @DisplayName("Adds library include")
        void testAddIncludeLib() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.addIncludeLib("jsx/include/jsx.hrl");
            
            ErlangImportContainer container = writer.getImportContainer();
            assertTrue(container.getIncludeLibs().contains("jsx/include/jsx.hrl"));
        }
        
        @Test
        @DisplayName("Writes includes")
        void testWriteIncludes() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.addInclude("types.hrl");
            writer.writeIncludes();
            
            String output = writer.toString();
            assertTrue(output.contains("-include(\"types.hrl\")."));
        }
    }
    
    // ========== Symbol-Aware Method Tests ==========
    
    @Nested
    @DisplayName("Symbol-Aware Methods")
    class SymbolAwareTests {
        
        @Test
        @DisplayName("Writes type from symbol")
        void testWriteTypeFromSymbol() {
            ErlangWriter writer = new ErlangWriter("my_module");
            
            Symbol symbol = Symbol.builder()
                    .name("user")
                    .namespace("test", "/")
                    .putProperty("erlangType", "map()")
                    .build();
            
            writer.writeTypeFromSymbol("user", symbol);
            
            String output = writer.toString();
            assertTrue(output.contains("-type user() :: map()."));
        }
    }
    
    // ========== Factory Tests ==========
    
    @Nested
    @DisplayName("Factory")
    class FactoryTests {
        
        @Test
        @DisplayName("Creates factory")
        void testFactory() {
            ErlangWriter.Factory factory = ErlangWriter.factory();
            assertNotNull(factory);
        }
        
        @Test
        @DisplayName("Factory creates writer with module name from filename")
        void testFactoryApply() {
            ErlangWriter.Factory factory = ErlangWriter.factory();
            ErlangWriter writer = factory.apply("my_client.erl", "namespace");
            
            assertEquals("my_client", writer.getModuleName());
        }
        
        @Test
        @DisplayName("Factory handles filename without extension")
        void testFactoryApplyNoExtension() {
            ErlangWriter.Factory factory = ErlangWriter.factory();
            ErlangWriter writer = factory.apply("my_client", "namespace");
            
            assertEquals("my_client", writer.getModuleName());
        }
    }
    
    // ========== Integration Tests ==========
    
    @Nested
    @DisplayName("Integration")
    class IntegrationTests {
        
        @Test
        @DisplayName("Generates complete module")
        void testCompleteModule() {
            ErlangWriter writer = new ErlangWriter("user_client");
            
            writer.writeModuleHeader()
                    .writeExports("create_user/2", "get_user/2")
                    .write("")
                    .writeSectionComment("Types")
                    .writeType("user", "map()")
                    .writeExportType("user", 0)
                    .write("")
                    .writeSectionComment("API")
                    .writeSpec("create_user", "map(), map()", "{ok, user()} | {error, term()}")
                    .openFunction("create_user", "Input, Config")
                    .write("{ok, Input}.")
                    .closeFunction();
            
            String output = writer.toString();
            
            // Verify module structure
            assertTrue(output.contains("-module(user_client)."));
            assertTrue(output.contains("-export(["));
            assertTrue(output.contains("create_user/2"));
            assertTrue(output.contains("-type user() :: map()."));
            assertTrue(output.contains("-export_type([user/0])."));
            assertTrue(output.contains("-spec create_user(map(), map()) -> {ok, user()} | {error, term()}."));
            assertTrue(output.contains("create_user(Input, Config) ->"));
        }
        
        @Test
        @DisplayName("Method chaining works correctly")
        void testMethodChaining() {
            ErlangWriter writer = new ErlangWriter("test");
            
            ErlangWriter result = writer
                    .writeModuleHeader()
                    .writeExports("foo/1")
                    .writeType("my_type", "term()")
                    .writeComment("A comment");
            
            assertSame(writer, result);
        }
    }
}
