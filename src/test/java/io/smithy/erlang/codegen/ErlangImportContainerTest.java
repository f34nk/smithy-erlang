package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import software.amazon.smithy.codegen.core.Symbol;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for ErlangImportContainer.
 */
class ErlangImportContainerTest {
    
    // ========== Constructor Tests ==========
    
    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {
        
        @Test
        @DisplayName("Creates empty container")
        void testEmptyConstructor() {
            ErlangImportContainer container = new ErlangImportContainer();
            
            assertFalse(container.hasIncludes());
            assertTrue(container.getIncludes().isEmpty());
            assertTrue(container.getIncludeLibs().isEmpty());
        }
    }
    
    // ========== addInclude Tests ==========
    
    @Nested
    @DisplayName("addInclude()")
    class AddIncludeTests {
        
        @Test
        @DisplayName("Adds single include")
        void testAddSingleInclude() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude("types.hrl");
            
            assertTrue(container.hasIncludes());
            assertTrue(container.getIncludes().contains("types.hrl"));
        }
        
        @Test
        @DisplayName("Adds multiple includes")
        void testAddMultipleIncludes() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude("types.hrl");
            container.addInclude("records.hrl");
            
            assertEquals(2, container.getIncludes().size());
            assertTrue(container.getIncludes().contains("types.hrl"));
            assertTrue(container.getIncludes().contains("records.hrl"));
        }
        
        @Test
        @DisplayName("Ignores null include")
        void testIgnoresNull() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude(null);
            
            assertFalse(container.hasIncludes());
        }
        
        @Test
        @DisplayName("Ignores empty include")
        void testIgnoresEmpty() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude("");
            
            assertFalse(container.hasIncludes());
        }
        
        @Test
        @DisplayName("Deduplicates includes")
        void testDeduplicates() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude("types.hrl");
            container.addInclude("types.hrl");
            
            assertEquals(1, container.getIncludes().size());
        }
    }
    
    // ========== addIncludeLib Tests ==========
    
    @Nested
    @DisplayName("addIncludeLib()")
    class AddIncludeLibTests {
        
        @Test
        @DisplayName("Adds single library include")
        void testAddSingleIncludeLib() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addIncludeLib("jsx/include/jsx.hrl");
            
            assertTrue(container.hasIncludes());
            assertTrue(container.getIncludeLibs().contains("jsx/include/jsx.hrl"));
        }
        
        @Test
        @DisplayName("Adds multiple library includes")
        void testAddMultipleIncludeLibs() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addIncludeLib("jsx/include/jsx.hrl");
            container.addIncludeLib("kernel/include/file.hrl");
            
            assertEquals(2, container.getIncludeLibs().size());
        }
        
        @Test
        @DisplayName("Ignores null library include")
        void testIgnoresNull() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addIncludeLib(null);
            
            assertFalse(container.hasIncludes());
        }
        
        @Test
        @DisplayName("Ignores empty library include")
        void testIgnoresEmpty() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addIncludeLib("");
            
            assertFalse(container.hasIncludes());
        }
    }
    
    // ========== importSymbol Tests ==========
    
    @Nested
    @DisplayName("importSymbol()")
    class ImportSymbolTests {
        
        @Test
        @DisplayName("Imports symbol with includeFile property")
        void testImportSymbolWithIncludeFile() {
            ErlangImportContainer container = new ErlangImportContainer();
            
            Symbol symbol = Symbol.builder()
                    .name("my_type")
                    .namespace("test", "/")
                    .putProperty("includeFile", "my_types.hrl")
                    .build();
            
            container.importSymbol(symbol, null);
            
            assertTrue(container.getIncludes().contains("my_types.hrl"));
        }
        
        @Test
        @DisplayName("Imports symbol with includeLibFile property")
        void testImportSymbolWithIncludeLibFile() {
            ErlangImportContainer container = new ErlangImportContainer();
            
            Symbol symbol = Symbol.builder()
                    .name("my_type")
                    .namespace("test", "/")
                    .putProperty("includeLibFile", "app/include/types.hrl")
                    .build();
            
            container.importSymbol(symbol, null);
            
            assertTrue(container.getIncludeLibs().contains("app/include/types.hrl"));
        }
        
        @Test
        @DisplayName("Handles symbol without include properties")
        void testImportSymbolWithoutProperties() {
            ErlangImportContainer container = new ErlangImportContainer();
            
            Symbol symbol = Symbol.builder()
                    .name("my_type")
                    .namespace("test", "/")
                    .build();
            
            container.importSymbol(symbol, null);
            
            assertFalse(container.hasIncludes());
        }
        
        @Test
        @DisplayName("Imports symbol with both include types")
        void testImportSymbolWithBothIncludes() {
            ErlangImportContainer container = new ErlangImportContainer();
            
            Symbol symbol = Symbol.builder()
                    .name("my_type")
                    .namespace("test", "/")
                    .putProperty("includeFile", "local.hrl")
                    .putProperty("includeLibFile", "lib/types.hrl")
                    .build();
            
            container.importSymbol(symbol, null);
            
            assertTrue(container.getIncludes().contains("local.hrl"));
            assertTrue(container.getIncludeLibs().contains("lib/types.hrl"));
        }
    }
    
    // ========== hasIncludes Tests ==========
    
    @Nested
    @DisplayName("hasIncludes()")
    class HasIncludesTests {
        
        @Test
        @DisplayName("Returns false for empty container")
        void testEmptyContainer() {
            ErlangImportContainer container = new ErlangImportContainer();
            assertFalse(container.hasIncludes());
        }
        
        @Test
        @DisplayName("Returns true when has local includes")
        void testWithLocalIncludes() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude("types.hrl");
            assertTrue(container.hasIncludes());
        }
        
        @Test
        @DisplayName("Returns true when has library includes")
        void testWithLibIncludes() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addIncludeLib("jsx/jsx.hrl");
            assertTrue(container.hasIncludes());
        }
    }
    
    // ========== getIncludes/getIncludeLibs Tests ==========
    
    @Nested
    @DisplayName("Getters")
    class GetterTests {
        
        @Test
        @DisplayName("getIncludes returns unmodifiable copy")
        void testGetIncludesImmutable() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude("types.hrl");
            
            Set<String> includes = container.getIncludes();
            
            assertThrows(UnsupportedOperationException.class, () -> {
                includes.add("another.hrl");
            });
        }
        
        @Test
        @DisplayName("getIncludeLibs returns unmodifiable copy")
        void testGetIncludeLibsImmutable() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addIncludeLib("lib/types.hrl");
            
            Set<String> includeLibs = container.getIncludeLibs();
            
            assertThrows(UnsupportedOperationException.class, () -> {
                includeLibs.add("another.hrl");
            });
        }
    }
    
    // ========== clear Tests ==========
    
    @Nested
    @DisplayName("clear()")
    class ClearTests {
        
        @Test
        @DisplayName("Clears all includes")
        void testClear() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude("types.hrl");
            container.addIncludeLib("lib/types.hrl");
            
            assertTrue(container.hasIncludes());
            
            container.clear();
            
            assertFalse(container.hasIncludes());
            assertTrue(container.getIncludes().isEmpty());
            assertTrue(container.getIncludeLibs().isEmpty());
        }
    }
    
    // ========== toString Tests ==========
    
    @Nested
    @DisplayName("toString()")
    class ToStringTests {
        
        @Test
        @DisplayName("Returns empty string when no includes")
        void testEmptyToString() {
            ErlangImportContainer container = new ErlangImportContainer();
            assertEquals("", container.toString());
        }
        
        @Test
        @DisplayName("Generates local include directive")
        void testLocalIncludeDirective() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude("types.hrl");
            
            String output = container.toString();
            assertTrue(output.contains("-include(\"types.hrl\")."));
        }
        
        @Test
        @DisplayName("Generates library include directive")
        void testLibIncludeDirective() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addIncludeLib("jsx/include/jsx.hrl");
            
            String output = container.toString();
            assertTrue(output.contains("-include_lib(\"jsx/include/jsx.hrl\")."));
        }
        
        @Test
        @DisplayName("Local includes come before library includes")
        void testIncludeOrder() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addIncludeLib("lib/types.hrl");
            container.addInclude("local.hrl");
            
            String output = container.toString();
            int localPos = output.indexOf("-include(\"local.hrl\")");
            int libPos = output.indexOf("-include_lib(\"lib/types.hrl\")");
            
            assertTrue(localPos < libPos, "Local includes should come before library includes");
        }
        
        @Test
        @DisplayName("Includes are sorted alphabetically")
        void testIncludesSorted() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude("z_types.hrl");
            container.addInclude("a_types.hrl");
            container.addInclude("m_types.hrl");
            
            String output = container.toString();
            int aPos = output.indexOf("a_types.hrl");
            int mPos = output.indexOf("m_types.hrl");
            int zPos = output.indexOf("z_types.hrl");
            
            assertTrue(aPos < mPos && mPos < zPos, "Includes should be sorted alphabetically");
        }
        
        @Test
        @DisplayName("Generates multiple include directives")
        void testMultipleIncludes() {
            ErlangImportContainer container = new ErlangImportContainer();
            container.addInclude("types.hrl");
            container.addInclude("records.hrl");
            container.addIncludeLib("jsx/jsx.hrl");
            
            String output = container.toString();
            
            assertTrue(output.contains("-include(\"types.hrl\")."));
            assertTrue(output.contains("-include(\"records.hrl\")."));
            assertTrue(output.contains("-include_lib(\"jsx/jsx.hrl\")."));
        }
    }
    
    // ========== Integration Tests ==========
    
    @Nested
    @DisplayName("Integration")
    class IntegrationTests {
        
        @Test
        @DisplayName("Works with ErlangWriter")
        void testWithErlangWriter() {
            ErlangWriter writer = new ErlangWriter("my_module");
            writer.addInclude("types.hrl");
            writer.addIncludeLib("jsx/jsx.hrl");
            
            writer.writeModuleHeader();
            writer.writeIncludes();
            writer.writeExports("foo/1");
            
            String output = writer.toString();
            
            assertTrue(output.contains("-module(my_module)."));
            assertTrue(output.contains("-include(\"types.hrl\")."));
            assertTrue(output.contains("-include_lib(\"jsx/jsx.hrl\")."));
            assertTrue(output.contains("-export(["));
        }
    }
}
