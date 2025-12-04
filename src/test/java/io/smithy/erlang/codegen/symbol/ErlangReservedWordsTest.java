package io.smithy.erlang.codegen.symbol;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import software.amazon.smithy.codegen.core.ReservedWords;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for ErlangReservedWords.
 */
class ErlangReservedWordsTest {
    
    // ========== isReserved() Tests ==========
    
    @Nested
    @DisplayName("isReserved()")
    class IsReservedTests {
        
        @ParameterizedTest
        @ValueSource(strings = {
            "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl",
            "bsr", "bxor", "case", "catch", "cond", "div", "end", "fun",
            "if", "let", "not", "of", "or", "orelse", "receive", "rem",
            "try", "when", "xor"
        })
        @DisplayName("Returns true for reserved words")
        void testAllReservedWords(String word) {
            assertTrue(ErlangReservedWords.isReserved(word),
                    "Expected '" + word + "' to be reserved");
        }
        
        @Test
        @DisplayName("Returns false for non-reserved words")
        void testNonReservedWords() {
            assertFalse(ErlangReservedWords.isReserved("my_function"));
            assertFalse(ErlangReservedWords.isReserved("user"));
            assertFalse(ErlangReservedWords.isReserved("get_item"));
            assertFalse(ErlangReservedWords.isReserved("list_buckets"));
        }
        
        @Test
        @DisplayName("Returns false for null")
        void testNull() {
            assertFalse(ErlangReservedWords.isReserved(null));
        }
        
        @Test
        @DisplayName("Returns false for empty string")
        void testEmptyString() {
            assertFalse(ErlangReservedWords.isReserved(""));
        }
        
        @Test
        @DisplayName("Is case-sensitive")
        void testCaseSensitive() {
            assertTrue(ErlangReservedWords.isReserved("case"));
            assertFalse(ErlangReservedWords.isReserved("Case"));
            assertFalse(ErlangReservedWords.isReserved("CASE"));
        }
    }
    
    // ========== escape() Tests ==========
    
    @Nested
    @DisplayName("escape()")
    class EscapeTests {
        
        @Test
        @DisplayName("Escapes reserved words with underscore suffix")
        void testEscapeReservedWords() {
            assertEquals("case_", ErlangReservedWords.escape("case"));
            assertEquals("end_", ErlangReservedWords.escape("end"));
            assertEquals("if_", ErlangReservedWords.escape("if"));
            assertEquals("receive_", ErlangReservedWords.escape("receive"));
            assertEquals("try_", ErlangReservedWords.escape("try"));
            assertEquals("when_", ErlangReservedWords.escape("when"));
        }
        
        @Test
        @DisplayName("Returns non-reserved words unchanged")
        void testNonReservedUnchanged() {
            assertEquals("my_function", ErlangReservedWords.escape("my_function"));
            assertEquals("user", ErlangReservedWords.escape("user"));
            assertEquals("get_item", ErlangReservedWords.escape("get_item"));
        }
        
        @Test
        @DisplayName("Handles null by returning null")
        void testNullReturnsNull() {
            assertNull(ErlangReservedWords.escape(null));
        }
        
        @Test
        @DisplayName("Returns empty string unchanged")
        void testEmptyStringUnchanged() {
            assertEquals("", ErlangReservedWords.escape(""));
        }
    }
    
    // ========== provider() Tests ==========
    
    @Nested
    @DisplayName("provider()")
    class ProviderTests {
        
        @Test
        @DisplayName("Returns a non-null ReservedWords instance")
        void testProviderNotNull() {
            ReservedWords provider = ErlangReservedWords.provider();
            assertNotNull(provider);
        }
        
        @Test
        @DisplayName("Provider escapes reserved words")
        void testProviderEscapesReservedWords() {
            ReservedWords provider = ErlangReservedWords.provider();
            
            assertEquals("case_", provider.escape("case"));
            assertEquals("end_", provider.escape("end"));
            assertEquals("if_", provider.escape("if"));
        }
        
        @Test
        @DisplayName("Provider returns non-reserved words unchanged")
        void testProviderLeavesNonReserved() {
            ReservedWords provider = ErlangReservedWords.provider();
            
            assertEquals("my_function", provider.escape("my_function"));
            assertEquals("user", provider.escape("user"));
        }
        
        @Test
        @DisplayName("Provider is cached (same instance returned)")
        void testProviderIsCached() {
            ReservedWords first = ErlangReservedWords.provider();
            ReservedWords second = ErlangReservedWords.provider();
            
            assertSame(first, second, "Provider should return cached instance");
        }
        
        @Test
        @DisplayName("Provider isReserved works correctly")
        void testProviderIsReserved() {
            ReservedWords provider = ErlangReservedWords.provider();
            
            assertTrue(provider.isReserved("case"));
            assertTrue(provider.isReserved("end"));
            assertFalse(provider.isReserved("my_function"));
        }
    }
    
    // ========== getReservedWords() Tests ==========
    
    @Nested
    @DisplayName("getReservedWords()")
    class GetReservedWordsTests {
        
        @Test
        @DisplayName("Returns all 27 reserved words")
        void testCount() {
            Set<String> reserved = ErlangReservedWords.getReservedWords();
            assertEquals(27, reserved.size());
        }
        
        @Test
        @DisplayName("Contains all expected words")
        void testContainsAllWords() {
            Set<String> reserved = ErlangReservedWords.getReservedWords();
            
            assertTrue(reserved.contains("after"));
            assertTrue(reserved.contains("case"));
            assertTrue(reserved.contains("end"));
            assertTrue(reserved.contains("fun"));
            assertTrue(reserved.contains("if"));
            assertTrue(reserved.contains("receive"));
            assertTrue(reserved.contains("try"));
            assertTrue(reserved.contains("when"));
        }
        
        @Test
        @DisplayName("Returns unmodifiable set")
        void testUnmodifiable() {
            Set<String> reserved = ErlangReservedWords.getReservedWords();
            
            assertThrows(UnsupportedOperationException.class, () -> {
                reserved.add("new_word");
            });
        }
    }
    
    // ========== count() Tests ==========
    
    @Nested
    @DisplayName("count()")
    class CountTests {
        
        @Test
        @DisplayName("Returns 27")
        void testCount() {
            assertEquals(27, ErlangReservedWords.count());
        }
        
        @Test
        @DisplayName("Matches getReservedWords size")
        void testMatchesSetSize() {
            assertEquals(ErlangReservedWords.getReservedWords().size(), 
                        ErlangReservedWords.count());
        }
    }
    
    // ========== ESCAPE_SUFFIX Tests ==========
    
    @Nested
    @DisplayName("ESCAPE_SUFFIX")
    class EscapeSuffixTests {
        
        @Test
        @DisplayName("Is underscore")
        void testIsUnderscore() {
            assertEquals("_", ErlangReservedWords.ESCAPE_SUFFIX);
        }
    }
    
    // ========== Integration Tests ==========
    
    @Nested
    @DisplayName("Integration")
    class IntegrationTests {
        
        @Test
        @DisplayName("All reserved words are properly escaped")
        void testAllReservedWordsEscaped() {
            for (String word : ErlangReservedWords.getReservedWords()) {
                String escaped = ErlangReservedWords.escape(word);
                assertEquals(word + "_", escaped,
                        "Expected '" + word + "' to be escaped to '" + word + "_'");
            }
        }
        
        @Test
        @DisplayName("Provider and static methods are consistent")
        void testConsistency() {
            ReservedWords provider = ErlangReservedWords.provider();
            
            for (String word : ErlangReservedWords.getReservedWords()) {
                // Static isReserved matches provider isReserved
                assertEquals(ErlangReservedWords.isReserved(word), 
                            provider.isReserved(word));
                
                // Static escape matches provider escape
                assertEquals(ErlangReservedWords.escape(word),
                            provider.escape(word));
            }
        }
        
        @Test
        @DisplayName("Works with EnhancedErlangSymbolProvider reserved words")
        void testCompatibilityWithSymbolProvider() {
            // Verify the same set of words
            Set<String> fromReservedWords = ErlangReservedWords.getReservedWords();
            Set<String> fromSymbolProvider = EnhancedErlangSymbolProvider.getReservedWords();
            
            assertEquals(fromReservedWords, fromSymbolProvider,
                    "Reserved words should be consistent between classes");
        }
    }
}
