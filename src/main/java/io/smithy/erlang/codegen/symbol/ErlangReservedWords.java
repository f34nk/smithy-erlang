package io.smithy.erlang.codegen.symbol;

import software.amazon.smithy.codegen.core.ReservedWords;
import software.amazon.smithy.codegen.core.ReservedWordsBuilder;

import java.util.Collections;
import java.util.Set;

/**
 * Erlang reserved words and naming escapes.
 * 
 * <p>This class provides utilities for handling Erlang reserved words
 * in generated code. It implements the Smithy {@link ReservedWords}
 * interface for integration with the code generation framework.
 * 
 * <p>Erlang reserved words cannot be used as identifiers (function names,
 * variable names, atom names, etc.) and must be escaped. This class
 * escapes reserved words by appending an underscore suffix.
 * 
 * <p>Example usage:
 * <pre>
 * // Check if a word is reserved
 * if (ErlangReservedWords.isReserved("case")) {
 *     // Handle reserved word
 * }
 * 
 * // Get a ReservedWords instance for use with Smithy
 * ReservedWords reserved = ErlangReservedWords.provider();
 * String escaped = reserved.escape("case"); // Returns "case_"
 * 
 * // Escape a word directly
 * String escaped = ErlangReservedWords.escape("end"); // Returns "end_"
 * </pre>
 * 
 * <p>The complete list of Erlang reserved words:
 * <pre>
 * after, and, andalso, band, begin, bnot, bor, bsl, bsr, bxor,
 * case, catch, cond, div, end, fun, if, let, not, of, or,
 * orelse, receive, rem, try, when, xor
 * </pre>
 * 
 * @see ReservedWords
 * @see EnhancedErlangSymbolProvider
 */
public final class ErlangReservedWords {
    
    /** The escape suffix appended to reserved words. */
    public static final String ESCAPE_SUFFIX = "_";
    
    /**
     * The complete set of Erlang reserved words.
     * 
     * <p>These are keywords that cannot be used as identifiers in Erlang code.
     */
    private static final Set<String> RESERVED = Set.of(
        "after",    // Used in receive timeouts
        "and",      // Boolean operator (deprecated, use andalso)
        "andalso",  // Short-circuit boolean AND
        "band",     // Bitwise AND
        "begin",    // Begin expression block
        "bnot",     // Bitwise NOT
        "bor",      // Bitwise OR
        "bsl",      // Bitwise shift left
        "bsr",      // Bitwise shift right
        "bxor",     // Bitwise XOR
        "case",     // Case expression
        "catch",    // Exception handling
        "cond",     // Conditional (reserved for future use)
        "div",      // Integer division
        "end",      // End block
        "fun",      // Anonymous function
        "if",       // If expression
        "let",      // Let expression (reserved for future use)
        "not",      // Boolean NOT
        "of",       // Used with case/receive
        "or",       // Boolean operator (deprecated, use orelse)
        "orelse",   // Short-circuit boolean OR
        "receive",  // Message receive
        "rem",      // Remainder (modulo)
        "try",      // Exception handling
        "when",     // Guard expression
        "xor"       // Boolean XOR
    );
    
    /** Cached ReservedWords instance. */
    private static volatile ReservedWords cachedProvider;
    
    private ErlangReservedWords() {
        // Prevent instantiation
    }
    
    /**
     * Creates a Smithy {@link ReservedWords} provider for Erlang.
     * 
     * <p>The returned provider can be used with Smithy's code generation
     * framework to automatically escape reserved words in generated identifiers.
     * 
     * <p>Reserved words are escaped by appending an underscore suffix.
     * For example, "case" becomes "case_".
     *
     * @return A ReservedWords instance for Erlang
     */
    public static ReservedWords provider() {
        if (cachedProvider == null) {
            synchronized (ErlangReservedWords.class) {
                if (cachedProvider == null) {
                    ReservedWordsBuilder builder = new ReservedWordsBuilder();
                    RESERVED.forEach(word -> builder.put(word, word + ESCAPE_SUFFIX));
                    cachedProvider = builder.build();
                }
            }
        }
        return cachedProvider;
    }
    
    /**
     * Checks if a word is an Erlang reserved word.
     *
     * @param word The word to check (case-sensitive)
     * @return true if the word is reserved, false otherwise
     */
    public static boolean isReserved(String word) {
        return word != null && RESERVED.contains(word);
    }
    
    /**
     * Escapes a word if it is reserved.
     * 
     * <p>If the word is a reserved word, an underscore suffix is appended.
     * Otherwise, the word is returned unchanged.
     *
     * @param word The word to escape
     * @return The escaped word, or the original if not reserved
     */
    public static String escape(String word) {
        if (isReserved(word)) {
            return word + ESCAPE_SUFFIX;
        }
        return word;
    }
    
    /**
     * Gets the unmodifiable set of Erlang reserved words.
     *
     * @return The set of reserved words
     */
    public static Set<String> getReservedWords() {
        return Collections.unmodifiableSet(RESERVED);
    }
    
    /**
     * Gets the number of reserved words.
     *
     * @return The count of reserved words
     */
    public static int count() {
        return RESERVED.size();
    }
}
