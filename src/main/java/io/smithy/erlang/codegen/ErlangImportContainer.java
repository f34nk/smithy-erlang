package io.smithy.erlang.codegen;

import software.amazon.smithy.codegen.core.ImportContainer;
import software.amazon.smithy.codegen.core.Symbol;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Import container for Erlang code generation.
 * 
 * <p>Erlang doesn't have traditional imports like Java or Python. Instead,
 * module dependencies are handled through:
 * <ul>
 *   <li>{@code -include("file.hrl")} for header files</li>
 *   <li>{@code -include_lib("app/include/file.hrl")} for library headers</li>
 *   <li>Fully qualified module calls: {@code module:function()}</li>
 * </ul>
 * 
 * <p>This container tracks include directives that may be needed in generated
 * code. These includes are rendered at the top of the generated module.
 * 
 * <p>Example usage:
 * <pre>
 * ErlangImportContainer imports = new ErlangImportContainer();
 * imports.addInclude("my_types.hrl");
 * imports.addIncludeLib("jsx/include/jsx.hrl");
 * 
 * // Later in code generation:
 * String includeDirectives = imports.toString();
 * </pre>
 * 
 * @see ErlangWriter
 */
public final class ErlangImportContainer implements ImportContainer {
    
    /** Local include files (for -include("file.hrl") directives). */
    private final Set<String> includes = new HashSet<>();
    
    /** Library include files (for -include_lib("path/file.hrl") directives). */
    private final Set<String> includeLibs = new HashSet<>();
    
    /**
     * Creates a new empty import container.
     */
    public ErlangImportContainer() {
        // Empty constructor
    }
    
    /**
     * Imports a symbol.
     * 
     * <p>For Erlang, this is typically a no-op since Erlang doesn't have
     * traditional imports. However, if the symbol has an "includeFile" property,
     * it will be tracked as an include directive.
     *
     * @param symbol The symbol to import
     * @param alias The alias (ignored for Erlang)
     */
    @Override
    public void importSymbol(Symbol symbol, String alias) {
        // Check if symbol has include file property
        symbol.getProperty("includeFile", String.class)
                .ifPresent(this::addInclude);
        
        symbol.getProperty("includeLibFile", String.class)
                .ifPresent(this::addIncludeLib);
    }
    
    /**
     * Adds a local include directive.
     * 
     * <p>This generates: {@code -include("filename.hrl").}
     *
     * @param filename The header file name (e.g., "my_types.hrl")
     */
    public void addInclude(String filename) {
        if (filename != null && !filename.isEmpty()) {
            includes.add(filename);
        }
    }
    
    /**
     * Adds a library include directive.
     * 
     * <p>This generates: {@code -include_lib("app/include/file.hrl").}
     *
     * @param path The library include path (e.g., "jsx/include/jsx.hrl")
     */
    public void addIncludeLib(String path) {
        if (path != null && !path.isEmpty()) {
            includeLibs.add(path);
        }
    }
    
    /**
     * Checks if any includes have been added.
     *
     * @return true if there are includes, false otherwise
     */
    public boolean hasIncludes() {
        return !includes.isEmpty() || !includeLibs.isEmpty();
    }
    
    /**
     * Gets the local includes.
     *
     * @return Set of local include filenames
     */
    public Set<String> getIncludes() {
        return Set.copyOf(includes);
    }
    
    /**
     * Gets the library includes.
     *
     * @return Set of library include paths
     */
    public Set<String> getIncludeLibs() {
        return Set.copyOf(includeLibs);
    }
    
    /**
     * Clears all includes.
     */
    public void clear() {
        includes.clear();
        includeLibs.clear();
    }
    
    /**
     * Returns the include directives as Erlang code.
     * 
     * <p>Example output:
     * <pre>
     * -include("my_types.hrl").
     * -include_lib("jsx/include/jsx.hrl").
     * </pre>
     *
     * @return String containing all include directives, or empty string if none
     */
    @Override
    public String toString() {
        if (!hasIncludes()) {
            return "";
        }
        
        StringBuilder sb = new StringBuilder();
        
        // Add local includes first (sorted for deterministic output)
        includes.stream()
                .sorted()
                .forEach(inc -> sb.append("-include(\"").append(inc).append("\").\n"));
        
        // Add library includes (sorted for deterministic output)
        includeLibs.stream()
                .sorted()
                .forEach(inc -> sb.append("-include_lib(\"").append(inc).append("\").\n"));
        
        return sb.toString();
    }
}
