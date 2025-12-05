package io.smithy.erlang.codegen;

import software.amazon.smithy.codegen.core.Symbol;
import software.amazon.smithy.codegen.core.SymbolWriter;

import java.util.function.BiFunction;

/**
 * Erlang code writer with Symbol support.
 * 
 * <p>This class extends {@link SymbolWriter} to provide Erlang-specific
 * code generation utilities. It includes custom formatters for working
 * with Smithy Symbols and helper methods for generating Erlang module
 * structure elements.
 * 
 * <h2>Custom Formatters</h2>
 * <ul>
 *   <li>{@code $T} - Formats a Symbol's Erlang type (from "erlangType" property)</li>
 *   <li>{@code $N} - Formats a Symbol's name</li>
 * </ul>
 * 
 * <h2>Example Usage</h2>
 * <pre>
 * ErlangWriter writer = new ErlangWriter("my_module");
 * 
 * writer.writeModuleHeader()
 *       .writeExports("foo/1", "bar/2")
 *       .writeBlankLine()
 *       .writeSpec("foo", "binary()", "{ok, term()}")
 *       .openFunction("foo", "Input")
 *       .write("{ok, Input}.")
 *       .closeFunction();
 * </pre>
 * 
 * @see ErlangImportContainer
 * @see SymbolWriter
 */
public final class ErlangWriter extends SymbolWriter<ErlangWriter, ErlangImportContainer> {
    
    /** Default indentation string (4 spaces for Erlang). */
    private static final String DEFAULT_INDENT = "    ";
    
    /** The module name for this writer. */
    private final String moduleName;
    
    /**
     * Creates a new ErlangWriter for the specified module.
     *
     * @param moduleName The Erlang module name (without .erl extension)
     */
    public ErlangWriter(String moduleName) {
        super(new ErlangImportContainer());
        this.moduleName = moduleName;
        
        // Set indentation
        setIndentText(DEFAULT_INDENT);
        
        // Register custom formatters
        putFormatter('T', new ErlangTypeFormatter());
        putFormatter('N', new ErlangNameFormatter());
    }
    
    /**
     * Gets the module name for this writer.
     *
     * @return The module name
     */
    public String getModuleName() {
        return moduleName;
    }
    
    // ========== Module Structure Methods ==========
    
    /**
     * Writes a blank line without any indentation.
     * 
     * <p>This should be used instead of {@code write("")} when a truly
     * blank line is needed, as {@code write("")} would include the
     * current indentation level, causing trailing whitespace.
     *
     * @return This writer for method chaining
     */
    public ErlangWriter writeBlankLine() {
        // Use dedent/indent to temporarily remove indentation for a clean blank line
        int currentIndent = getIndentLevel();
        dedent(currentIndent);
        write("");
        indent(currentIndent);
        return this;
    }
    
    /**
     * Writes the module header declaration.
     * 
     * <p>Generates: {@code -module(module_name).}
     *
     * @return This writer for method chaining
     */
    public ErlangWriter writeModuleHeader() {
        write("-module($L).", moduleName);
        writeBlankLine();
        return this;
    }
    
    /**
     * Writes a module header with a custom module name.
     * 
     * <p>Generates: {@code -module(custom_name).}
     *
     * @param customModuleName The module name to use
     * @return This writer for method chaining
     */
    public ErlangWriter writeModuleHeader(String customModuleName) {
        write("-module($L).", customModuleName);
        writeBlankLine();
        return this;
    }
    
    /**
     * Writes function exports.
     * 
     * <p>Example output:
     * <pre>
     * -export([
     *     foo/1,
     *     bar/2
     * ]).
     * </pre>
     *
     * @param functions Function signatures (e.g., "foo/1", "bar/2")
     * @return This writer for method chaining
     */
    public ErlangWriter writeExports(String... functions) {
        if (functions.length == 0) {
            write("-export([]).");
            return this;
        }
        
        write("-export([");
        indent();
        for (int i = 0; i < functions.length; i++) {
            String comma = (i < functions.length - 1) ? "," : "";
            write("$L$L", functions[i], comma);
        }
        dedent();
        write("]).");
        writeBlankLine();
        return this;
    }
    
    // ========== Type Definition Methods ==========
    
    /**
     * Writes a type definition.
     * 
     * <p>Generates: {@code -type name() :: typeSpec.}
     *
     * @param name The type name
     * @param typeSpec The type specification
     * @return This writer for method chaining
     */
    public ErlangWriter writeType(String name, String typeSpec) {
        write("-type $L() :: $L.", name, typeSpec);
        return this;
    }
    
    /**
     * Writes an opaque type definition.
     * 
     * <p>Generates: {@code -opaque name() :: typeSpec.}
     *
     * @param name The type name
     * @param typeSpec The type specification
     * @return This writer for method chaining
     */
    public ErlangWriter writeOpaqueType(String name, String typeSpec) {
        write("-opaque $L() :: $L.", name, typeSpec);
        return this;
    }
    
    /**
     * Writes a type export.
     * 
     * <p>Generates: {@code -export_type([name/arity]).}
     *
     * @param name The type name
     * @param arity The type arity
     * @return This writer for method chaining
     */
    public ErlangWriter writeExportType(String name, int arity) {
        write("-export_type([$L/$L]).", name, arity);
        return this;
    }
    
    /**
     * Writes multiple type exports.
     * 
     * <p>Example output:
     * <pre>
     * -export_type([
     *     foo/0,
     *     bar/1
     * ]).
     * </pre>
     *
     * @param types Type specifications (e.g., "foo/0", "bar/1")
     * @return This writer for method chaining
     */
    public ErlangWriter writeExportTypes(String... types) {
        if (types.length == 0) {
            return this;
        }
        
        if (types.length == 1) {
            write("-export_type([$L]).", types[0]);
            return this;
        }
        
        write("-export_type([");
        indent();
        for (int i = 0; i < types.length; i++) {
            String comma = (i < types.length - 1) ? "," : "";
            write("$L$L", types[i], comma);
        }
        dedent();
        write("]).");
        return this;
    }
    
    // ========== Function Definition Methods ==========
    
    /** Maximum line length before breaking spec onto multiple lines. */
    private static final int MAX_SPEC_LINE_LENGTH = 100;
    
    /**
     * Writes a function specification.
     * 
     * <p>Generates: {@code -spec functionName(argTypes) -> returnType.}
     * 
     * <p>If the spec line exceeds {@value #MAX_SPEC_LINE_LENGTH} characters,
     * it will be broken across two lines with the return type indented.
     *
     * @param functionName The function name
     * @param argTypes The argument types (e.g., "binary(), integer()")
     * @param returnType The return type (e.g., "{ok, term()} | {error, term()}")
     * @return This writer for method chaining
     */
    public ErlangWriter writeSpec(String functionName, String argTypes, String returnType) {
        String specLine = String.format("-spec %s(%s) -> %s.", functionName, argTypes, returnType);
        if (specLine.length() > MAX_SPEC_LINE_LENGTH) {
            write("-spec $L($L) ->", functionName, argTypes);
            write("    $L.", returnType);
        } else {
            write("-spec $L($L) -> $L.", functionName, argTypes, returnType);
        }
        return this;
    }
    
    /**
     * Writes a function specification with no arguments.
     * 
     * <p>Generates: {@code -spec functionName() -> returnType.}
     *
     * @param functionName The function name
     * @param returnType The return type
     * @return This writer for method chaining
     */
    public ErlangWriter writeSpec(String functionName, String returnType) {
        write("-spec $L() -> $L.", functionName, returnType);
        return this;
    }
    
    /**
     * Opens a function definition.
     * 
     * <p>Generates the function head and increases indentation.
     * Must be paired with {@link #closeFunction()}.
     * 
     * <p>Example:
     * <pre>
     * writer.openFunction("foo", "Input, Options")
     *       .write("Result = process(Input),")
     *       .write("{ok, Result}.")
     *       .closeFunction();
     * </pre>
     *
     * @param name The function name
     * @param params The function parameters
     * @return This writer for method chaining
     */
    public ErlangWriter openFunction(String name, String params) {
        write("$L($L) ->", name, params);
        indent();
        return this;
    }
    
    /**
     * Opens a function definition with no parameters.
     *
     * @param name The function name
     * @return This writer for method chaining
     */
    public ErlangWriter openFunction(String name) {
        write("$L() ->", name);
        indent();
        return this;
    }
    
    /**
     * Closes a function definition.
     * 
     * <p>Decreases indentation and adds a blank line.
     *
     * @return This writer for method chaining
     */
    public ErlangWriter closeFunction() {
        dedent();
        writeBlankLine();
        return this;
    }
    
    /**
     * Writes a complete simple function.
     * 
     * <p>For more complex functions, use {@link #openFunction(String, String)}
     * and {@link #closeFunction()}.
     *
     * @param name The function name
     * @param params The function parameters
     * @param body A runnable that writes the function body
     * @return This writer for method chaining
     */
    public ErlangWriter writeFunction(String name, String params, Runnable body) {
        write("$L($L) ->", name, params);
        indent();
        body.run();
        dedent();
        writeBlankLine();
        return this;
    }
    
    // ========== Common Code Pattern Methods ==========
    
    /**
     * Writes the standard retry case expression.
     * 
     * <p>Generates:
     * <pre>
     * case maps:get(enable_retry, Options, true) of
     *     true -> aws_retry:with_retry(RequestFun, Options);
     *     false -> RequestFun()
     * end.
     * </pre>
     *
     * @return This writer for method chaining
     */
    public ErlangWriter writeRetryCase() {
        write("case maps:get(enable_retry, Options, true) of");
        indent();
        write("true -> aws_retry:with_retry(RequestFun, Options);");
        write("false -> RequestFun()");
        dedent();
        write("end.");
        return this;
    }
    
    /**
     * Writes a formatted httpc:request case expression.
     * 
     * <p>Generates:
     * <pre>
     * case
     *     httpc:request(method, Request, [], [
     *         {body_format, binary}
     *     ])
     * of
     *     {ok, {{_, StatusCode, _}, _RespHeaders, ResponseBody}} when StatusCode >= 200, StatusCode < 300 ->
     *         ... successHandler ...
     *     {ok, {{_, StatusCode, _}, _RespHeaders, ErrorBody}} ->
     *         ... errorHandler ...
     *     {error, Reason} ->
     *         {error, {http_error, Reason}}
     * end;
     * </pre>
     *
     * @param method The HTTP method expression (e.g., "post" or "binary_to_atom(string:lowercase(Method), utf8)")
     * @param successHandler Runnable that writes the success clause body
     * @param errorHandler Runnable that writes the error clause body
     * @return This writer for method chaining
     */
    public ErlangWriter writeHttpcCase(String method, Runnable successHandler, Runnable errorHandler) {
        write("case");
        indent();
        write("httpc:request($L, Request, [], [", method);
        indent();
        write("{body_format, binary}");
        dedent();
        write("])");
        dedent();
        write("of");
        indent();
        write("{ok, {{_, StatusCode, _}, _RespHeaders, ResponseBody}} when StatusCode >= 200, StatusCode < 300 ->");
        indent();
        successHandler.run();
        dedent();
        write("{ok, {{_, StatusCode, _}, _RespHeaders, ErrorBody}} ->");
        indent();
        errorHandler.run();
        dedent();
        write("{error, Reason} ->");
        indent();
        write("{error, {http_error, Reason}}");
        dedent();
        dedent();
        write("end;");
        return this;
    }
    
    /**
     * Writes the sign-and-send block structure for AWS requests.
     * 
     * <p>This helper generates the complete structure for signing a request
     * with SigV4 and sending it via httpc. It handles:
     * <ul>
     *   <li>Sign request with aws_sigv4</li>
     *   <li>Convert headers to string format</li>
     *   <li>Build the Request tuple</li>
     *   <li>Make the httpc:request call</li>
     *   <li>Handle success, error, and HTTP error responses</li>
     *   <li>Handle signing errors</li>
     * </ul>
     * 
     * <p>Generates:
     * <pre>
     * %% Sign and send request
     * case aws_sigv4:sign_request(Method, Url, Headers, Body, Client) of
     *     {ok, SignedHeaders} ->
     *         ContentType = "...",
     *         StringHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- SignedHeaders],
     *         ... requestBuilder ...
     *         
     *         case httpc:request(...) of
     *             ...
     *         end;
     *     {error, SignError} ->
     *         {error, {signing_error, SignError}}
     * end.
     * </pre>
     *
     * @param contentType The Content-Type for the request (e.g., "application/xml")
     * @param bodyVariable The name of the body variable (e.g., "Body" or "Payload")
     * @param httpMethod The HTTP method expression (e.g., "post" or "binary_to_atom(string:lowercase(Method), utf8)")
     * @param requestBuilder Runnable that writes the Request building code (e.g., "Request = {...}")
     * @param successHandler Runnable that writes the success response handling code
     * @param errorHandler Runnable that writes the error response handling code
     * @return This writer for method chaining
     */
    public ErlangWriter writeSignAndSendBlock(
            String contentType,
            String bodyVariable,
            String httpMethod,
            Runnable requestBuilder,
            Runnable successHandler,
            Runnable errorHandler) {
        
        write("%% Sign and send request");
        write("case aws_sigv4:sign_request(Method, Url, Headers, $L, Client) of", bodyVariable);
        indent();
        write("{ok, SignedHeaders} ->");
        indent();
        write("ContentType = \"$L\",", contentType);
        write("StringHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- SignedHeaders],");
        requestBuilder.run();
        writeBlankLine();
        writeHttpcCase(httpMethod, successHandler, errorHandler);
        dedent();
        write("{error, SignError} ->");
        indent();
        write("{error, {signing_error, SignError}}");
        dedent();
        dedent();
        write("end.");
        return this;
    }
    
    /**
     * Writes a standard XML decode case expression.
     * 
     * <p>Generates:
     * <pre>
     * case aws_xml:decode(ResponseBody) of
     *     {ok, XmlMap} -> {ok, XmlMap};
     *     {error, DecodeError} -> {error, {xml_decode_error, DecodeError}}
     * end
     * </pre>
     *
     * @return This writer for method chaining
     */
    public ErlangWriter writeXmlDecodeCase() {
        return writeXmlDecodeCase("");
    }
    
    /**
     * Writes a standard XML decode case expression with a suffix.
     * 
     * <p>Generates:
     * <pre>
     * case aws_xml:decode(ResponseBody) of
     *     {ok, XmlMap} -> {ok, XmlMap};
     *     {error, DecodeError} -> {error, {xml_decode_error, DecodeError}}
     * end{suffix}
     * </pre>
     *
     * @param suffix The suffix to append after "end" (e.g., ";" or "")
     * @return This writer for method chaining
     */
    public ErlangWriter writeXmlDecodeCase(String suffix) {
        write("case aws_xml:decode(ResponseBody) of");
        indent();
        write("{ok, XmlMap} -> {ok, XmlMap};");
        write("{error, DecodeError} -> {error, {xml_decode_error, DecodeError}}");
        dedent();
        write("end$L", suffix);
        return this;
    }
    
    /**
     * Writes XML decode with empty body handling.
     * 
     * <p>Generates:
     * <pre>
     * case ResponseBody of
     *     <<>> -> {ok, #{}};
     *     _ ->
     *         case aws_xml:decode(ResponseBody) of
     *             {ok, XmlMap} -> {ok, XmlMap};
     *             {error, DecodeError} -> {error, {xml_decode_error, DecodeError}}
     *         end
     * end
     * </pre>
     *
     * @return This writer for method chaining
     */
    public ErlangWriter writeXmlDecodeWithEmptyCheck() {
        return writeXmlDecodeWithEmptyCheck("");
    }
    
    /**
     * Writes XML decode with empty body handling and a suffix.
     * 
     * <p>Generates:
     * <pre>
     * case ResponseBody of
     *     <<>> -> {ok, #{}};
     *     _ ->
     *         case aws_xml:decode(ResponseBody) of
     *             {ok, XmlMap} -> {ok, XmlMap};
     *             {error, DecodeError} -> {error, {xml_decode_error, DecodeError}}
     *         end
     * end{suffix}
     * </pre>
     *
     * @param suffix The suffix to append after "end" (e.g., ";" or "")
     * @return This writer for method chaining
     */
    public ErlangWriter writeXmlDecodeWithEmptyCheck(String suffix) {
        write("case ResponseBody of");
        indent();
        write("<<>> -> {ok, #{}};");
        write("_ ->");
        indent();
        writeXmlDecodeCase();
        dedent();
        dedent();
        write("end$L", suffix);
        return this;
    }
    
    /**
     * Writes a standard JSON decode case expression.
     * 
     * <p>Generates:
     * <pre>
     * case jsx:decode(ResponseBody, [return_maps]) of
     *     JsonMap when is_map(JsonMap) -> {ok, JsonMap};
     *     _ -> {error, {json_decode_error, invalid_json}}
     * end
     * </pre>
     *
     * @return This writer for method chaining
     */
    public ErlangWriter writeJsonDecodeCase() {
        return writeJsonDecodeCase("");
    }
    
    /**
     * Writes a standard JSON decode case expression with a suffix.
     * 
     * <p>Generates:
     * <pre>
     * case jsx:decode(ResponseBody, [return_maps]) of
     *     JsonMap when is_map(JsonMap) -> {ok, JsonMap};
     *     _ -> {error, {json_decode_error, invalid_json}}
     * end{suffix}
     * </pre>
     *
     * @param suffix The suffix to append after "end" (e.g., ";" or "")
     * @return This writer for method chaining
     */
    public ErlangWriter writeJsonDecodeCase(String suffix) {
        write("case jsx:decode(ResponseBody, [return_maps]) of");
        indent();
        write("JsonMap when is_map(JsonMap) -> {ok, JsonMap};");
        write("_ -> {error, {json_decode_error, invalid_json}}");
        dedent();
        write("end$L", suffix);
        return this;
    }
    
    /**
     * Writes JSON decode with empty body handling.
     * 
     * <p>Generates:
     * <pre>
     * case ResponseBody of
     *     <<>> -> {ok, #{}};
     *     <<"{}"\>> -> {ok, #{}};
     *     _ ->
     *         case jsx:decode(ResponseBody, [return_maps]) of
     *             JsonMap when is_map(JsonMap) -> {ok, JsonMap};
     *             _ -> {error, {json_decode_error, invalid_json}}
     *         end
     * end
     * </pre>
     *
     * @return This writer for method chaining
     */
    public ErlangWriter writeJsonDecodeWithEmptyCheck() {
        return writeJsonDecodeWithEmptyCheck("");
    }
    
    /**
     * Writes JSON decode with empty body handling and a suffix.
     * 
     * <p>Generates:
     * <pre>
     * case ResponseBody of
     *     <<>> -> {ok, #{}};
     *     <<"{}"\>> -> {ok, #{}};
     *     _ ->
     *         case jsx:decode(ResponseBody, [return_maps]) of
     *             JsonMap when is_map(JsonMap) -> {ok, JsonMap};
     *             _ -> {error, {json_decode_error, invalid_json}}
     *         end
     * end{suffix}
     * </pre>
     *
     * @param suffix The suffix to append after "end" (e.g., ";" or "")
     * @return This writer for method chaining
     */
    public ErlangWriter writeJsonDecodeWithEmptyCheck(String suffix) {
        write("case ResponseBody of");
        indent();
        write("<<>> -> {ok, #{}};");
        write("<<\"{}\">> -> {ok, #{}};");
        write("_ ->");
        indent();
        writeJsonDecodeCase();
        dedent();
        dedent();
        write("end$L", suffix);
        return this;
    }
    
    // ========== HTTP Header Building Methods ==========
    
    /**
     * Represents a mapping from a Smithy member to an HTTP header.
     */
    public static final class HeaderMapping {
        private final String memberName;
        private final String headerName;
        private final String varName;
        private final boolean required;
        
        /**
         * Creates a new HeaderMapping.
         *
         * @param memberName The Smithy member name (e.g., "IfMatch")
         * @param headerName The HTTP header name (e.g., "If-Match")
         * @param varName The Erlang variable name prefix (e.g., "IfMatch")
         * @param required Whether the header is required
         */
        public HeaderMapping(String memberName, String headerName, String varName, boolean required) {
            this.memberName = memberName;
            this.headerName = headerName;
            this.varName = varName;
            this.required = required;
        }
        
        /**
         * Creates a HeaderMapping with the member name used as the variable name.
         */
        public static HeaderMapping of(String memberName, String headerName, boolean required) {
            return new HeaderMapping(memberName, headerName, capitalize(memberName), required);
        }
        
        public String memberName() { return memberName; }
        public String headerName() { return headerName; }
        public String varName() { return varName; }
        public boolean required() { return required; }
        
        private static String capitalize(String s) {
            if (s == null || s.isEmpty()) return s;
            return Character.toUpperCase(s.charAt(0)) + s.substring(1);
        }
    }
    
    /**
     * Writes a header building chain for HTTP headers.
     * 
     * <p>For empty headers, generates:
     * <pre>
     * %% Headers
     * Headers = [{<<"Content-Type">>, <<"application/xml">>}],
     * </pre>
     * 
     * <p>For headers with members, generates:
     * <pre>
     * %% Build headers with @httpHeader members
     * Headers0 = [{<<"Content-Type">>, <<"application/xml">>}],
     * Headers1 = case maps:get(<<"IfMatch">>, Input, undefined) of
     *     undefined -> Headers0;
     *     IfMatchValue -> [{<<"If-Match">>, ensure_binary(IfMatchValue)} | Headers0]
     * end,
     * Headers = Headers1,
     * </pre>
     *
     * @param contentType The Content-Type header value (e.g., "application/xml")
     * @param headers List of header mappings
     * @return This writer for method chaining
     */
    public ErlangWriter writeHeaderBuildingChain(String contentType, java.util.List<HeaderMapping> headers) {
        if (headers.isEmpty()) {
            write("%% Headers");
            write("Headers = [{<<\"Content-Type\">>, <<\"$L\">>}],", contentType);
            return this;
        }
        
        write("%% Build headers with @httpHeader members");
        write("Headers0 = [{<<\"Content-Type\">>, <<\"$L\">>}],", contentType);
        
        for (int i = 0; i < headers.size(); i++) {
            HeaderMapping header = headers.get(i);
            String currentHeadersVar = "Headers" + i;
            String nextHeadersVar = "Headers" + (i + 1);
            
            if (header.required()) {
                write("$LValue = ensure_binary(maps:get(<<\"$L\">>, Input)),",
                        header.varName(), header.memberName());
                write("$L = [{<<\"$L\">>, $LValue} | $L],",
                        nextHeadersVar, header.headerName(), header.varName(), currentHeadersVar);
            } else {
                write("$L = case maps:get(<<\"$L\">>, Input, undefined) of", nextHeadersVar, header.memberName());
                indent();
                write("undefined -> $L;", currentHeadersVar);
                write("$LValue -> [{<<\"$L\">>, ensure_binary($LValue)} | $L]",
                        header.varName(), header.headerName(), header.varName(), currentHeadersVar);
                dedent();
                write("end,");
            }
        }
        
        write("Headers = $L,", "Headers" + headers.size());
        return this;
    }
    
    // ========== HTTP Query String Building Methods ==========
    
    /**
     * Represents a mapping from a Smithy member to an HTTP query parameter.
     */
    public static final class QueryParamMapping {
        private final String memberName;
        private final String queryName;
        private final String varName;
        
        /**
         * Creates a new QueryParamMapping.
         *
         * @param memberName The Smithy member name (e.g., "Delimiter")
         * @param queryName The HTTP query parameter name (e.g., "delimiter")
         * @param varName The Erlang variable name prefix (e.g., "Delimiter")
         */
        public QueryParamMapping(String memberName, String queryName, String varName) {
            this.memberName = memberName;
            this.queryName = queryName;
            this.varName = varName;
        }
        
        /**
         * Creates a QueryParamMapping with the member name used as the variable name.
         */
        public static QueryParamMapping of(String memberName, String queryName) {
            return new QueryParamMapping(memberName, queryName, capitalize(memberName));
        }
        
        public String memberName() { return memberName; }
        public String queryName() { return queryName; }
        public String varName() { return varName; }
        
        private static String capitalize(String s) {
            if (s == null || s.isEmpty()) return s;
            return Character.toUpperCase(s.charAt(0)) + s.substring(1);
        }
    }
    
    /**
     * Writes query string building code for HTTP query parameters.
     * 
     * <p>For empty query params, generates:
     * <pre>
     * %% No query parameters
     * QueryString = <<"">>,
     * </pre>
     * 
     * <p>For query params, generates:
     * <pre>
     * %% Build query string from @httpQuery parameters
     * QueryPairs0 = [],
     * QueryPairs1 = case maps:get(<<"Delimiter">>, Input, undefined) of
     *     undefined -> QueryPairs0;
     *     DelimiterVal -> [{<<"delimiter">>, ensure_binary(DelimiterVal)} | QueryPairs0]
     * end,
     * QueryString = case QueryPairs1 of
     *     [] -> <<"">>;
     *     Pairs -> Encoded = uri_string:compose_query(Pairs), <<"?", Encoded/binary>>
     * end,
     * </pre>
     *
     * @param queryParams List of query parameter mappings
     * @return This writer for method chaining
     */
    public ErlangWriter writeQueryStringBuilder(java.util.List<QueryParamMapping> queryParams) {
        if (queryParams.isEmpty()) {
            write("%% No query parameters");
            write("QueryString = <<\"\">>,");
            return this;
        }
        
        write("%% Build query string from @httpQuery parameters");
        write("QueryPairs0 = [],");
        
        for (int i = 0; i < queryParams.size(); i++) {
            QueryParamMapping param = queryParams.get(i);
            String currentPairsVar = "QueryPairs" + i;
            String nextPairsVar = "QueryPairs" + (i + 1);
            
            write("$L = case maps:get(<<\"$L\">>, Input, undefined) of", nextPairsVar, param.memberName());
            indent();
            write("undefined -> $L;", currentPairsVar);
            write("$LVal -> [{<<\"$L\">>, ensure_binary($LVal)} | $L]",
                    param.varName(), param.queryName(), param.varName(), currentPairsVar);
            dedent();
            write("end,");
        }
        
        String finalPairsVar = "QueryPairs" + queryParams.size();
        write("QueryString = case $L of", finalPairsVar);
        indent();
        write("[] -> <<\"\">>;");
        write("Pairs -> Encoded = uri_string:compose_query(Pairs), <<\"?\", Encoded/binary>>");
        dedent();
        write("end,");
        return this;
    }
    
    // ========== Record Methods ==========
    
    /**
     * Writes a record definition.
     * 
     * <p>Example output:
     * <pre>
     * -record(my_record, {
     *     field1 :: binary(),
     *     field2 :: integer()
     * }).
     * </pre>
     *
     * @param recordName The record name
     * @param fieldWriter A runnable that writes the record fields
     * @return This writer for method chaining
     */
    public ErlangWriter writeRecord(String recordName, Runnable fieldWriter) {
        write("-record($L, {", recordName);
        indent();
        fieldWriter.run();
        dedent();
        write("}).");
        writeBlankLine();
        return this;
    }
    
    // ========== Comment Methods ==========
    
    /**
     * Writes a single-line comment.
     * 
     * <p>Generates: {@code %% comment text}
     *
     * @param comment The comment text
     * @return This writer for method chaining
     */
    public ErlangWriter writeComment(String comment) {
        write("%% $L", comment);
        return this;
    }
    
    /**
     * Writes an EDoc documentation comment.
     * 
     * <p>Generates: {@code %%% @doc summary}
     *
     * @param summary The documentation summary
     * @return This writer for method chaining
     */
    public ErlangWriter writeEdocComment(String summary) {
        write("%%% @doc $L", summary);
        return this;
    }
    
    /**
     * Writes a multi-line documentation comment.
     * 
     * <p>Example output:
     * <pre>
     * %% @doc This is the summary.
     * %%
     * %% This is additional documentation
     * %% that spans multiple lines.
     * </pre>
     *
     * @param doc The documentation text (may contain newlines)
     * @return This writer for method chaining
     */
    public ErlangWriter writeDocComment(String doc) {
        String[] lines = doc.split("\n");
        for (int i = 0; i < lines.length; i++) {
            if (i == 0) {
                write("%% @doc $L", lines[i]);
            } else {
                write("%% $L", lines[i]);
            }
        }
        return this;
    }
    
    /**
     * Writes a section separator comment.
     * 
     * <p>Example output:
     * <pre>
     * %% ==========================================================
     * %% Section Title
     * %% ==========================================================
     * </pre>
     *
     * @param title The section title
     * @return This writer for method chaining
     */
    public ErlangWriter writeSectionComment(String title) {
        write("%% ==========================================================");
        write("%% $L", title);
        write("%% ==========================================================");
        writeBlankLine();
        return this;
    }
    
    // ========== Utility Methods ==========
    
    /**
     * Writes include directive(s) from the import container.
     * 
     * <p>This should be called after the module header and before exports.
     *
     * @return This writer for method chaining
     */
    public ErlangWriter writeIncludes() {
        ErlangImportContainer imports = getImportContainer();
        if (imports.hasIncludes()) {
            writeWithNoFormatting(imports.toString());
            writeBlankLine();
        }
        return this;
    }
    
    /**
     * Adds a local include to this writer's import container.
     *
     * @param filename The header file to include
     * @return This writer for method chaining
     */
    public ErlangWriter addInclude(String filename) {
        getImportContainer().addInclude(filename);
        return this;
    }
    
    /**
     * Adds a library include to this writer's import container.
     *
     * @param path The library header path to include
     * @return This writer for method chaining
     */
    public ErlangWriter addIncludeLib(String path) {
        getImportContainer().addIncludeLib(path);
        return this;
    }
    
    // ========== Symbol-Aware Methods ==========
    
    /**
     * Writes a type definition using a Symbol.
     * 
     * <p>Uses the Symbol's "erlangType" property if available.
     *
     * @param name The type name
     * @param symbol The symbol providing the type specification
     * @return This writer for method chaining
     */
    public ErlangWriter writeTypeFromSymbol(String name, Symbol symbol) {
        String typeSpec = symbol.getProperty("erlangType", String.class).orElse("term()");
        return writeType(name, typeSpec);
    }
    
    // ========== Custom Formatters ==========
    
    /**
     * Formatter for Erlang type names from Symbols.
     * 
     * <p>Usage in format strings: {@code $T}
     * 
     * <p>If the value is a {@link Symbol}, returns its "erlangType" property
     * or "term()" as default. Otherwise, returns the string representation.
     */
    private static final class ErlangTypeFormatter implements BiFunction<Object, String, String> {
        @Override
        public String apply(Object value, String indent) {
            if (value instanceof Symbol) {
                Symbol symbol = (Symbol) value;
                return symbol.getProperty("erlangType", String.class).orElse("term()");
            }
            return String.valueOf(value);
        }
    }
    
    /**
     * Formatter for Erlang names from Symbols.
     * 
     * <p>Usage in format strings: {@code $N}
     * 
     * <p>If the value is a {@link Symbol}, returns its name.
     * Otherwise, returns the string representation.
     */
    private static final class ErlangNameFormatter implements BiFunction<Object, String, String> {
        @Override
        public String apply(Object value, String indent) {
            if (value instanceof Symbol) {
                return ((Symbol) value).getName();
            }
            return String.valueOf(value);
        }
    }
    
    // ========== Factory Method ==========
    
    /**
     * Creates a factory for ErlangWriter instances.
     * 
     * <p>This is useful when integrating with Smithy's WriterDelegator.
     *
     * @return A factory that creates ErlangWriter instances
     */
    public static Factory factory() {
        return new Factory();
    }
    
    /**
     * Factory class for creating ErlangWriter instances.
     */
    public static final class Factory implements SymbolWriter.Factory<ErlangWriter> {
        @Override
        public ErlangWriter apply(String filename, String namespace) {
            // Extract module name from filename (remove .erl extension)
            String moduleName = filename;
            if (filename.endsWith(".erl")) {
                moduleName = filename.substring(0, filename.length() - 4);
            }
            return new ErlangWriter(moduleName);
        }
    }
}
