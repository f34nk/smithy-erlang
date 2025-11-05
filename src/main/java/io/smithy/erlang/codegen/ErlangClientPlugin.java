package io.smithy.erlang.codegen;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.build.PluginContext;
import software.amazon.smithy.build.SmithyBuildPlugin;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.ListShape;
import software.amazon.smithy.model.shapes.MapShape;
import software.amazon.smithy.model.shapes.MemberShape;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.Shape;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.shapes.StructureShape;
import software.amazon.smithy.model.shapes.UnionShape;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpLabelTrait;
import software.amazon.smithy.model.traits.HttpPayloadTrait;
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.HttpTrait;

/**
 * Simple Smithy Build plugin for generating Erlang client code.
 */
public final class ErlangClientPlugin implements SmithyBuildPlugin {
    
    private static final Logger LOGGER = Logger.getLogger(ErlangClientPlugin.class.getName());
    
    @Override
    public String getName() {
        return "erlang-client-codegen";
    }
    
    @Override
    public void execute(PluginContext context) {
        LOGGER.info("Executing Erlang client code generation");
        
        // Load settings
        ErlangClientSettings settings = ErlangClientSettings.from(
                context.getSettings(),
                context.getModel(),
                context.getFileManifest());
        
        Model model = context.getModel();
        FileManifest fileManifest = context.getFileManifest();
        
        LOGGER.info("Generating Erlang client for service: " + settings.getService());
        LOGGER.info("Output directory: " + settings.getOutputDir());
        LOGGER.info("Module name: " + settings.getModule());
        
        // Get the service shape
        ServiceShape service = model.expectShape(settings.getService(), ServiceShape.class);
        
        // Create symbol provider
        ErlangSymbolProvider symbolProvider = new ErlangSymbolProvider(model, settings);
        
        try {
            // Generate client module
            generateClientModule(service, model, symbolProvider, settings, fileManifest);
            
            // Generate types header file
            generateTypesHeader(service, model, symbolProvider, settings, fileManifest);
            
            // Generate types module (for union encoding/decoding functions)
            generateTypesModule(service, model, symbolProvider, settings, fileManifest);
            
            // Copy AWS SigV4 module
            copyAwsSigV4Module(settings, fileManifest);
            
            // Copy AWS credentials module
            copyAwsCredentialsModule(settings, fileManifest);
            
            // Copy AWS config module
            copyAwsConfigModule(settings, fileManifest);
            
            LOGGER.info("Erlang client code generation completed successfully");
        } catch (Exception e) {
            throw new RuntimeException("Failed to generate Erlang client code", e);
        }
    }
    
    private void generateClientModule(
            ServiceShape service,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangClientSettings settings,
            FileManifest fileManifest) throws IOException {
        
        String moduleName = settings.getModule();
        
        // Determine output path
        Path outputPath;
        boolean useCustomDir = settings.getOutputDir() != null && !settings.getOutputDir().isEmpty();
        
        if (useCustomDir) {
            // Navigate up from build/smithy/source/projection/ to project root, then resolve outputDir
            // Files go directly to outputDir (no src/ subdirectory)
            Path baseDir = fileManifest.getBaseDir();
            Path projectRoot = baseDir;
            
            // Try to navigate up 4 levels (typical Smithy build structure)
            // but handle case where we don't have that many parents (e.g., tests)
            for (int i = 0; i < 4 && projectRoot != null && projectRoot.getParent() != null; i++) {
                projectRoot = projectRoot.getParent();
            }
            
            // If projectRoot is null, fall back to baseDir
            if (projectRoot == null) {
                projectRoot = baseDir;
            }
            
            Path customOutputDir = projectRoot.resolve(settings.getOutputDir());
            outputPath = customOutputDir.resolve(moduleName + ".erl");
        } else {
            // Use default FileManifest location (includes src/ subdirectory)
            outputPath = fileManifest.getBaseDir().resolve("src/" + moduleName + ".erl");
        }
        
        ErlangWriter writer = new ErlangWriter();
        
        // Module declaration
        writer.writeModule(moduleName);
        writer.write("");
        writer.writeComment("Generated Smithy client for " + service.getId().getName());
        writer.write("");
        
        // Include types header file for type specs
        writer.write("-include(\"$L_types.hrl\").", moduleName);
        writer.write("");
        
        // Get all operations
        List<OperationShape> operations = service.getOperations().stream()
                .map(shapeId -> model.expectShape(shapeId, OperationShape.class))
                .collect(Collectors.toList());
        
        // Generate exports
        List<String> exports = new ArrayList<>();
        exports.add("new/1");
        for (OperationShape operation : operations) {
            String opName = ErlangSymbolProvider.toErlangName(operation.getId().getName());
            exports.add(opName + "/2");
        }
        writer.writeExport(exports.toArray(new String[0]));
        writer.write("");
        
        // ignore dialyzer warnings
        // For example: "user_client.erl:81:9: The pattern <<68,69,76,69,84,69>> can never match the type <<_:24>>"
        writer.writeComment("Ignore dialyzer warnings for \"The pattern ... can never match the type ...\".");
        writer.write("-dialyzer({[no_match], [$L]}).", String.join(", ", exports));
        writer.write("");
        
        // Generate new/1 function
        writer.writeComment("Creates a new client with the given configuration");
        writer.writeSpec("new", "(Config :: map()) -> {ok, map()}");
        writer.writeFunction("new", "Config", () -> {
            writer.write("{ok, Config}.");
        });
        
        // Generate URL encoding helper function
        generateUrlEncodeHelper(writer);
        
        // Generate operation functions
        for (OperationShape operation : operations) {
            generateOperation(operation, service, model, symbolProvider, writer);
        }
        
        // Write to file
        if (useCustomDir) {
            // Write directly to filesystem for custom output directory
            // Only create directories if parent exists and is writable
            try {
                if (outputPath.getParent() != null) {
                    Files.createDirectories(outputPath.getParent());
                }
                Files.writeString(outputPath, writer.toString());
                LOGGER.info("Generated client module: " + outputPath);
            } catch (java.nio.file.FileSystemException e) {
                // Fall back to FileManifest if we can't write to filesystem (e.g., in tests)
                LOGGER.warning("Cannot write to custom directory, using FileManifest instead: " + e.getMessage());
                // Use the default manifest path when falling back
                Path manifestPath = fileManifest.getBaseDir().resolve("src/" + moduleName + ".erl");
                fileManifest.writeFile(manifestPath, writer.toString());
            }
        } else {
            // Use FileManifest for default location
            fileManifest.writeFile(outputPath, writer.toString());
        }
    }
    
    private void generateUrlEncodeHelper(ErlangWriter writer) {
        writer.write("");
        writer.writeComment("URL encode a value for use in URI path parameters");
        writer.writeComment("Uses RFC 3986 compliant encoding via uri_string:quote/1");
        writer.write("");
        writer.writeComment("@spec url_encode(binary() | list()) -> binary().");
        writer.write("url_encode(Binary) when is_binary(Binary) ->");
        writer.indent();
        writer.write("url_encode(binary_to_list(Binary));");
        writer.dedent();
        writer.write("url_encode(String) when is_list(String) ->");
        writer.indent();
        writer.write("list_to_binary(uri_string:quote(String)).");
        writer.dedent();
        writer.write("");
        
        // Generate ensure_binary helper
        writer.writeComment("Convert a value to binary for use in URI substitution");
        writer.write("");
        writer.writeComment("@spec ensure_binary(term()) -> binary().");
        writer.write("ensure_binary(Value) when is_binary(Value) ->");
        writer.indent();
        writer.write("Value;");
        writer.dedent();
        writer.write("ensure_binary(Value) when is_list(Value) ->");
        writer.indent();
        writer.write("list_to_binary(Value);");
        writer.dedent();
        writer.write("ensure_binary(Value) when is_integer(Value) ->");
        writer.indent();
        writer.write("integer_to_binary(Value);");
        writer.dedent();
        writer.write("ensure_binary(Value) when is_float(Value) ->");
        writer.indent();
        writer.write("float_to_binary(Value);");
        writer.dedent();
        writer.write("ensure_binary(Value) when is_atom(Value) ->");
        writer.indent();
        writer.write("atom_to_binary(Value, utf8);");
        writer.dedent();
        writer.write("ensure_binary(Value) ->");
        writer.indent();
        writer.write("list_to_binary(io_lib:format(\"~p\", [Value])).");
        writer.dedent();
        writer.write("");
    }
    
    private void generateOperation(
            OperationShape operation,
            ServiceShape service,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangWriter writer) {
        
        String opName = ErlangSymbolProvider.toErlangName(operation.getId().getName());
        
        // Get HTTP trait if present
        final String method;
        final String uri;
        if (operation.hasTrait(HttpTrait.class)) {
            HttpTrait http = operation.expectTrait(HttpTrait.class);
            method = http.getMethod();
            uri = http.getUri().toString();
        } else {
            method = "POST";
            uri = "/";
        }
        
        // Find @httpLabel, @httpHeader, and @httpQuery members in input
        List<MemberShape> httpLabelMembers = new ArrayList<>();
        List<MemberShape> httpHeaderInputMembers = new ArrayList<>();
        List<MemberShape> httpQueryMembers = new ArrayList<>();
        if (operation.getInput().isPresent()) {
            StructureShape input = model.expectShape(operation.getInput().get(), StructureShape.class);
            for (MemberShape member : input.getAllMembers().values()) {
                if (member.hasTrait(HttpLabelTrait.class)) {
                    httpLabelMembers.add(member);
                }
                if (member.hasTrait(HttpHeaderTrait.class)) {
                    httpHeaderInputMembers.add(member);
                }
                if (member.hasTrait(HttpQueryTrait.class)) {
                    httpQueryMembers.add(member);
                }
            }
        }
        
        // Find @httpHeader members in output
        List<MemberShape> httpHeaderOutputMembers = new ArrayList<>();
        if (operation.getOutput().isPresent()) {
            StructureShape output = model.expectShape(operation.getOutput().get(), StructureShape.class);
            for (MemberShape member : output.getAllMembers().values()) {
                if (member.hasTrait(HttpHeaderTrait.class)) {
                    httpHeaderOutputMembers.add(member);
                }
            }
        }
        
        // Generate -spec with record types
        String inputType = "map()";
        String outputType = "map()";
        
        if (operation.getInput().isPresent()) {
            ShapeId inputId = operation.getInput().get();
            String inputRecordName = ErlangSymbolProvider.toErlangName(inputId.getName());
            inputType = "#" + inputRecordName + "{}";
        }
        
        if (operation.getOutput().isPresent()) {
            ShapeId outputId = operation.getOutput().get();
            String outputRecordName = ErlangSymbolProvider.toErlangName(outputId.getName());
            outputType = "#" + outputRecordName + "{}";
        }
        
        writer.writeComment("Calls the " + operation.getId().getName() + " operation");
        writer.write("-spec $L(Client :: map(), Input :: $L) -> {ok, $L} | {error, term()}.",
                opName, inputType, outputType);
        // Add guard to ensure runtime uses maps
        writer.write("$L(Client, Input) when is_map(Input) ->", opName);
        writer.indent();
        writer.write("Method = <<\"$L\">>,", method);
        writer.write("Endpoint = maps:get(endpoint, Client),");
        writer.write("");
        
        // Generate URI with path parameter substitution
        String uriVariable;
        if (httpLabelMembers.isEmpty()) {
            // No path parameters - use URI as-is
            writer.write("%% No path parameters");
            writer.write("Uri = <<\"$L\">>,", uri);
            uriVariable = "Uri";
        } else {
                // Path parameters need substitution
                writer.write("%% Build URL with path parameters");
                writer.write("Uri0 = <<\"$L\">>,", uri);
                
                // Generate substitution code for each @httpLabel member
                for (int i = 0; i < httpLabelMembers.size(); i++) {
                    MemberShape member = httpLabelMembers.get(i);
                    String memberName = member.getMemberName();
                    String erlangFieldName = ErlangSymbolProvider.toErlangName(memberName);
                    
                    // Get the label name (defaults to member name if not specified)
                    String labelName = memberName;
                    
                    // Extract value from input and URL encode it
                    writer.write("$LValue = maps:get(<<\"$L\">>, Input),", 
                            capitalize(erlangFieldName), memberName);
                    writer.write("$LEncoded = url_encode(ensure_binary($LValue)),",
                            capitalize(erlangFieldName), capitalize(erlangFieldName));
                    
                    // Substitute into URI
                    writer.write("Uri$L = binary:replace(Uri$L, <<\"{$L}\">>, $LEncoded),",
                            i + 1, i, labelName, capitalize(erlangFieldName));
                }
                
                uriVariable = "Uri" + httpLabelMembers.size();
            }
            
            // Generate query string from @httpQuery parameters
            if (httpQueryMembers.isEmpty()) {
                // No query parameters
                writer.write("Url = <<Endpoint/binary, $L/binary>>,", uriVariable);
            } else {
                // Build query string incrementally
                writer.write("");
                writer.write("%% Build query string from @httpQuery parameters");
                writer.write("QueryPairs0 = [],");
                
                for (int i = 0; i < httpQueryMembers.size(); i++) {
                    MemberShape member = httpQueryMembers.get(i);
                    String memberName = member.getMemberName();
                    String erlangFieldName = ErlangSymbolProvider.toErlangName(memberName);
                    
                    // Get query parameter name from trait
                    HttpQueryTrait queryTrait = member.expectTrait(HttpQueryTrait.class);
                    String queryName = queryTrait.getValue();
                    
                    // Determine target shape type for conversion
                    Shape targetShape = model.expectShape(member.getTarget());
                    String conversionFunc = "ensure_binary";
                    
                    // Use integer_to_binary for integer types
                    if (targetShape.isIntegerShape() || targetShape.isLongShape() || 
                        targetShape.isShortShape() || targetShape.isByteShape()) {
                        conversionFunc = "integer_to_binary";
                    }
                    
                    // All query parameters are optional (use default of undefined)
                    writer.write("QueryPairs$L = case maps:get(<<\"$L\">>, Input, undefined) of",
                            i + 1, memberName);
                    writer.indent();
                    writer.write("undefined -> QueryPairs$L;", i);
                    writer.write("$LVal -> [{<<\"$L\">>, $L($LVal)} | QueryPairs$L]",
                            capitalize(erlangFieldName), queryName, conversionFunc, capitalize(erlangFieldName), i);
                    writer.dedent();
                    writer.write("end,");
                }
                
                // Compose query string
                writer.write("");
                writer.write("QueryString = case QueryPairs$L of", httpQueryMembers.size());
                writer.indent();
                writer.write("[] -> <<\"\">>;");
                writer.write("Pairs ->");
                writer.indent();
                writer.write("Encoded = uri_string:compose_query(Pairs),");
                writer.write("<<\"?\", Encoded/binary>>");
                writer.dedent();
                writer.dedent();
                writer.write("end,");
                writer.write("");
                writer.write("Url = <<Endpoint/binary, $L/binary, QueryString/binary>>,", uriVariable);
            }
            
            // Generate request body based on @httpPayload
            writer.write("");
            MemberShape payloadMember = null;
            if (operation.getInput().isPresent()) {
                StructureShape input = model.expectShape(operation.getInput().get(), StructureShape.class);
                for (MemberShape member : input.getAllMembers().values()) {
                    if (member.hasTrait(HttpPayloadTrait.class)) {
                        payloadMember = member;
                        break;
                    }
                }
            }
            
            String contentType;
            if (payloadMember == null) {
                // No @httpPayload - entire structure is JSON
                writer.write("%% No @httpPayload - encode entire structure as JSON");
                writer.write("Body = jsx:encode(Input),");
                contentType = "application/json";
            } else {
                // @httpPayload present - extract member and handle by type
                String memberName = payloadMember.getMemberName();
                Shape targetShape = model.expectShape(payloadMember.getTarget());
                
                writer.write("%% @httpPayload - extract payload member");
                if (targetShape.isBlobShape()) {
                    // Blob payload - use directly as binary
                    writer.write("Body = maps:get(<<\"$L\">>, Input),", memberName);
                    contentType = "application/octet-stream";
                } else if (targetShape.isStringShape()) {
                    // String payload - convert to binary
                    writer.write("PayloadString = maps:get(<<\"$L\">>, Input),", memberName);
                    writer.write("Body = ensure_binary(PayloadString),");
                    contentType = "text/plain";
                } else {
                    // Structure payload - encode as JSON
                    writer.write("PayloadData = maps:get(<<\"$L\">>, Input),", memberName);
                    writer.write("Body = jsx:encode(PayloadData),");
                    contentType = "application/json";
                }
            }
            writer.write("");
            
            // Generate headers with @httpHeader members
            if (httpHeaderInputMembers.isEmpty()) {
                // No custom headers
                writer.write("%% Headers");
                writer.write("Headers = [{<<\"Content-Type\">>, <<\"$L\">>}],", contentType);
            } else {
                // Build headers with @httpHeader members
                writer.write("%% Build headers with @httpHeader members");
                writer.write("Headers0 = [{<<\"Content-Type\">>, <<\"$L\">>}],", contentType);
                
                for (int i = 0; i < httpHeaderInputMembers.size(); i++) {
                    MemberShape member = httpHeaderInputMembers.get(i);
                    String memberName = member.getMemberName();
                    String erlangFieldName = ErlangSymbolProvider.toErlangName(memberName);
                    
                    // Get header name from trait
                    // For @httpHeader("HeaderName"), the header name is stored in the trait
                    HttpHeaderTrait headerTrait = member.expectTrait(HttpHeaderTrait.class);
                    String headerName = headerTrait.getValue();
                    
                    // Check if member is required
                    boolean isRequired = member.hasTrait(software.amazon.smithy.model.traits.RequiredTrait.class);
                    
                    if (isRequired) {
                        // Required header - always add it
                        writer.write("$LValue = ensure_binary(maps:get(<<\"$L\">>, Input)),",
                                capitalize(erlangFieldName), memberName);
                        writer.write("Headers$L = [{<<\"$L\">>, $LValue} | Headers$L],",
                                i + 1, headerName, capitalize(erlangFieldName), i);
                    } else {
                        // Optional header - add conditionally
                        writer.write("Headers$L = case maps:get(<<\"$L\">>, Input, undefined) of",
                                i + 1, memberName);
                        writer.indent();
                        writer.write("undefined -> Headers$L;", i);
                        writer.write("$LValue -> [{<<\"$L\">>, ensure_binary($LValue)} | Headers$L]",
                                capitalize(erlangFieldName), headerName, capitalize(erlangFieldName), i);
                        writer.dedent();
                        writer.write("end,");
                    }
                }
                
                writer.write("Headers = Headers$L,", httpHeaderInputMembers.size());
            }
            
            writer.write("");
            writer.write("%% Sign request with AWS SigV4 if credentials provided");
            writer.write("SignedHeaders = case maps:get(credentials, Client, undefined) of");
            writer.indent();
            writer.write("undefined ->");
            writer.indent();
            writer.write("%% No credentials - use headers as-is");
            writer.write("Headers;");
            writer.dedent();
            writer.write("Credentials ->");
            writer.indent();
            writer.write("%% AWS credentials present - sign the request");
            writer.write("Region = maps:get(region, Client, <<\"us-east-1\">>),");
            // Extract service name from service shape
            String serviceName = service.getId().getName().toLowerCase();
            writer.write("Service = <<\"$L\">>,", serviceName);
            writer.write("CredentialsWithRegion = Credentials#{region => Region, service => Service},");
            writer.write("aws_sigv4:sign_request(Method, Url, Headers, Body, CredentialsWithRegion)");
            writer.dedent();
            writer.dedent();
            writer.write("end,");
            writer.write("");
            writer.write("%% Make HTTP request");
            writer.write("Request = case Method of");
            writer.indent();
            writer.write("<<\"GET\">> -> {binary_to_list(Url), [{binary_to_list(K), binary_to_list(V)} || {K, V} <- SignedHeaders]};");
            writer.write("<<\"DELETE\">> -> {binary_to_list(Url), [{binary_to_list(K), binary_to_list(V)} || {K, V} <- SignedHeaders]};");
            writer.write("_ -> {binary_to_list(Url), [{binary_to_list(K), binary_to_list(V)} || {K, V} <- SignedHeaders], \"application/json\", binary_to_list(Body)}");
            writer.dedent();
            writer.write("end,");
            writer.write("");
            writer.write("case httpc:request(binary_to_atom(string:lowercase(Method), utf8), Request, [], [{body_format, binary}]) of");
            writer.indent();
            
            // Check for output payload
            MemberShape outputPayloadMember = null;
            if (operation.getOutput().isPresent()) {
                StructureShape output = model.expectShape(operation.getOutput().get(), StructureShape.class);
                for (MemberShape member : output.getAllMembers().values()) {
                    if (member.hasTrait(HttpPayloadTrait.class)) {
                        outputPayloadMember = member;
                        break;
                    }
                }
            }
            
            if (httpHeaderOutputMembers.isEmpty()) {
                // No output headers to extract
                writer.write("{ok, {{_, 200, _}, _, ResponseBody}} ->");
                writer.indent();
                
                // Handle response body based on @httpPayload
                if (outputPayloadMember == null) {
                    // No @httpPayload - parse entire response as JSON
                    writer.write("{ok, jsx:decode(ResponseBody, [return_maps])};");
                } else {
                    // @httpPayload present - handle by type
                    String memberName = outputPayloadMember.getMemberName();
                    Shape targetShape = model.expectShape(outputPayloadMember.getTarget());
                    
                    if (targetShape.isBlobShape()) {
                        // Blob payload - return body directly in map
                        writer.write("{ok, #{<<\"$L\">> => ResponseBody}};", memberName);
                    } else if (targetShape.isStringShape()) {
                        // String payload - convert to string/binary
                        writer.write("{ok, #{<<\"$L\">> => ResponseBody}};", memberName);
                    } else {
                        // Structure payload - parse JSON
                        writer.write("PayloadData = jsx:decode(ResponseBody, [return_maps]),");
                        writer.write("{ok, #{<<\"$L\">> => PayloadData}};", memberName);
                    }
                }
                writer.dedent();
            } else {
                // Extract headers from response
                writer.write("{ok, {{_, 200, _}, ResponseHeaders, ResponseBody}} ->");
                writer.indent();
                
                // Handle response body based on @httpPayload
                if (outputPayloadMember == null) {
                    // No @httpPayload - parse entire response as JSON
                    writer.write("%% Parse body");
                    writer.write("Output0 = jsx:decode(ResponseBody, [return_maps]),");
                } else {
                    // @httpPayload present - handle by type
                    String memberName = outputPayloadMember.getMemberName();
                    Shape targetShape = model.expectShape(outputPayloadMember.getTarget());
                    
                    if (targetShape.isBlobShape()) {
                        // Blob payload - use body directly
                        writer.write("%% Blob payload");
                        writer.write("Output0 = #{<<\"$L\">> => ResponseBody},", memberName);
                    } else if (targetShape.isStringShape()) {
                        // String payload
                        writer.write("%% String payload");
                        writer.write("Output0 = #{<<\"$L\">> => ResponseBody},", memberName);
                    } else {
                        // Structure payload - parse JSON
                        writer.write("%% Structure payload");
                        writer.write("PayloadData = jsx:decode(ResponseBody, [return_maps]),");
                        writer.write("Output0 = #{<<\"$L\">> => PayloadData},", memberName);
                    }
                }
                
                writer.write("");
                writer.write("%% Extract @httpHeader members from response");
                
                for (int i = 0; i < httpHeaderOutputMembers.size(); i++) {
                    MemberShape member = httpHeaderOutputMembers.get(i);
                    String memberName = member.getMemberName();
                    String erlangFieldName = ErlangSymbolProvider.toErlangName(memberName);
                    
                    // Get header name from trait (lowercase for case-insensitive lookup)
                    // For @httpHeader("HeaderName"), the header name is stored in the trait
                    HttpHeaderTrait headerTrait = member.expectTrait(HttpHeaderTrait.class);
                    String headerName = headerTrait.getValue();
                    String headerNameLower = headerName.toLowerCase();
                    
                    // Extract header value
                    String varName = capitalize(erlangFieldName) + "Value";
                    writer.write("Output$L = case lists:keyfind(\"$L\", 1, ResponseHeaders) of",
                            i + 1, headerNameLower);
                    writer.indent();
                    writer.write("{_, $L} -> maps:put(<<\"$L\">>, list_to_binary($L), Output$L);",
                            varName, memberName, varName, i);
                    writer.write("false -> Output$L", i);
                    writer.dedent();
                    writer.write("end,");
                }
                
                writer.write("{ok, Output$L};", httpHeaderOutputMembers.size());
                writer.dedent();
            }
            
            writer.write("{ok, {{_, StatusCode, _}, _, ResponseBody}} ->");
            writer.indent();
            writer.write("{error, {StatusCode, jsx:decode(ResponseBody, [return_maps])}};");
            writer.dedent();
            writer.write("{error, Reason} ->");
            writer.indent();
        writer.write("{error, Reason}");
        writer.dedent();
        writer.dedent();
        writer.write("end.");
        writer.dedent();
    }
    
    /**
     * Capitalize the first letter of a string.
     */
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
    
    private void generateTypesHeader(
            ServiceShape service,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangClientSettings settings,
            FileManifest fileManifest) throws IOException {
        
        String moduleName = settings.getModule() + "_types";
        
        // Determine output path for .hrl file
        Path outputPath;
        boolean useCustomDir = settings.getOutputDir() != null && !settings.getOutputDir().isEmpty();
        
        if (useCustomDir) {
            // Navigate up from build/smithy/source/projection/ to project root, then resolve outputDir
            // Files go directly to outputDir (no src/ subdirectory)
            Path baseDir = fileManifest.getBaseDir();
            Path projectRoot = baseDir;
            
            // Try to navigate up 4 levels (typical Smithy build structure)
            // but handle case where we don't have that many parents (e.g., tests)
            for (int i = 0; i < 4 && projectRoot != null && projectRoot.getParent() != null; i++) {
                projectRoot = projectRoot.getParent();
            }
            
            // If projectRoot is null, fall back to baseDir
            if (projectRoot == null) {
                projectRoot = baseDir;
            }
            
            Path customOutputDir = projectRoot.resolve(settings.getOutputDir());
            outputPath = customOutputDir.resolve(moduleName + ".hrl");
        } else {
            // Use default FileManifest location (includes src/ subdirectory)
            outputPath = fileManifest.getBaseDir().resolve("src/" + moduleName + ".hrl");
        }
        
        ErlangWriter writer = new ErlangWriter();
        
        // Add header guards
        String headerGuard = moduleName.toUpperCase() + "_HRL";
        writer.write("-ifndef($L).", headerGuard);
        writer.write("-define($L, true).", headerGuard);
        writer.write("");
        writer.writeComment("Generated type definitions for Smithy model");
        writer.write("");
        
        // Track all structures and unions, and collect them
        java.util.Set<ShapeId> allStructures = new java.util.HashSet<>();
        java.util.Set<ShapeId> allUnions = new java.util.HashSet<>();
        List<StructureShape> structuresToProcess = new ArrayList<>();
        List<UnionShape> unionsToProcess = new ArrayList<>();
        
        // Collect all structures and unions from operations
        for (ShapeId operationId : service.getOperations()) {
            OperationShape operation = model.expectShape(operationId, OperationShape.class);
            
            // Collect input structure
            operation.getInput().ifPresent(inputId -> {
                StructureShape inputShape = model.expectShape(inputId, StructureShape.class);
                if (allStructures.add(inputId)) {
                    structuresToProcess.add(inputShape);
                }
            });
            
            // Collect output structure
            operation.getOutput().ifPresent(outputId -> {
                StructureShape outputShape = model.expectShape(outputId, StructureShape.class);
                if (allStructures.add(outputId)) {
                    structuresToProcess.add(outputShape);
                }
            });
            
            // Collect error structures
            for (ShapeId errorId : operation.getErrors()) {
                StructureShape errorShape = model.expectShape(errorId, StructureShape.class);
                if (allStructures.add(errorId)) {
                    structuresToProcess.add(errorShape);
                }
            }
        }
        
        // Collect all nested structures and unions
        int previousSize = -1;
        int previousUnionSize = -1;
        while (structuresToProcess.size() != previousSize || unionsToProcess.size() != previousUnionSize) {
            previousSize = structuresToProcess.size();
            previousUnionSize = unionsToProcess.size();
            List<StructureShape> currentStructures = new ArrayList<>(structuresToProcess);
            List<UnionShape> currentUnions = new ArrayList<>(unionsToProcess);
            
            // Collect from structures
            for (StructureShape structure : currentStructures) {
                for (MemberShape member : structure.getAllMembers().values()) {
                    Shape targetShape = model.expectShape(member.getTarget());
                    collectNestedShapes(targetShape, model, allStructures, structuresToProcess, allUnions, unionsToProcess);
                }
            }
            
            // Collect from unions
            for (UnionShape union : currentUnions) {
                for (MemberShape member : union.getAllMembers().values()) {
                    Shape targetShape = model.expectShape(member.getTarget());
                    collectNestedShapes(targetShape, model, allStructures, structuresToProcess, allUnions, unionsToProcess);
                }
            }
        }
        
        // Sort structures by dependencies (topological sort)
        List<StructureShape> sortedStructures = topologicalSort(structuresToProcess, model);
        
        // Generate structures in dependency order
        for (StructureShape structure : sortedStructures) {
            generateStructure(structure, model, symbolProvider, writer);
        }
        
        // Generate union types (type definitions only in header)
        LOGGER.info("Generating " + unionsToProcess.size() + " union types");
        for (UnionShape union : unionsToProcess) {
            generateUnion(union, model, symbolProvider, writer);
        }
        
        // Close header guard
        writer.write("");
        writer.write("-endif.");
        
        // Write to file
        if (useCustomDir) {
            // Write directly to filesystem for custom output directory
            // Only create directories if parent exists and is writable
            try {
                if (outputPath.getParent() != null) {
                    Files.createDirectories(outputPath.getParent());
                }
                Files.writeString(outputPath, writer.toString());
                LOGGER.info("Generated types header: " + outputPath);
            } catch (java.nio.file.FileSystemException e) {
                // Fall back to FileManifest if we can't write to filesystem (e.g., in tests)
                LOGGER.warning("Cannot write to custom directory, using FileManifest instead: " + e.getMessage());
                // Use the default manifest path when falling back
                Path manifestPath = fileManifest.getBaseDir().resolve("src/" + moduleName + ".hrl");
                fileManifest.writeFile(manifestPath, writer.toString());
            }
        } else {
            // Use FileManifest for default location
            fileManifest.writeFile(outputPath, writer.toString());
        }
    }
    
    /**
     * Topologically sort structures so dependencies are generated before the structures that use them.
     */
    private List<StructureShape> topologicalSort(List<StructureShape> structures, Model model) {
        // Build dependency map: structure -> structures it depends on
        java.util.Map<ShapeId, java.util.Set<ShapeId>> dependencies = new java.util.HashMap<>();
        java.util.Map<ShapeId, StructureShape> shapeMap = new java.util.HashMap<>();
        
        for (StructureShape structure : structures) {
            shapeMap.put(structure.getId(), structure);
            java.util.Set<ShapeId> deps = new java.util.HashSet<>();
            
            // Find all structure dependencies in members
            for (MemberShape member : structure.getAllMembers().values()) {
                collectStructureDependencies(model.expectShape(member.getTarget()), model, deps);
            }
            
            dependencies.put(structure.getId(), deps);
        }
        
        // Perform topological sort using Kahn's algorithm
        List<StructureShape> sorted = new ArrayList<>();
        java.util.Set<ShapeId> visited = new java.util.HashSet<>();
        java.util.Set<ShapeId> processing = new java.util.HashSet<>();
        
        for (StructureShape structure : structures) {
            if (!visited.contains(structure.getId())) {
                topologicalSortVisit(structure.getId(), dependencies, shapeMap, visited, processing, sorted);
            }
        }
        
        return sorted;
    }
    
    private void topologicalSortVisit(
            ShapeId shapeId,
            java.util.Map<ShapeId, java.util.Set<ShapeId>> dependencies,
            java.util.Map<ShapeId, StructureShape> shapeMap,
            java.util.Set<ShapeId> visited,
            java.util.Set<ShapeId> processing,
            List<StructureShape> sorted) {
        
        if (visited.contains(shapeId)) {
            return;
        }
        
        if (processing.contains(shapeId)) {
            // Circular dependency detected - skip it
            return;
        }
        
        processing.add(shapeId);
        
        // Visit dependencies first
        java.util.Set<ShapeId> deps = dependencies.getOrDefault(shapeId, java.util.Collections.emptySet());
        for (ShapeId dep : deps) {
            if (shapeMap.containsKey(dep)) {  // Only visit if it's a structure we're generating
                topologicalSortVisit(dep, dependencies, shapeMap, visited, processing, sorted);
            }
        }
        
        processing.remove(shapeId);
        visited.add(shapeId);
        
        // Add to sorted list (dependencies are already added)
        if (shapeMap.containsKey(shapeId)) {
            sorted.add(shapeMap.get(shapeId));
        }
    }
    
    /**
     * Collect all structure dependencies from a shape (recursively through lists/maps).
     */
    private void collectStructureDependencies(Shape shape, Model model, java.util.Set<ShapeId> deps) {
        if (shape instanceof StructureShape) {
            deps.add(shape.getId());
        } else if (shape instanceof ListShape) {
            Shape memberShape = model.expectShape(((ListShape) shape).getMember().getTarget());
            collectStructureDependencies(memberShape, model, deps);
        } else if (shape instanceof MapShape) {
            MapShape mapShape = (MapShape) shape;
            Shape valueShape = model.expectShape(mapShape.getValue().getTarget());
            collectStructureDependencies(valueShape, model, deps);
        }
    }
    
    private void collectNestedShapes(
            Shape shape,
            Model model,
            java.util.Set<ShapeId> generatedStructures,
            List<StructureShape> structuresToGenerate,
            java.util.Set<ShapeId> generatedUnions,
            List<UnionShape> unionsToGenerate) {
        
        if (shape instanceof StructureShape) {
            if (generatedStructures.add(shape.getId())) {
                structuresToGenerate.add((StructureShape) shape);
            }
        } else if (shape instanceof UnionShape) {
            if (generatedUnions.add(shape.getId())) {
                unionsToGenerate.add((UnionShape) shape);
            }
        } else if (shape instanceof ListShape) {
            Shape memberShape = model.expectShape(((ListShape) shape).getMember().getTarget());
            collectNestedShapes(memberShape, model, generatedStructures, structuresToGenerate, generatedUnions, unionsToGenerate);
        } else if (shape instanceof MapShape) {
            MapShape mapShape = (MapShape) shape;
            Shape keyShape = model.expectShape(mapShape.getKey().getTarget());
            Shape valueShape = model.expectShape(mapShape.getValue().getTarget());
            collectNestedShapes(keyShape, model, generatedStructures, structuresToGenerate, generatedUnions, unionsToGenerate);
            collectNestedShapes(valueShape, model, generatedStructures, structuresToGenerate, generatedUnions, unionsToGenerate);
        }
    }
    
    private void generateStructure(
            StructureShape structure,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangWriter writer) {
        
        String recordName = ErlangSymbolProvider.toErlangName(structure.getId().getName());
        
        writer.writeComment("Record for " + structure.getId().getName());
        writer.writeRecord(recordName, () -> {
            List<MemberShape> members = new ArrayList<>(structure.getAllMembers().values());
            
            for (int i = 0; i < members.size(); i++) {
                MemberShape member = members.get(i);
                String memberName = ErlangSymbolProvider.toErlangName(member.getMemberName());
                Shape targetShape = model.expectShape(member.getTarget());
                String typeAnnotation = " :: " + symbolProvider.getErlangType(targetShape);
                
                writer.writeInline(memberName + typeAnnotation);
                if (i < members.size() - 1) {
                    writer.write(",");
                } else {
                    writer.write("");
                }
            }
        });
    }
    
    private void generateTypesModule(
            ServiceShape service,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangClientSettings settings,
            FileManifest fileManifest) throws IOException {
        
        String moduleName = settings.getModule() + "_types";
        
        // Determine output path for .erl file
        Path outputPath;
        boolean useCustomDir = settings.getOutputDir() != null && !settings.getOutputDir().isEmpty();
        
        if (useCustomDir) {
            Path baseDir = fileManifest.getBaseDir();
            Path projectRoot = baseDir;
            
            for (int i = 0; i < 4 && projectRoot != null && projectRoot.getParent() != null; i++) {
                projectRoot = projectRoot.getParent();
            }
            
            if (projectRoot == null) {
                projectRoot = baseDir;
            }
            
            Path customOutputDir = projectRoot.resolve(settings.getOutputDir());
            outputPath = customOutputDir.resolve(moduleName + ".erl");
        } else {
            outputPath = fileManifest.getBaseDir().resolve("src/" + moduleName + ".erl");
        }
        
        // Collect all unions
        java.util.Set<ShapeId> allUnions = new java.util.HashSet<>();
        List<UnionShape> unionsToProcess = new ArrayList<>();
        
        // Collect unions from operations
        for (ShapeId operationId : service.getOperations()) {
            OperationShape operation = model.expectShape(operationId, OperationShape.class);
            
            operation.getInput().ifPresent(inputId -> {
                StructureShape inputShape = model.expectShape(inputId, StructureShape.class);
                collectUnionsFromStructure(inputShape, model, allUnions, unionsToProcess);
            });
            
            operation.getOutput().ifPresent(outputId -> {
                StructureShape outputShape = model.expectShape(outputId, StructureShape.class);
                collectUnionsFromStructure(outputShape, model, allUnions, unionsToProcess);
            });
        }
        
        // If no unions, skip module generation
        if (unionsToProcess.isEmpty()) {
            LOGGER.info("No unions found, skipping types module generation");
            return;
        }
        
        LOGGER.info("Generating types module with " + unionsToProcess.size() + " union encoding functions");
        
        ErlangWriter writer = new ErlangWriter();
        
        // Module declaration
        writer.writeModule(moduleName);
        writer.write("");
        writer.writeComment("Generated union type encoding/decoding functions");
        writer.write("");
        
        // Include types header
        writer.write("-include(\"$L.hrl\").", moduleName);
        writer.write("");
        
        // Export encoding and decoding functions
        writer.write("-export([");
        List<UnionShape> unions = new ArrayList<>(unionsToProcess);
        for (int i = 0; i < unions.size(); i++) {
            UnionShape union = unions.get(i);
            String unionName = ErlangSymbolProvider.toErlangName(union.getId().getName());
            if (i > 0) {
                writer.write("         ");
            }
            writer.writeInline("encode_" + unionName + "/1");
            writer.write(",");
        }
        writer.write("");
        for (int i = 0; i < unions.size(); i++) {
            UnionShape union = unions.get(i);
            String unionName = ErlangSymbolProvider.toErlangName(union.getId().getName());
            writer.write("         ");
            writer.writeInline("decode_" + unionName + "/1");
            if (i < unions.size() - 1) {
                writer.write(",");
            }
        }
        writer.write("]).");
        writer.write("");
        
        // Generate encoding functions for each union
        for (UnionShape union : unionsToProcess) {
            generateUnionEncodingFunction(union, model, symbolProvider, writer);
        }
        
        // Generate decoding functions for each union
        for (UnionShape union : unionsToProcess) {
            generateUnionDecodingFunction(union, model, symbolProvider, writer);
        }
        
        // Write to file
        if (useCustomDir) {
            try {
                if (outputPath.getParent() != null) {
                    Files.createDirectories(outputPath.getParent());
                }
                Files.writeString(outputPath, writer.toString());
                LOGGER.info("Generated types module: " + outputPath);
            } catch (java.nio.file.FileSystemException e) {
                LOGGER.warning("Cannot write to custom directory, using FileManifest instead: " + e.getMessage());
                Path manifestPath = fileManifest.getBaseDir().resolve("src/" + moduleName + ".erl");
                fileManifest.writeFile(manifestPath, writer.toString());
            }
        } else {
            fileManifest.writeFile(outputPath, writer.toString());
        }
    }
    
    private void collectUnionsFromStructure(
            StructureShape structure,
            Model model,
            java.util.Set<ShapeId> allUnions,
            List<UnionShape> unionsToProcess) {
        
        for (MemberShape member : structure.getAllMembers().values()) {
            Shape targetShape = model.expectShape(member.getTarget());
            collectUnionsRecursive(targetShape, model, allUnions, unionsToProcess);
        }
    }
    
    private void collectUnionsRecursive(
            Shape shape,
            Model model,
            java.util.Set<ShapeId> allUnions,
            List<UnionShape> unionsToProcess) {
        
        if (shape instanceof UnionShape) {
            if (allUnions.add(shape.getId())) {
                unionsToProcess.add((UnionShape) shape);
                // Also collect unions from union members
                for (MemberShape member : ((UnionShape) shape).getAllMembers().values()) {
                    Shape memberShape = model.expectShape(member.getTarget());
                    collectUnionsRecursive(memberShape, model, allUnions, unionsToProcess);
                }
            }
        } else if (shape instanceof StructureShape) {
            for (MemberShape member : ((StructureShape) shape).getAllMembers().values()) {
                Shape memberShape = model.expectShape(member.getTarget());
                collectUnionsRecursive(memberShape, model, allUnions, unionsToProcess);
            }
        } else if (shape instanceof ListShape) {
            Shape memberShape = model.expectShape(((ListShape) shape).getMember().getTarget());
            collectUnionsRecursive(memberShape, model, allUnions, unionsToProcess);
        } else if (shape instanceof MapShape) {
            MapShape mapShape = (MapShape) shape;
            Shape valueShape = model.expectShape(mapShape.getValue().getTarget());
            collectUnionsRecursive(valueShape, model, allUnions, unionsToProcess);
        }
    }
    
    private void generateUnionEncodingFunction(
            UnionShape union,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangWriter writer) {
        
        String unionName = ErlangSymbolProvider.toErlangName(union.getId().getName());
        String functionName = "encode_" + unionName;
        
        writer.writeComment("Encode " + union.getId().getName() + " union to JSON");
        writer.writeSpec(functionName, "(" + unionName + "() | {unknown, term()}) -> map()");
        
        // Generate function clauses for each variant
        List<MemberShape> members = new ArrayList<>(union.getAllMembers().values());
        for (int i = 0; i < members.size(); i++) {
            MemberShape member = members.get(i);
            String variantName = ErlangSymbolProvider.toErlangName(member.getMemberName());
            String originalMemberName = member.getMemberName();
            Shape targetShape = model.expectShape(member.getTarget());
            
            // Function clause: encode_storage_type({s3, S3Data}) ->
            writer.write("$L({$L, Data}) ->", functionName, variantName);
            writer.indent();
            
            // Generate encoding based on target shape type
            if (targetShape.isStructureShape()) {
                // Structure: encode as map (already in map format from API)
                writer.write("#{<<\"$L\">> => Data};", originalMemberName);
            } else if (targetShape.isStringShape() || targetShape.isIntegerShape() || 
                       targetShape.isLongShape() || targetShape.isBooleanShape() ||
                       targetShape.isFloatShape() || targetShape.isDoubleShape() ||
                       targetShape.isBlobShape() || targetShape.isTimestampShape()) {
                // Primitive: use directly
                writer.write("#{<<\"$L\">> => Data};", originalMemberName);
            } else if (targetShape.isListShape() || targetShape.isMapShape()) {
                // List or map: use directly
                writer.write("#{<<\"$L\">> => Data};", originalMemberName);
            } else {
                // Default: use data as-is
                writer.write("#{<<\"$L\">> => Data};", originalMemberName);
            }
            
            writer.dedent();
        }
        
        // Add unknown variant handler for forward compatibility
        writer.write("$L({unknown, Data}) ->", functionName);
        writer.indent();
        writer.write("#{<<\"unknown\">> => Data}.");
        writer.dedent();
        
        writer.write("");
    }
    
    private void generateUnionDecodingFunction(
            UnionShape union,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangWriter writer) {
        
        String unionName = ErlangSymbolProvider.toErlangName(union.getId().getName());
        String functionName = "decode_" + unionName;
        
        writer.writeComment("Decode JSON to " + union.getId().getName() + " union");
        writer.writeSpec(functionName, "(map()) -> " + unionName + "() | {unknown, term()}");
        
        // Generate function clauses for each variant
        List<MemberShape> members = new ArrayList<>(union.getAllMembers().values());
        for (int i = 0; i < members.size(); i++) {
            MemberShape member = members.get(i);
            String variantName = ErlangSymbolProvider.toErlangName(member.getMemberName());
            String originalMemberName = member.getMemberName();
            Shape targetShape = model.expectShape(member.getTarget());
            
            // Function clause: decode_storage_type(#{<<"s3">> := Data}) ->
            writer.write("$L(#{<<\"$L\">> := Data}) ->", functionName, originalMemberName);
            writer.indent();
            
            // Generate decoding based on target shape type
            if (targetShape.isStructureShape()) {
                // Structure: return tagged tuple with data (data is already a map)
                writer.write("{$L, Data};", variantName);
            } else if (targetShape.isStringShape() || targetShape.isIntegerShape() || 
                       targetShape.isLongShape() || targetShape.isBooleanShape() ||
                       targetShape.isFloatShape() || targetShape.isDoubleShape() ||
                       targetShape.isBlobShape() || targetShape.isTimestampShape()) {
                // Primitive: return tagged tuple with data directly
                writer.write("{$L, Data};", variantName);
            } else if (targetShape.isListShape() || targetShape.isMapShape()) {
                // List or map: return tagged tuple with data directly
                writer.write("{$L, Data};", variantName);
            } else {
                // Default: return tagged tuple with data as-is
                writer.write("{$L, Data};", variantName);
            }
            
            writer.dedent();
        }
        
        // Add unknown variant handler for forward compatibility
        writer.writeComment("Handle unknown variants for forward compatibility");
        writer.write("$L(UnknownVariant) ->", functionName);
        writer.indent();
        writer.write("{unknown, UnknownVariant}.");
        writer.dedent();
        
        writer.write("");
    }
    
    private void copyAwsSigV4Module(
            ErlangClientSettings settings,
            FileManifest fileManifest) throws IOException {
        
        LOGGER.info("Copying AWS SigV4 module to generated output");
        
        // Read aws_sigv4.erl from resources
        java.io.InputStream sigv4Stream = getClass().getClassLoader().getResourceAsStream("aws_sigv4.erl");
        if (sigv4Stream == null) {
            LOGGER.warning("aws_sigv4.erl not found in resources, skipping");
            return;
        }
        
        String sigv4Content = new String(sigv4Stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
        sigv4Stream.close();
        
        // Determine output path
        Path outputPath;
        boolean useCustomDir = settings.getOutputDir() != null && !settings.getOutputDir().isEmpty();
        
        if (useCustomDir) {
            // Navigate up from build/smithy/source/projection/ to project root, then resolve outputDir
            Path baseDir = fileManifest.getBaseDir();
            Path projectRoot = baseDir;
            
            // Try to navigate up 4 levels (typical Smithy build structure)
            for (int i = 0; i < 4 && projectRoot != null && projectRoot.getParent() != null; i++) {
                projectRoot = projectRoot.getParent();
            }
            
            // If projectRoot is null, fall back to baseDir
            if (projectRoot == null) {
                projectRoot = baseDir;
            }
            
            Path customOutputDir = projectRoot.resolve(settings.getOutputDir());
            outputPath = customOutputDir.resolve("aws_sigv4.erl");
        } else {
            // Use default FileManifest location
            outputPath = fileManifest.getBaseDir().resolve("src/aws_sigv4.erl");
        }
        
        // Write the file
        if (useCustomDir) {
            try {
                if (outputPath.getParent() != null) {
                    Files.createDirectories(outputPath.getParent());
                }
                Files.writeString(outputPath, sigv4Content);
                LOGGER.info("Copied AWS SigV4 module: " + outputPath);
            } catch (java.nio.file.FileSystemException e) {
                // Fall back to FileManifest if we can't write to filesystem
                LOGGER.warning("Cannot write to custom directory, using FileManifest instead: " + e.getMessage());
                Path manifestPath = fileManifest.getBaseDir().resolve("src/aws_sigv4.erl");
                fileManifest.writeFile(manifestPath, sigv4Content);
            }
        } else {
            fileManifest.writeFile(outputPath, sigv4Content);
        }
    }
    
    private void copyAwsCredentialsModule(
            ErlangClientSettings settings,
            FileManifest fileManifest) throws IOException {
        
        LOGGER.info("Copying AWS credentials module to generated output");
        
        // Read aws_credentials.erl from resources
        java.io.InputStream credStream = getClass().getClassLoader().getResourceAsStream("aws_credentials.erl");
        if (credStream == null) {
            LOGGER.warning("aws_credentials.erl not found in resources, skipping");
            return;
        }
        
        String credContent = new String(credStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
        credStream.close();
        
        // Determine output path
        Path outputPath;
        boolean useCustomDir = settings.getOutputDir() != null && !settings.getOutputDir().isEmpty();
        
        if (useCustomDir) {
            // Navigate up from build/smithy/source/projection/ to project root, then resolve outputDir
            Path baseDir = fileManifest.getBaseDir();
            Path projectRoot = baseDir;
            
            // Try to navigate up 4 levels (typical Smithy build structure)
            for (int i = 0; i < 4 && projectRoot != null && projectRoot.getParent() != null; i++) {
                projectRoot = projectRoot.getParent();
            }
            
            // If projectRoot is null, fall back to baseDir
            if (projectRoot == null) {
                projectRoot = baseDir;
            }
            
            Path customOutputDir = projectRoot.resolve(settings.getOutputDir());
            outputPath = customOutputDir.resolve("aws_credentials.erl");
        } else {
            // Use default FileManifest location
            outputPath = fileManifest.getBaseDir().resolve("src/aws_credentials.erl");
        }
        
        // Write the file
        if (useCustomDir) {
            try {
                if (outputPath.getParent() != null) {
                    Files.createDirectories(outputPath.getParent());
                }
                Files.writeString(outputPath, credContent);
                LOGGER.info("Copied AWS credentials module: " + outputPath);
            } catch (java.nio.file.FileSystemException e) {
                // Fall back to FileManifest if we can't write to filesystem
                LOGGER.warning("Cannot write to custom directory, using FileManifest instead: " + e.getMessage());
                Path manifestPath = fileManifest.getBaseDir().resolve("src/aws_credentials.erl");
                fileManifest.writeFile(manifestPath, credContent);
            }
        } else {
            fileManifest.writeFile(outputPath, credContent);
        }
    }
    
    private void copyAwsConfigModule(
            ErlangClientSettings settings,
            FileManifest fileManifest) throws IOException {
        
        LOGGER.info("Copying AWS config module to generated output");
        
        // Read aws_config.erl from resources
        java.io.InputStream configStream = getClass().getClassLoader().getResourceAsStream("aws_config.erl");
        if (configStream == null) {
            LOGGER.warning("aws_config.erl not found in resources, skipping");
            return;
        }
        
        String configContent = new String(configStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
        configStream.close();
        
        // Determine output path
        Path outputPath;
        boolean useCustomDir = settings.getOutputDir() != null && !settings.getOutputDir().isEmpty();
        
        if (useCustomDir) {
            // Navigate up from build/smithy/source/projection/ to project root, then resolve outputDir
            Path baseDir = fileManifest.getBaseDir();
            Path projectRoot = baseDir;
            
            // Try to navigate up 4 levels (typical Smithy build structure)
            for (int i = 0; i < 4 && projectRoot != null && projectRoot.getParent() != null; i++) {
                projectRoot = projectRoot.getParent();
            }
            
            // If projectRoot is null, fall back to baseDir
            if (projectRoot == null) {
                projectRoot = baseDir;
            }
            
            Path customOutputDir = projectRoot.resolve(settings.getOutputDir());
            outputPath = customOutputDir.resolve("aws_config.erl");
        } else {
            // Use default FileManifest location
            outputPath = fileManifest.getBaseDir().resolve("src/aws_config.erl");
        }
        
        // Write the file
        if (useCustomDir) {
            try {
                if (outputPath.getParent() != null) {
                    Files.createDirectories(outputPath.getParent());
                }
                Files.writeString(outputPath, configContent);
                LOGGER.info("Copied AWS config module: " + outputPath);
            } catch (java.nio.file.FileSystemException e) {
                // Fall back to FileManifest if we can't write to filesystem
                LOGGER.warning("Cannot write to custom directory, using FileManifest instead: " + e.getMessage());
                Path manifestPath = fileManifest.getBaseDir().resolve("src/aws_config.erl");
                fileManifest.writeFile(manifestPath, configContent);
            }
        } else {
            fileManifest.writeFile(outputPath, configContent);
        }
    }
    
    /**
     * Generate a union type definition as Erlang tagged tuples.
     * Format: -type storage_type() :: {s3, #s3_storage{}} | {glacier, #glacier_storage{}} | ...
     */
    private void generateUnion(
            UnionShape union,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangWriter writer) {
        
        String typeName = ErlangSymbolProvider.toErlangName(union.getId().getName());
        
        writer.write("");
        writer.writeComment("Union type for " + union.getId().getName());
        writer.writeComment("Represented as tagged tuples: {variant_name, variant_data}");
        
        // Start type definition
        writer.write("-type $L() :: ", typeName);
        
        // Generate union variants
        List<MemberShape> members = new ArrayList<>(union.getAllMembers().values());
        for (int i = 0; i < members.size(); i++) {
            MemberShape member = members.get(i);
            String variantName = ErlangSymbolProvider.toErlangName(member.getMemberName());
            Shape targetShape = model.expectShape(member.getTarget());
            
            // Get the type representation for the variant
            String variantType;
            if (targetShape.isStructureShape()) {
                // Structure: use record syntax
                String recordName = ErlangSymbolProvider.toErlangName(targetShape.getId().getName());
                variantType = "#" + recordName + "{}";
            } else {
                // Primitive or other type: use type name
                variantType = symbolProvider.getErlangType(targetShape);
            }
            
            // Add pipe separator for all but first variant
            if (i > 0) {
                writer.writeInline("    | ");
            } else {
                writer.writeInline("    ");
            }
            
            // Write variant: {variant_name, variant_type}
            writer.writeInline("{$L, $L}", variantName, variantType);
            
            if (i < members.size() - 1) {
                writer.write("");
            }
        }
        
        // Close type definition
        writer.write(".");
    }
    
    /**
     * Check if a shape is a union.
     */
    private boolean isUnion(Shape shape) {
        return shape.isUnionShape();
    }
}
