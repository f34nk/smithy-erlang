package io.smithy.erlang.codegen;

import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.build.PluginContext;
import software.amazon.smithy.build.SmithyBuildPlugin;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.HttpLabelTrait;
import software.amazon.smithy.model.traits.HttpTrait;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;

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
            
            // Generate types module
            generateTypesModule(service, model, symbolProvider, settings, fileManifest);
            
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
            Path projectRoot = fileManifest.getBaseDir().getParent().getParent().getParent().getParent();
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
            generateOperation(operation, model, symbolProvider, writer);
        }
        
        // Write to file
        if (useCustomDir) {
            // Write directly to filesystem for custom output directory
            Files.createDirectories(outputPath.getParent());
            Files.writeString(outputPath, writer.toString());
            LOGGER.info("Generated client module: " + outputPath);
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
        
        // Find @httpLabel members in input
        List<MemberShape> httpLabelMembers = new ArrayList<>();
        if (operation.getInput().isPresent()) {
            StructureShape input = model.expectShape(operation.getInput().get(), StructureShape.class);
            for (MemberShape member : input.getAllMembers().values()) {
                if (member.hasTrait(HttpLabelTrait.class)) {
                    httpLabelMembers.add(member);
                }
            }
        }
        
        writer.writeComment("Calls the " + operation.getId().getName() + " operation");
        writer.writeSpec(opName, "(Client :: map(), Input :: map()) -> {ok, map()} | {error, term()}");
        writer.writeFunction(opName, "Client, Input", () -> {
            writer.write("Method = <<\"$L\">>,", method);
            writer.write("Endpoint = maps:get(endpoint, Client),");
            writer.write("");
            
            // Generate URI with path parameter substitution
            if (httpLabelMembers.isEmpty()) {
                // No path parameters - use URI as-is
                writer.write("%% No path parameters");
                writer.write("Uri = <<\"$L\">>,", uri);
                writer.write("Url = <<Endpoint/binary, Uri/binary>>,");
            } else {
                // Path parameters need substitution
                writer.write("%% Build URL with path parameters");
                writer.write("Uri0 = <<\"$L\">>,", uri);
                
                // Parse URI template to get labels
                UriTemplate template = new UriTemplate(uri);
                
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
                
                writer.write("Url = <<Endpoint/binary, Uri$L/binary>>,", httpLabelMembers.size());
            }
            
            writer.write("");
            writer.write("Body = jsx:encode(Input),");
            writer.write("Headers = [{<<\"Content-Type\">>, <<\"application/json\">>}],");
            writer.write("");
            writer.write("%% Make HTTP request");
            writer.write("Request = case Method of");
            writer.indent();
            writer.write("<<\"GET\">> -> {binary_to_list(Url), [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers]};");
            writer.write("<<\"DELETE\">> -> {binary_to_list(Url), [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers]};");
            writer.write("_ -> {binary_to_list(Url), [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers], \"application/json\", binary_to_list(Body)}");
            writer.dedent();
            writer.write("end,");
            writer.write("");
            writer.write("case httpc:request(binary_to_atom(string:lowercase(Method), utf8), Request, [], [{body_format, binary}]) of");
            writer.indent();
            writer.write("{ok, {{_, 200, _}, _, ResponseBody}} ->");
            writer.indent();
            writer.write("{ok, jsx:decode(ResponseBody, [return_maps])};");
            writer.dedent();
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
        });
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
    
    private void generateTypesModule(
            ServiceShape service,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangClientSettings settings,
            FileManifest fileManifest) throws IOException {
        
        String moduleName = settings.getModule() + "_types";
        
        // Determine output path
        Path outputPath;
        boolean useCustomDir = settings.getOutputDir() != null && !settings.getOutputDir().isEmpty();
        
        if (useCustomDir) {
            // Navigate up from build/smithy/source/projection/ to project root, then resolve outputDir
            // Files go directly to outputDir (no src/ subdirectory)
            Path projectRoot = fileManifest.getBaseDir().getParent().getParent().getParent().getParent();
            Path customOutputDir = projectRoot.resolve(settings.getOutputDir());
            outputPath = customOutputDir.resolve(moduleName + ".erl");
        } else {
            // Use default FileManifest location (includes src/ subdirectory)
            outputPath = fileManifest.getBaseDir().resolve("src/" + moduleName + ".erl");
        }
        
        ErlangWriter writer = new ErlangWriter();
        
        writer.writeModule(moduleName);
        writer.write("");
        writer.writeComment("Generated types for Smithy model");
        writer.write("");
        
        // Track all structures and collect them
        java.util.Set<ShapeId> allStructures = new java.util.HashSet<>();
        List<StructureShape> structuresToProcess = new ArrayList<>();
        
        // Collect all structures from operations
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
        
        // Collect all nested structures
        int previousSize = -1;
        while (structuresToProcess.size() != previousSize) {
            previousSize = structuresToProcess.size();
            List<StructureShape> currentStructures = new ArrayList<>(structuresToProcess);
            
            for (StructureShape structure : currentStructures) {
                for (MemberShape member : structure.getAllMembers().values()) {
                    Shape targetShape = model.expectShape(member.getTarget());
                    collectNestedStructures(targetShape, model, allStructures, structuresToProcess);
                }
            }
        }
        
        // Sort structures by dependencies (topological sort)
        List<StructureShape> sortedStructures = topologicalSort(structuresToProcess, model);
        
        // Generate structures in dependency order
        for (StructureShape structure : sortedStructures) {
            generateStructure(structure, model, symbolProvider, writer);
        }
        
        // Write to file
        if (useCustomDir) {
            // Write directly to filesystem for custom output directory
            Files.createDirectories(outputPath.getParent());
            Files.writeString(outputPath, writer.toString());
            LOGGER.info("Generated types module: " + outputPath);
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
    
    private void collectNestedStructures(
            Shape shape,
            Model model,
            java.util.Set<ShapeId> generatedStructures,
            List<StructureShape> structuresToGenerate) {
        
        if (shape instanceof StructureShape) {
            if (generatedStructures.add(shape.getId())) {
                structuresToGenerate.add((StructureShape) shape);
            }
        } else if (shape instanceof ListShape) {
            Shape memberShape = model.expectShape(((ListShape) shape).getMember().getTarget());
            collectNestedStructures(memberShape, model, generatedStructures, structuresToGenerate);
        } else if (shape instanceof MapShape) {
            MapShape mapShape = (MapShape) shape;
            Shape keyShape = model.expectShape(mapShape.getKey().getTarget());
            Shape valueShape = model.expectShape(mapShape.getValue().getTarget());
            collectNestedStructures(keyShape, model, generatedStructures, structuresToGenerate);
            collectNestedStructures(valueShape, model, generatedStructures, structuresToGenerate);
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
}
