package io.smithy.erlang.codegen;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import io.smithy.erlang.codegen.aws.AwsProtocol;
import io.smithy.erlang.codegen.aws.AwsProtocolDetector;
import io.smithy.erlang.codegen.protocols.*;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.*;

/**
 * Core client module code generation logic.
 * 
 * <p>This class contains the actual Erlang client code generation logic
 * used by the DirectedCodegen architecture via ErlangGenerator.
 * 
 * <p>The generated module includes:
 * <ul>
 *   <li>Module header and exports</li>
 *   <li>Type definitions (structures, enums, unions)</li>
 *   <li>Client constructor functions</li>
 *   <li>Operation functions for each service operation</li>
 *   <li>Helper functions (encoding/decoding, validation)</li>
 * </ul>
 */
public final class ClientModuleWriter {
    
    private static final Logger LOGGER = Logger.getLogger(ClientModuleWriter.class.getName());
    
    private final ServiceShape service;
    private final Model model;
    private final String moduleName;
    private final FileManifest fileManifest;
    private final String outputDir;
    
    // Collected shapes
    private final List<OperationShape> operations;
    private final Set<ShapeId> processedStructures = new HashSet<>();
    private final List<StructureShape> structuresToGenerate = new ArrayList<>();
    private final Set<ShapeId> allUnions = new HashSet<>();
    private final List<UnionShape> unionsToProcess = new ArrayList<>();
    private final Set<ShapeId> allEnums = new HashSet<>();
    private final List<StringShape> enumsToProcess = new ArrayList<>();
    private final List<StructureShape> inputStructuresToValidate = new ArrayList<>();
    
    /**
     * Creates a new client module writer.
     *
     * @param service The service shape
     * @param model The Smithy model
     * @param moduleName The module name
     * @param fileManifest The file manifest for output
     * @param outputDir Optional output directory
     */
    public ClientModuleWriter(ServiceShape service, Model model, String moduleName,
                               FileManifest fileManifest, String outputDir) {
        this.service = service;
        this.model = model;
        this.moduleName = moduleName;
        this.fileManifest = fileManifest;
        this.outputDir = outputDir;
        
        // Collect operations
        this.operations = service.getOperations().stream()
                .map(shapeId -> model.expectShape(shapeId, OperationShape.class))
                .collect(Collectors.toList());
        
        // Collect all shapes
        collectShapes();
    }
    
    /**
     * Creates a writer using ErlangContext.
     */
    public static ClientModuleWriter fromContext(ErlangContext context) {
        ServiceShape service = context.model().expectShape(
                context.settings().service(), ServiceShape.class);
        String moduleName = context.settings().moduleName();
        if (moduleName == null || moduleName.isEmpty()) {
            moduleName = EnhancedErlangSymbolProvider.toErlangName(service.getId().getName());
        }
        return new ClientModuleWriter(
                service,
                context.model(),
                moduleName,
                context.fileManifest(),
                context.settings().outputDir()
        );
    }
    
    /**
     * Generates the complete client module.
     */
    public void generate() throws IOException {
        ErlangWriter writer = new ErlangWriter(moduleName);
        
        // Module declaration
        writer.writeModuleHeader(moduleName);
        writer.writeComment("Generated Smithy client for " + service.getId().getName());
        writer.writeBlankLine();
        
        // Sort structures topologically
        List<StructureShape> sortedStructures = topologicalSort(structuresToGenerate);
        
        // Generate exports
        writeApiExports(writer);
        writeTypeExports(writer, sortedStructures);
        writeHelperExports(writer);
        writeDialyzerSuppressions(writer);
        
        // Generate type definitions
        writeTypeDefinitions(writer, sortedStructures);
        
        // Generate new/1 function
        writeClientConstructor(writer);
        
        // Generate URL encoding helper
        writeUrlEncodeHelper(writer);
        
        // Generate operation functions
        for (OperationShape operation : operations) {
            generateOperation(operation, writer);
            
            // Generate pagination helper if operation is paginated
            Optional<PaginatedTrait> paginationInfo = getPaginationInfo(operation);
            if (paginationInfo.isPresent()) {
                generatePaginationHelper(operation, paginationInfo.get(), writer);
            }
        }
        
        // Generate helper functions
        writeHelperFunctions(writer);
        
        // Write to file
        writeToFile(writer);
    }
    
    /**
     * Copies all required runtime modules to the output directory.
     */
    public void copyRuntimeModules() throws IOException {
        copyModule("aws_sigv4.erl");
        copyModule("aws_credentials.erl");
        copyModule("aws_config.erl");
        copyModule("aws_retry.erl");
        copyModule("aws_xml.erl");
        copyModule("aws_query.erl");
        copyModule("aws_s3.erl");
        copyModule("aws_endpoints.erl");
    }
    
    // ========== Shape Collection ==========
    
    private void collectShapes() {
        for (OperationShape operation : operations) {
            collectFromOperation(operation);
        }
        collectStructuresFromUnions();
    }
    
    private void collectFromOperation(OperationShape operation) {
        operation.getInput().ifPresent(inputId -> {
            StructureShape inputShape = model.expectShape(inputId, StructureShape.class);
            collectStructuresRecursive(inputShape);
            collectUnionsFromStructure(inputShape);
            collectEnumsFromStructure(inputShape);
            
            boolean hasRequiredFields = inputShape.getAllMembers().values().stream()
                    .anyMatch(member -> member.hasTrait(RequiredTrait.class));
            if (hasRequiredFields) {
                inputStructuresToValidate.add(inputShape);
            }
        });
        
        operation.getOutput().ifPresent(outputId -> {
            StructureShape outputShape = model.expectShape(outputId, StructureShape.class);
            collectStructuresRecursive(outputShape);
            collectUnionsFromStructure(outputShape);
            collectEnumsFromStructure(outputShape);
        });
        
        for (ShapeId errorId : operation.getErrors()) {
            StructureShape errorShape = model.expectShape(errorId, StructureShape.class);
            collectStructuresRecursive(errorShape);
        }
    }
    
    private void collectStructuresRecursive(StructureShape structure) {
        if (processedStructures.contains(structure.getId())) {
            return;
        }
        processedStructures.add(structure.getId());
        structuresToGenerate.add(structure);
        
        for (MemberShape member : structure.getAllMembers().values()) {
            Shape targetShape = model.expectShape(member.getTarget());
            
            if (targetShape instanceof StructureShape) {
                collectStructuresRecursive((StructureShape) targetShape);
            } else if (targetShape instanceof ListShape) {
                ListShape listShape = (ListShape) targetShape;
                Shape memberTarget = model.expectShape(listShape.getMember().getTarget());
                if (memberTarget instanceof StructureShape) {
                    collectStructuresRecursive((StructureShape) memberTarget);
                }
            } else if (targetShape instanceof MapShape) {
                MapShape mapShape = (MapShape) targetShape;
                Shape valueTarget = model.expectShape(mapShape.getValue().getTarget());
                if (valueTarget instanceof StructureShape) {
                    collectStructuresRecursive((StructureShape) valueTarget);
                }
            }
        }
    }
    
    private void collectUnionsFromStructure(StructureShape structure) {
        for (MemberShape member : structure.getAllMembers().values()) {
            Shape targetShape = model.expectShape(member.getTarget());
            
            if (targetShape instanceof UnionShape && !allUnions.contains(targetShape.getId())) {
                allUnions.add(targetShape.getId());
                unionsToProcess.add((UnionShape) targetShape);
            } else if (targetShape instanceof ListShape) {
                ListShape listShape = (ListShape) targetShape;
                Shape memberTarget = model.expectShape(listShape.getMember().getTarget());
                if (memberTarget instanceof UnionShape && !allUnions.contains(memberTarget.getId())) {
                    allUnions.add(memberTarget.getId());
                    unionsToProcess.add((UnionShape) memberTarget);
                }
            }
        }
    }
    
    private void collectEnumsFromStructure(StructureShape structure) {
        for (MemberShape member : structure.getAllMembers().values()) {
            Shape targetShape = model.expectShape(member.getTarget());
            
            if (targetShape instanceof StringShape) {
                StringShape stringShape = (StringShape) targetShape;
                if (stringShape.hasTrait(EnumTrait.class) && !allEnums.contains(stringShape.getId())) {
                    allEnums.add(stringShape.getId());
                    enumsToProcess.add(stringShape);
                }
            } else if (targetShape instanceof ListShape) {
                ListShape listShape = (ListShape) targetShape;
                Shape memberTarget = model.expectShape(listShape.getMember().getTarget());
                if (memberTarget instanceof StringShape) {
                    StringShape stringShape = (StringShape) memberTarget;
                    if (stringShape.hasTrait(EnumTrait.class) && !allEnums.contains(stringShape.getId())) {
                        allEnums.add(stringShape.getId());
                        enumsToProcess.add(stringShape);
                    }
                }
            }
        }
    }
    
    private void collectStructuresFromUnions() {
        for (UnionShape union : unionsToProcess) {
            for (MemberShape member : union.getAllMembers().values()) {
                Shape targetShape = model.expectShape(member.getTarget());
                if (targetShape instanceof StructureShape) {
                    StructureShape structure = (StructureShape) targetShape;
                    collectStructuresRecursive(structure);
                    // Also collect enums from union member structures
                    collectEnumsFromStructure(structure);
                }
            }
        }
    }
    
    // ========== Export Writers ==========
    
    private void writeApiExports(ErlangWriter writer) {
        List<String> apiExports = new ArrayList<>();
        apiExports.add("new/1");
        
        for (OperationShape operation : operations) {
            String opName = EnhancedErlangSymbolProvider.toErlangName(operation.getId().getName());
            apiExports.add(opName + "/2");
            apiExports.add(opName + "/3");
            
            if (operation.hasTrait(PaginatedTrait.class)) {
                apiExports.add(opName + "_all_pages/2");
                apiExports.add(opName + "_all_pages/3");
            }
        }
        
        writer.writeExports(apiExports.toArray(new String[0]));
        writer.writeBlankLine();
    }
    
    private void writeTypeExports(ErlangWriter writer, List<StructureShape> sortedStructures) {
        if (sortedStructures.isEmpty() && enumsToProcess.isEmpty() && unionsToProcess.isEmpty()) {
            return;
        }
        
        List<String> typeExports = new ArrayList<>();
        
        for (StructureShape structure : sortedStructures) {
            String typeName = EnhancedErlangSymbolProvider.toErlangName(structure.getId().getName());
            typeExports.add(typeName + "/0");
        }
        
        for (StringShape enumShape : enumsToProcess) {
            String enumName = EnhancedErlangSymbolProvider.toErlangName(enumShape.getId().getName());
            typeExports.add(enumName + "/0");
        }
        
        for (UnionShape union : unionsToProcess) {
            String unionName = EnhancedErlangSymbolProvider.toErlangName(union.getId().getName());
            typeExports.add(unionName + "/0");
        }
        
        writer.writeExportTypes(typeExports.toArray(new String[0]));
        writer.writeBlankLine();
    }
    
    private void writeHelperExports(ErlangWriter writer) {
        if (unionsToProcess.isEmpty() && enumsToProcess.isEmpty() && inputStructuresToValidate.isEmpty()) {
            return;
        }
        
        List<String> helperExports = new ArrayList<>();
        
        for (UnionShape union : unionsToProcess) {
            String unionName = EnhancedErlangSymbolProvider.toErlangName(union.getId().getName());
            helperExports.add("encode_" + unionName + "/1");
        }
        for (UnionShape union : unionsToProcess) {
            String unionName = EnhancedErlangSymbolProvider.toErlangName(union.getId().getName());
            helperExports.add("decode_" + unionName + "/1");
        }
        
        for (StringShape enumShape : enumsToProcess) {
            String enumName = EnhancedErlangSymbolProvider.toErlangName(enumShape.getId().getName());
            helperExports.add("encode_" + enumName + "/1");
        }
        for (StringShape enumShape : enumsToProcess) {
            String enumName = EnhancedErlangSymbolProvider.toErlangName(enumShape.getId().getName());
            helperExports.add("decode_" + enumName + "/1");
        }
        
        for (StructureShape inputStructure : inputStructuresToValidate) {
            String structureName = EnhancedErlangSymbolProvider.toErlangName(inputStructure.getId().getName());
            helperExports.add("validate_" + structureName + "/1");
        }
        
        writer.writeExports(helperExports.toArray(new String[0]));
    }
    
    private void writeDialyzerSuppressions(ErlangWriter writer) {
        writer.writeBlankLine();
        writer.writeComment("Suppress dialyzer warnings");
        writer.writeComment("For example:");
        writer.writeComment("\"The pattern ... can never match the type ...\".");
        writer.write("-dialyzer([no_contracts, no_match]).");
        writer.writeBlankLine();
    }
    
    // ========== Type Definition Writers ==========
    
    private void writeTypeDefinitions(ErlangWriter writer, List<StructureShape> sortedStructures) {
        if (sortedStructures.isEmpty() && enumsToProcess.isEmpty() && unionsToProcess.isEmpty()) {
            return;
        }
        
        writer.writeComment("Type definitions");
        writer.writeBlankLine();
        
        for (StructureShape structure : sortedStructures) {
            generateStructure(structure, writer);
        }
        
        for (StringShape enumShape : enumsToProcess) {
            generateEnum(enumShape, writer);
        }
        
        for (UnionShape union : unionsToProcess) {
            generateUnion(union, writer);
        }
    }
    
    private void generateStructure(StructureShape structure, ErlangWriter writer) {
        String typeName = EnhancedErlangSymbolProvider.toErlangName(structure.getId().getName());
        
        List<String> fieldSpecs = new ArrayList<>();
        for (MemberShape member : structure.getAllMembers().values()) {
            String memberName = EnhancedErlangSymbolProvider.toErlangName(member.getMemberName());
            String memberType = getErlangType(member.getTarget());
            fieldSpecs.add(memberName + " => " + memberType);
        }
        
        if (fieldSpecs.isEmpty()) {
            writer.write("-type $L() :: #{}.", typeName);
        } else {
            writer.write("-type $L() :: #{", typeName);
            writer.indent();
            for (int i = 0; i < fieldSpecs.size(); i++) {
                if (i < fieldSpecs.size() - 1) {
                    writer.write("$L,", fieldSpecs.get(i));
                } else {
                    writer.write("$L", fieldSpecs.get(i));
                }
            }
            writer.dedent();
            writer.write("}.");
        }
        writer.writeBlankLine();
    }
    
    private void generateEnum(StringShape enumShape, ErlangWriter writer) {
        String enumName = EnhancedErlangSymbolProvider.toErlangName(enumShape.getId().getName());
        
        Optional<EnumTrait> enumTrait = enumShape.getTrait(EnumTrait.class);
        if (enumTrait.isEmpty()) {
            writer.write("-type $L() :: binary().", enumName);
            return;
        }
        
        List<String> values = enumTrait.get().getValues().stream()
                .map(def -> "'" + def.getValue() + "'")
                .collect(Collectors.toList());
        
        writer.write("-type $L() :: $L.", enumName, String.join(" | ", values));
        writer.writeBlankLine();
    }
    
    private void generateUnion(UnionShape union, ErlangWriter writer) {
        String unionName = EnhancedErlangSymbolProvider.toErlangName(union.getId().getName());
        
        List<String> variants = new ArrayList<>();
        for (MemberShape member : union.getAllMembers().values()) {
            String memberName = EnhancedErlangSymbolProvider.toErlangName(member.getMemberName());
            String memberType = getErlangType(member.getTarget());
            variants.add("{" + memberName + ", " + memberType + "}");
        }
        
        // Add unknown variant
        variants.add("{unknown, term()}");
        
        writer.write("-type $L() :: $L.", unionName, String.join(" | ", variants));
        writer.writeBlankLine();
    }
    
    // ========== Function Writers ==========
    
    private void writeClientConstructor(ErlangWriter writer) {
        writer.writeComment("Creates a new client with the given configuration");
        writer.writeSpec("new", "Config :: map()", "{ok, map()}");
        writer.writeFunction("new", "Config", () -> {
            writer.write("{ok, Config}.");
        });
    }
    
    private void writeUrlEncodeHelper(ErlangWriter writer) {
        writer.writeBlankLine();
        writer.writeComment("URL encode a value for use in URI path parameters");
        writer.writeBlankLine();
        writer.write("url_encode(Binary) when is_binary(Binary) ->");
        writer.indent();
        writer.write("url_encode(binary_to_list(Binary));");
        writer.dedent();
        writer.write("url_encode(String) when is_list(String) ->");
        writer.indent();
        writer.write("list_to_binary(uri_string:quote(String)).");
        writer.dedent();
        writer.writeBlankLine();
        
        writer.writeComment("Convert a value to binary for use in URI substitution");
        writer.writeBlankLine();
        writer.write("ensure_binary(Bin) when is_binary(Bin) -> Bin;");
        writer.write("ensure_binary(List) when is_list(List) -> list_to_binary(List);");
        writer.write("ensure_binary(Int) when is_integer(Int) -> integer_to_binary(Int);");
        writer.write("ensure_binary(Float) when is_float(Float) -> float_to_binary(Float);");
        writer.write("ensure_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);");
        writer.write("ensure_binary(Other) -> list_to_binary(io_lib:format(\"~p\", [Other])).");
        writer.writeBlankLine();
    }
    
    private void generateOperation(OperationShape operation, ErlangWriter writer) {
        AwsProtocol awsProtocol = AwsProtocolDetector.detectProtocol(service);
        LOGGER.fine("Generating operation " + operation.getId().getName() + 
                   " with protocol " + awsProtocol.name());
        
        // Create context for protocol generator
        ErlangSettings settings = ErlangSettings.builder()
                .service(service.getId())
                .moduleName(moduleName)
                .build();
        EnhancedErlangSymbolProvider symbolProvider = new EnhancedErlangSymbolProvider(model, settings);
        software.amazon.smithy.codegen.core.WriterDelegator<ErlangWriter> writerDelegator = 
                new software.amazon.smithy.codegen.core.WriterDelegator<>(
                        fileManifest, symbolProvider, ErlangWriter.factory());
        ErlangContext context = ErlangContext.builder()
                .model(model)
                .settings(settings)
                .symbolProvider(symbolProvider)
                .fileManifest(fileManifest)
                .writerDelegator(writerDelegator)
                .build();
        
        ProtocolGenerator generator = createProtocolGenerator(awsProtocol);
        if (generator != null) {
            generator.generateOperation(operation, writer, context);
        } else {
            LOGGER.warning("Protocol " + awsProtocol.name() + " not implemented");
            generateDefaultOperation(operation, writer);
        }
    }
    
    private ProtocolGenerator createProtocolGenerator(AwsProtocol protocol) {
        switch (protocol) {
            case AWS_JSON_1_0:
            case AWS_JSON_1_1:
                return new AwsJsonProtocolGenerator();
            case REST_JSON_1:
                return new RestJsonProtocolGenerator();
            case REST_XML:
                return new RestXmlProtocolGenerator();
            case AWS_QUERY:
                return new AwsQueryProtocolGenerator();
            case EC2_QUERY:
                return new Ec2QueryProtocolGenerator();
            default:
                return null;
        }
    }
    
    private void generateDefaultOperation(OperationShape operation, ErlangWriter writer) {
        String opName = EnhancedErlangSymbolProvider.toErlangName(operation.getId().getName());
        
        // Generate 2-arity version
        writer.writeComment("@doc " + operation.getId().getName() + " operation");
        writer.writeSpec(opName, "Client :: map(), Input :: map()", 
                "{ok, map()} | {error, term()}");
        writer.write("$L(Client, Input) ->", opName);
        writer.indent();
        writer.write("$L(Client, Input, #{}).", opName);
        writer.dedent();
        writer.writeBlankLine();
        
        // Generate 3-arity version
        writer.writeSpec(opName, "Client :: map(), Input :: map(), Options :: map()", 
                "{ok, map()} | {error, term()}");
        writer.write("$L(_Client, _Input, _Options) ->", opName);
        writer.indent();
        writer.write("{error, not_implemented}.");
        writer.dedent();
        writer.writeBlankLine();
    }
    
    private void generatePaginationHelper(OperationShape operation, PaginatedTrait trait, 
                                          ErlangWriter writer) {
        String opName = EnhancedErlangSymbolProvider.toErlangName(operation.getId().getName());
        String helperName = opName + "_all_pages";
        
        String inputToken = trait.getInputToken().orElse(null);
        String outputToken = trait.getOutputToken().orElse(null);
        String items = trait.getItems().orElse(null);
        
        if (inputToken == null || outputToken == null) {
            return;
        }
        
        // Use original JSON key names (camelCase), not snake_case
        String inputTokenJson = inputToken;
        String outputTokenJson = outputToken;
        
        // Generate 2-arity helper
        writer.writeComment("@doc Fetches all pages for " + opName);
        writer.write("$L(Client, Input) ->", helperName);
        writer.indent();
        writer.write("$L(Client, Input, #{}).", helperName);
        writer.dedent();
        writer.writeBlankLine();
        
        // Generate 3-arity helper with recursive implementation
        writer.write("$L(Client, Input, Options) when is_map(Input), is_map(Options) ->", helperName);
        writer.indent();
        writer.write("$L_recursive(Client, Input, Options, []).", helperName);
        writer.dedent();
        writer.writeBlankLine();
        
        // Generate recursive function
        writer.write("$L_recursive(Client, Input, Options, Acc) when is_map(Input), is_map(Options), is_list(Acc) ->", helperName);
        writer.indent();
        writer.write("case $L(Client, Input, Options) of", opName);
        writer.indent();
        
        if (items != null) {
            // With items path
            writer.write("{ok, #{<<\"$L\">> := Items, <<\"$L\">> := NextToken}} when is_binary(NextToken), byte_size(NextToken) > 0 ->", items, outputTokenJson);
            writer.indent();
            writer.write("%% More pages available - continue pagination");
            writer.write("NewAcc = Acc ++ Items,");
            writer.write("NewInput = Input#{<<\"$L\">> => NextToken},", inputTokenJson);
            writer.write("$L_recursive(Client, NewInput, Options, NewAcc);", helperName);
            writer.dedent();
            writer.write("{ok, #{<<\"$L\">> := Items}} ->", items);
            writer.indent();
            writer.write("%% Last page - return accumulated results");
            writer.write("{ok, Acc ++ Items};");
            writer.dedent();
            writer.write("{ok, #{<<\"$L\">> := Items, <<\"$L\">> := <<>>}} ->", items, outputTokenJson);
            writer.indent();
            writer.write("%% Empty token - return accumulated results");
            writer.write("{ok, Acc ++ Items};");
            writer.dedent();
        } else {
            // Without items path
            writer.write("{ok, Result = #{<<\"$L\">> := NextToken}} when is_binary(NextToken), byte_size(NextToken) > 0 ->", outputTokenJson);
            writer.indent();
            writer.write("NewAcc = Acc ++ [Result],");
            writer.write("NewInput = Input#{<<\"$L\">> => NextToken},", inputTokenJson);
            writer.write("$L_recursive(Client, NewInput, Options, NewAcc);", helperName);
            writer.dedent();
            writer.write("{ok, Result} ->", items);
            writer.indent();
            writer.write("{ok, Acc ++ [Result]};");
            writer.dedent();
        }
        
        writer.write("{error, Reason} ->");
        writer.indent();
        writer.write("%% Error occurred - return error");
        writer.write("{error, Reason}");
        writer.dedent();
        writer.dedent();
        writer.write("end.");
        writer.dedent();
        writer.writeBlankLine();
    }
    
    // ========== Helper Function Writers ==========
    
    private void writeHelperFunctions(ErlangWriter writer) {
        if (unionsToProcess.isEmpty() && enumsToProcess.isEmpty() && inputStructuresToValidate.isEmpty()) {
            return;
        }
        
        writer.writeBlankLine();
        writer.writeComment("===================================================================");
        writer.writeComment("Helper Functions");
        writer.writeComment("===================================================================");
        writer.writeBlankLine();
        
        for (UnionShape union : unionsToProcess) {
            generateUnionEncodingFunction(union, writer);
        }
        
        for (UnionShape union : unionsToProcess) {
            generateUnionDecodingFunction(union, writer);
        }
        
        for (StringShape enumShape : enumsToProcess) {
            generateEnumEncodingFunction(enumShape, writer);
        }
        
        for (StringShape enumShape : enumsToProcess) {
            generateEnumDecodingFunction(enumShape, writer);
        }
        
        for (StructureShape inputStructure : inputStructuresToValidate) {
            generateValidationFunction(inputStructure, writer);
        }
    }
    
    private void generateUnionEncodingFunction(UnionShape union, ErlangWriter writer) {
        String unionName = EnhancedErlangSymbolProvider.toErlangName(union.getId().getName());
        String functionName = "encode_" + unionName;
        
        writer.writeSpec(functionName, unionName + "() | {unknown, term()}", "map()");
        
        for (MemberShape member : union.getAllMembers().values()) {
            String memberName = EnhancedErlangSymbolProvider.toErlangName(member.getMemberName());
            String jsonKey = member.getMemberName();
            writer.write("$L({$L, Value}) ->", functionName, memberName);
            writer.indent();
            writer.write("#{<<\"$L\">> => Value};", jsonKey);
            writer.dedent();
        }
        
        writer.write("$L({unknown, Value}) ->", functionName);
        writer.indent();
        writer.write("#{<<\"unknown\">> => Value}.");
        writer.dedent();
        writer.writeBlankLine();
    }
    
    private void generateUnionDecodingFunction(UnionShape union, ErlangWriter writer) {
        String unionName = EnhancedErlangSymbolProvider.toErlangName(union.getId().getName());
        String functionName = "decode_" + unionName;
        
        writer.writeSpec(functionName, "map()", unionName + "() | {unknown, term()}");
        writer.write("$L(Map) when is_map(Map) ->", functionName);
        writer.indent();
        
        List<MemberShape> members = new ArrayList<>(union.getAllMembers().values());
        for (int i = 0; i < members.size(); i++) {
            MemberShape member = members.get(i);
            String memberName = EnhancedErlangSymbolProvider.toErlangName(member.getMemberName());
            String jsonKey = member.getMemberName();
            
            if (i == 0) {
                writer.write("case maps:find(<<\"$L\">>, Map) of", jsonKey);
            } else {
                writer.write("        case maps:find(<<\"$L\">>, Map) of", jsonKey);
            }
            writer.indent();
            writer.write("{ok, Value} -> {$L, Value};", memberName);
            writer.write("error ->");
            writer.dedent();
        }
        
        // Handle unknown
        writer.write("            {unknown, Map}");
        
        // Close all case statements
        for (int i = 0; i < members.size(); i++) {
            writer.write("    end");
        }
        writer.write(".");
        writer.dedent();
        writer.writeBlankLine();
    }
    
    private void generateEnumEncodingFunction(StringShape enumShape, ErlangWriter writer) {
        String enumName = EnhancedErlangSymbolProvider.toErlangName(enumShape.getId().getName());
        String functionName = "encode_" + enumName;
        
        Optional<EnumTrait> enumTrait = enumShape.getTrait(EnumTrait.class);
        if (enumTrait.isEmpty()) {
            writer.writeSpec(functionName, enumName + "() | {unknown, term()}", "binary()");
            writer.write("$L(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);", functionName);
            writer.write("$L({unknown, Value}) -> Value.", functionName);
            writer.writeBlankLine();
            return;
        }
        
        writer.writeSpec(functionName, enumName + "() | {unknown, term()}", "binary()");
        
        for (EnumDefinition def : enumTrait.get().getValues()) {
            String value = def.getValue();
            writer.write("$L('$L') -> <<\"$L\">>;", functionName, value, value);
        }
        
        writer.write("$L({unknown, Value}) -> Value.", functionName);
        writer.writeBlankLine();
    }
    
    private void generateEnumDecodingFunction(StringShape enumShape, ErlangWriter writer) {
        String enumName = EnhancedErlangSymbolProvider.toErlangName(enumShape.getId().getName());
        String functionName = "decode_" + enumName;
        
        Optional<EnumTrait> enumTrait = enumShape.getTrait(EnumTrait.class);
        if (enumTrait.isEmpty()) {
            writer.writeSpec(functionName, "binary()", "{ok, " + enumName + "()} | {error, {invalid_enum_value, binary()}}");
            writer.write("$L(Value) -> {ok, Value}.", functionName);
            writer.writeBlankLine();
            return;
        }
        
        writer.writeSpec(functionName, "binary()", "{ok, " + enumName + "()} | {error, {invalid_enum_value, binary()}}");
        
        for (EnumDefinition def : enumTrait.get().getValues()) {
            String value = def.getValue();
            writer.write("$L(<<\"$L\">>) -> {ok, '$L'};", functionName, value, value);
        }
        
        writer.write("$L(Other) -> {error, {invalid_enum_value, Other}}.", functionName);
        writer.writeBlankLine();
    }
    
    private void generateValidationFunction(StructureShape structure, ErlangWriter writer) {
        String structureName = EnhancedErlangSymbolProvider.toErlangName(structure.getId().getName());
        String functionName = "validate_" + structureName;
        
        List<String> requiredFields = structure.getAllMembers().values().stream()
                .filter(member -> member.hasTrait(RequiredTrait.class))
                .map(member -> "<<\"" + member.getMemberName() + "\">>")
                .collect(Collectors.toList());
        
        writer.writeSpec(functionName, "map()", "ok | {error, {missing_required_fields, [binary()]}}");
        writer.write("$L(Input) ->", functionName);
        writer.indent();
        writer.write("RequiredFields = [$L],", String.join(", ", requiredFields));
        writer.write("Missing = [F || F <- RequiredFields, not maps:is_key(F, Input)],");
        writer.write("case Missing of");
        writer.indent();
        writer.write("[] -> ok;");
        writer.write("_ -> {error, {missing_required_fields, Missing}}");
        writer.dedent();
        writer.write("end.");
        writer.dedent();
        writer.writeBlankLine();
    }
    
    // ========== Utility Methods ==========
    
    private String getErlangType(ShapeId shapeId) {
        Shape shape = model.expectShape(shapeId);
        
        if (shape instanceof StringShape) {
            if (shape.hasTrait(EnumTrait.class)) {
                return EnhancedErlangSymbolProvider.toErlangName(shape.getId().getName()) + "()";
            }
            return "binary()";
        } else if (shape instanceof IntegerShape || shape instanceof LongShape) {
            return "integer()";
        } else if (shape instanceof FloatShape || shape instanceof DoubleShape) {
            return "float()";
        } else if (shape instanceof BooleanShape) {
            return "boolean()";
        } else if (shape instanceof BlobShape) {
            return "binary()";
        } else if (shape instanceof TimestampShape) {
            return "binary()";
        } else if (shape instanceof ListShape) {
            ListShape listShape = (ListShape) shape;
            String memberType = getErlangType(listShape.getMember().getTarget());
            return "[" + memberType + "]";
        } else if (shape instanceof MapShape) {
            MapShape mapShape = (MapShape) shape;
            String keyType = getErlangType(mapShape.getKey().getTarget());
            String valueType = getErlangType(mapShape.getValue().getTarget());
            return "#{" + keyType + " => " + valueType + "}";
        } else if (shape instanceof StructureShape) {
            return EnhancedErlangSymbolProvider.toErlangName(shape.getId().getName()) + "()";
        } else if (shape instanceof UnionShape) {
            return EnhancedErlangSymbolProvider.toErlangName(shape.getId().getName()) + "()";
        }
        
        return "term()";
    }
    
    private List<StructureShape> topologicalSort(List<StructureShape> structures) {
        // Simple implementation - return as-is for now
        // A full implementation would sort by dependencies
        return new ArrayList<>(structures);
    }
    
    private Optional<PaginatedTrait> getPaginationInfo(OperationShape operation) {
        return operation.getTrait(PaginatedTrait.class);
    }
    
    private void writeToFile(ErlangWriter writer) throws IOException {
        Path outputPath;
        boolean useCustomDir = outputDir != null && !outputDir.isEmpty();
        
        if (useCustomDir) {
            Path baseDir = fileManifest.getBaseDir();
            Path projectRoot = baseDir;
            
            for (int i = 0; i < 4 && projectRoot != null && projectRoot.getParent() != null; i++) {
                projectRoot = projectRoot.getParent();
            }
            
            if (projectRoot == null) {
                projectRoot = baseDir;
            }
            
            Path customOutputDir = projectRoot.resolve(outputDir);
            outputPath = customOutputDir.resolve(moduleName + ".erl");
            
            try {
                if (outputPath.getParent() != null) {
                    Files.createDirectories(outputPath.getParent());
                }
                Files.writeString(outputPath, writer.toString());
                LOGGER.info("Generated client module: " + outputPath);
            } catch (java.nio.file.FileSystemException e) {
                LOGGER.warning("Cannot write to custom directory, using FileManifest: " + e.getMessage());
                outputPath = fileManifest.getBaseDir().resolve("src/" + moduleName + ".erl");
                fileManifest.writeFile(outputPath, writer.toString());
            }
        } else {
            outputPath = fileManifest.getBaseDir().resolve("src/" + moduleName + ".erl");
            fileManifest.writeFile(outputPath, writer.toString());
        }
    }
    
    private void copyModule(String filename) throws IOException {
        try (InputStream is = getClass().getResourceAsStream("/" + filename)) {
            if (is == null) {
                LOGGER.warning("Resource not found: /" + filename);
                return;
            }
            
            Path outputPath;
            boolean useCustomDir = outputDir != null && !outputDir.isEmpty();
            
            if (useCustomDir) {
                Path baseDir = fileManifest.getBaseDir();
                Path projectRoot = baseDir;
                
                for (int i = 0; i < 4 && projectRoot != null && projectRoot.getParent() != null; i++) {
                    projectRoot = projectRoot.getParent();
                }
                
                if (projectRoot == null) {
                    projectRoot = baseDir;
                }
                
                outputPath = projectRoot.resolve(outputDir).resolve(filename);
                
                try {
                    if (outputPath.getParent() != null) {
                        Files.createDirectories(outputPath.getParent());
                    }
                    Files.copy(is, outputPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                    LOGGER.fine("Copied " + filename + " to " + outputPath);
                } catch (java.nio.file.FileSystemException e) {
                    outputPath = fileManifest.getBaseDir().resolve(filename);
                    fileManifest.writeFile(outputPath, new String(is.readAllBytes()));
                }
            } else {
                outputPath = fileManifest.getBaseDir().resolve(filename);
                fileManifest.writeFile(outputPath, new String(is.readAllBytes()));
            }
        }
    }
}
