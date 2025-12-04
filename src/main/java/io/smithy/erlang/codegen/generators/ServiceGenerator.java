package io.smithy.erlang.codegen.generators;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.codegen.core.Symbol;
import software.amazon.smithy.codegen.core.SymbolProvider;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.EnumTrait;
import software.amazon.smithy.model.traits.PaginatedTrait;
import software.amazon.smithy.model.traits.RequiredTrait;

import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * Generates the client module for a Smithy service.
 * 
 * <p>This generator creates the main Erlang client module containing:
 * <ul>
 *   <li>Module header and exports</li>
 *   <li>Type definitions (structures, enums, unions)</li>
 *   <li>Client constructor functions ({@code new/1})</li>
 *   <li>Operation functions for each service operation</li>
 *   <li>Helper functions (encoding/decoding, validation)</li>
 * </ul>
 * 
 * <p>Example usage:
 * <pre>
 * ServiceGenerator generator = new ServiceGenerator(serviceShape, context);
 * generator.generate();
 * </pre>
 * 
 * <p>The generated module follows Erlang conventions:
 * <ul>
 *   <li>Module name in snake_case</li>
 *   <li>Type specifications for all exported functions</li>
 *   <li>EDoc comments for documentation</li>
 *   <li>Dialyzer-compatible type annotations</li>
 * </ul>
 * 
 * @see ErlangContext
 * @see ErlangWriter
 */
public final class ServiceGenerator {
    
    private static final Logger LOGGER = Logger.getLogger(ServiceGenerator.class.getName());
    
    private final ServiceShape service;
    private final ErlangContext context;
    private final Model model;
    private final SymbolProvider symbolProvider;
    
    // Collected shapes for generation
    private final List<OperationShape> operations;
    private final Set<ShapeId> processedStructures;
    private final List<StructureShape> structuresToGenerate;
    private final Set<ShapeId> allUnions;
    private final List<UnionShape> unionsToProcess;
    private final Set<ShapeId> allEnums;
    private final List<StringShape> enumsToProcess;
    private final List<StructureShape> inputStructuresToValidate;
    
    /**
     * Creates a new service generator.
     *
     * @param service The service shape to generate
     * @param context The code generation context
     */
    public ServiceGenerator(ServiceShape service, ErlangContext context) {
        this.service = Objects.requireNonNull(service, "service is required");
        this.context = Objects.requireNonNull(context, "context is required");
        this.model = context.model();
        this.symbolProvider = context.symbolProvider();
        
        // Initialize collection structures
        this.operations = collectOperations();
        this.processedStructures = new HashSet<>();
        this.structuresToGenerate = new ArrayList<>();
        this.allUnions = new HashSet<>();
        this.unionsToProcess = new ArrayList<>();
        this.allEnums = new HashSet<>();
        this.enumsToProcess = new ArrayList<>();
        this.inputStructuresToValidate = new ArrayList<>();
        
        // Collect all shapes needed for generation
        collectShapes();
    }
    
    /**
     * Gets the service shape being generated.
     *
     * @return The service shape
     */
    public ServiceShape getService() {
        return service;
    }
    
    /**
     * Gets the code generation context.
     *
     * @return The context
     */
    public ErlangContext getContext() {
        return context;
    }
    
    /**
     * Gets the collected operations for the service.
     *
     * @return Unmodifiable list of operations
     */
    public List<OperationShape> getOperations() {
        return Collections.unmodifiableList(operations);
    }
    
    /**
     * Gets the structures collected for generation.
     *
     * @return Unmodifiable list of structures
     */
    public List<StructureShape> getStructures() {
        return Collections.unmodifiableList(structuresToGenerate);
    }
    
    /**
     * Gets the unions collected for generation.
     *
     * @return Unmodifiable list of unions
     */
    public List<UnionShape> getUnions() {
        return Collections.unmodifiableList(unionsToProcess);
    }
    
    /**
     * Gets the enums collected for generation.
     *
     * @return Unmodifiable list of enums
     */
    public List<StringShape> getEnums() {
        return Collections.unmodifiableList(enumsToProcess);
    }
    
    /**
     * Gets the input structures that require validation.
     *
     * @return Unmodifiable list of structures with required fields
     */
    public List<StructureShape> getInputStructuresToValidate() {
        return Collections.unmodifiableList(inputStructuresToValidate);
    }
    
    /**
     * Generates the complete client module.
     * 
     * <p>This method writes the module file using the writer delegator
     * from the context. The file is written to the location determined
     * by the symbol provider.
     */
    public void generate() {
        String moduleName = getModuleName();
        String filename = moduleName + ".erl";
        
        LOGGER.fine("Generating service module: " + filename);
        
        context.writerDelegator().useFileWriter(filename, writer -> {
            writeModuleHeader(writer, moduleName);
            writeExports(writer);
            writeTypeExports(writer);
            writeHelperExports(writer);
            writeDialyzerSuppressions(writer);
            writeTypeDefinitions(writer);
            
            // Note: Client constructor and operation functions are still
            // generated by ErlangClientPlugin. This will be migrated in
            // future steps when protocol integration is complete.
        });
    }
    
    /**
     * Gets the Erlang module name for this service.
     *
     * @return The module name in snake_case
     */
    public String getModuleName() {
        String moduleName = context.settings().moduleName();
        if (moduleName != null && !moduleName.isEmpty()) {
            return moduleName;
        }
        return EnhancedErlangSymbolProvider.toErlangName(service.getId().getName());
    }
    
    /**
     * Writes the module header including module declaration and comments.
     */
    void writeModuleHeader(ErlangWriter writer, String moduleName) {
        writer.writeModuleHeader(moduleName);
        writer.write("");
        writer.writeComment("Generated Smithy client for " + service.getId().getName());
        writer.write("");
    }
    
    /**
     * Writes the main API exports (-export([...])).
     */
    void writeExports(ErlangWriter writer) {
        List<String> apiExports = new ArrayList<>();
        apiExports.add("new/1");
        
        for (OperationShape operation : operations) {
            String opName = EnhancedErlangSymbolProvider.toErlangName(operation.getId().getName());
            apiExports.add(opName + "/2");
            apiExports.add(opName + "/3"); // 3-arity version for retry support
            
            // Add pagination helper exports if operation is paginated
            if (operation.hasTrait(PaginatedTrait.class)) {
                apiExports.add(opName + "_all_pages/2");
                apiExports.add(opName + "_all_pages/3");
            }
        }
        
        writer.writeExports(apiExports.toArray(new String[0]));
        writer.write("");
    }
    
    /**
     * Writes the type exports (-export_type([...])).
     */
    void writeTypeExports(ErlangWriter writer) {
        if (structuresToGenerate.isEmpty() && enumsToProcess.isEmpty() && unionsToProcess.isEmpty()) {
            return;
        }
        
        List<String> typeExports = new ArrayList<>();
        
        // Export structure types
        for (StructureShape structure : structuresToGenerate) {
            String typeName = EnhancedErlangSymbolProvider.toErlangName(structure.getId().getName());
            typeExports.add(typeName + "/0");
        }
        
        // Export enum types
        for (StringShape enumShape : enumsToProcess) {
            String enumName = EnhancedErlangSymbolProvider.toErlangName(enumShape.getId().getName());
            typeExports.add(enumName + "/0");
        }
        
        // Export union types
        for (UnionShape union : unionsToProcess) {
            String unionName = EnhancedErlangSymbolProvider.toErlangName(union.getId().getName());
            typeExports.add(unionName + "/0");
        }
        
        writer.write("-export_type([");
        for (int i = 0; i < typeExports.size(); i++) {
            if (i > 0) {
                writer.writeInline("             ");
            }
            writer.writeInline(typeExports.get(i));
            if (i < typeExports.size() - 1) {
                writer.write(",");
            } else {
                writer.write("");
            }
        }
        writer.write("        ]).");
        writer.write("");
    }
    
    /**
     * Writes the helper function exports (encoding, decoding, validation).
     */
    void writeHelperExports(ErlangWriter writer) {
        if (unionsToProcess.isEmpty() && enumsToProcess.isEmpty() && inputStructuresToValidate.isEmpty()) {
            return;
        }
        
        List<String> helperExports = new ArrayList<>();
        
        // Union encoding/decoding functions
        for (UnionShape union : unionsToProcess) {
            String unionName = EnhancedErlangSymbolProvider.toErlangName(union.getId().getName());
            helperExports.add("encode_" + unionName + "/1");
        }
        for (UnionShape union : unionsToProcess) {
            String unionName = EnhancedErlangSymbolProvider.toErlangName(union.getId().getName());
            helperExports.add("decode_" + unionName + "/1");
        }
        
        // Enum encoding/decoding functions
        for (StringShape enumShape : enumsToProcess) {
            String enumName = EnhancedErlangSymbolProvider.toErlangName(enumShape.getId().getName());
            helperExports.add("encode_" + enumName + "/1");
        }
        for (StringShape enumShape : enumsToProcess) {
            String enumName = EnhancedErlangSymbolProvider.toErlangName(enumShape.getId().getName());
            helperExports.add("decode_" + enumName + "/1");
        }
        
        // Validation functions
        for (StructureShape inputStructure : inputStructuresToValidate) {
            String structureName = EnhancedErlangSymbolProvider.toErlangName(inputStructure.getId().getName());
            helperExports.add("validate_" + structureName + "/1");
        }
        
        writer.write("-export([");
        for (int i = 0; i < helperExports.size(); i++) {
            if (i > 0) {
                writer.writeInline("         ");
            }
            writer.writeInline(helperExports.get(i));
            if (i < helperExports.size() - 1) {
                writer.write(",");
            } else {
                writer.write("");
            }
        }
        writer.write("        ]).");
        writer.write("");
    }
    
    /**
     * Writes dialyzer suppression directives.
     */
    void writeDialyzerSuppressions(ErlangWriter writer) {
        writer.write("");
        writer.writeComment("Suppress dialyzer warnings");
        writer.writeComment("For example:");
        writer.writeComment("\"The pattern ... can never match the type ...\".");
        writer.write("-dialyzer([no_contracts, no_match]).");
        writer.write("");
    }
    
    /**
     * Writes type definitions (structures, enums, unions).
     */
    void writeTypeDefinitions(ErlangWriter writer) {
        if (structuresToGenerate.isEmpty() && enumsToProcess.isEmpty() && unionsToProcess.isEmpty()) {
            return;
        }
        
        writer.writeComment("Type definitions");
        writer.write("");
        
        // Note: Actual type generation is still delegated to ErlangClientPlugin
        // This will be migrated in future phases when we create
        // StructureGenerator, EnumGenerator, and UnionGenerator
    }
    
    // ========== Collection Methods ==========
    
    private List<OperationShape> collectOperations() {
        return service.getOperations().stream()
                .map(shapeId -> model.expectShape(shapeId, OperationShape.class))
                .collect(Collectors.toList());
    }
    
    private void collectShapes() {
        for (OperationShape operation : operations) {
            collectFromOperation(operation);
        }
        
        // Collect structures from unions
        collectStructuresFromUnions();
    }
    
    private void collectFromOperation(OperationShape operation) {
        // Collect from input
        operation.getInput().ifPresent(inputId -> {
            StructureShape inputShape = model.expectShape(inputId, StructureShape.class);
            collectStructuresRecursive(inputShape);
            collectUnionsFromStructure(inputShape);
            collectEnumsFromStructure(inputShape);
            
            // Check if structure has required fields
            boolean hasRequiredFields = inputShape.getAllMembers().values().stream()
                    .anyMatch(member -> member.hasTrait(RequiredTrait.class));
            
            if (hasRequiredFields) {
                inputStructuresToValidate.add(inputShape);
            }
        });
        
        // Collect from output
        operation.getOutput().ifPresent(outputId -> {
            StructureShape outputShape = model.expectShape(outputId, StructureShape.class);
            collectStructuresRecursive(outputShape);
            collectUnionsFromStructure(outputShape);
            collectEnumsFromStructure(outputShape);
        });
        
        // Collect error structures
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
        
        // Recursively collect member structures
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
                    collectStructuresRecursive((StructureShape) targetShape);
                }
            }
        }
    }
}
