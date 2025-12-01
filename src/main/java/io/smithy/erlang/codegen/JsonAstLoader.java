package io.smithy.erlang.codegen;

import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.loader.ModelAssembler;
import software.amazon.smithy.model.validation.ValidationEvent;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Utility class for loading Smithy models from JSON AST files.
 */
public class JsonAstLoader {
    
    private static final Logger LOGGER = Logger.getLogger(JsonAstLoader.class.getName());
    
    /**
     * Load a Smithy model from a single JSON AST file.
     * 
     * @param jsonFile Path to the JSON AST file
     * @return Assembled Smithy Model
     * @throws RuntimeException if loading fails
     */
    public static Model loadFromJsonFile(Path jsonFile) {
        LOGGER.info("Loading Smithy model from JSON AST file: " + jsonFile);
        
        ModelAssembler assembler = Model.assembler();
        assembler.discoverModels(); // Discover built-in traits from classpath
        assembler.addImport(jsonFile);
        
        return assembleModel(assembler);
    }
    
    /**
     * Load a Smithy model from multiple JSON AST files.
     * 
     * @param jsonFiles List of paths to JSON AST files
     * @return Assembled Smithy Model
     * @throws RuntimeException if loading fails
     */
    public static Model loadFromJsonFiles(List<Path> jsonFiles) {
        LOGGER.info("Loading Smithy model from " + jsonFiles.size() + " JSON AST files");
        
        ModelAssembler assembler = Model.assembler();
        assembler.discoverModels(); // Discover built-in traits from classpath
        for (Path jsonFile : jsonFiles) {
            LOGGER.info("  - Adding: " + jsonFile);
            assembler.addImport(jsonFile);
        }
        
        return assembleModel(assembler);
    }
    
    /**
     * Load all JSON AST files from a directory (recursively).
     * 
     * @param directory Directory containing JSON AST files
     * @return Assembled Smithy Model
     * @throws IOException if directory reading fails
     * @throws RuntimeException if loading fails
     */
    public static Model loadFromDirectory(Path directory) throws IOException {
        LOGGER.info("Loading Smithy models from directory: " + directory);
        
        List<Path> jsonFiles;
        try (Stream<Path> paths = Files.walk(directory)) {
            jsonFiles = paths
                .filter(Files::isRegularFile)
                .filter(path -> path.toString().endsWith(".json"))
                .collect(Collectors.toList());
        }
        
        if (jsonFiles.isEmpty()) {
            throw new RuntimeException("No JSON AST files found in directory: " + directory);
        }
        
        ModelAssembler assembler = Model.assembler();
        assembler.discoverModels(); // Discover built-in traits from classpath
        for (Path jsonFile : jsonFiles) {
            LOGGER.info("  - Adding: " + jsonFile);
            assembler.addImport(jsonFile);
        }
        
        return assembleModel(assembler);
    }
    
    /**
     * Load from mixed sources: both .smithy IDL files and .json AST files.
     * 
     * @param sources List of file or directory paths
     * @return Assembled Smithy Model
     * @throws IOException if file reading fails
     */
    public static Model loadFromMixedSources(List<Path> sources) throws IOException {
        LOGGER.info("Loading Smithy model from mixed sources");
        
        ModelAssembler assembler = Model.assembler();
        assembler.discoverModels(); // Discover built-in traits from classpath
        
        for (Path source : sources) {
            if (Files.isDirectory(source)) {
                // Add all .smithy and .json files from directory
                try (Stream<Path> paths = Files.walk(source)) {
                    paths.filter(Files::isRegularFile)
                         .filter(path -> path.toString().endsWith(".smithy") 
                                      || path.toString().endsWith(".json"))
                         .forEach(path -> {
                             LOGGER.info("  - Adding: " + path);
                             assembler.addImport(path);
                         });
                }
            } else if (Files.isRegularFile(source)) {
                // Add individual file
                LOGGER.info("  - Adding: " + source);
                assembler.addImport(source);
            } else {
                LOGGER.warning("Source not found: " + source);
            }
        }
        
        return assembleModel(assembler);
    }
    
    /**
     * Load from a JSON string (useful for testing or API consumption).
     * 
     * @param jsonContent JSON AST content as string
     * @return Assembled Smithy Model
     */
    public static Model loadFromJsonString(String jsonContent) {
        LOGGER.info("Loading Smithy model from JSON string");
        
        ModelAssembler assembler = Model.assembler();
        assembler.discoverModels(); // Discover built-in traits from classpath
        assembler.addUnparsedModel("inline.json", jsonContent);
        
        return assembleModel(assembler);
    }
    
    /**
     * Assemble the model and handle validation.
     */
    private static Model assembleModel(ModelAssembler assembler) {
        // Assemble the model
        Model model = assembler.assemble().unwrap();
        
        // Log any validation events
        List<ValidationEvent> events = assembler.assemble().getValidationEvents();
        if (!events.isEmpty()) {
            LOGGER.info("Model validation events:");
            for (ValidationEvent event : events) {
                LOGGER.info("  - " + event.getSeverity() + ": " + event.getMessage());
            }
        }
        
        return model;
    }
    
    /**
     * Example usage demonstrating different loading methods.
     */
    public static void main(String[] args) throws IOException {
        // Example 1: Load from single JSON file
        Model model1 = loadFromJsonFile(Paths.get("model/service.json"));
        System.out.println("Loaded model with " + model1.shapes().count() + " shapes");
        
        // Example 2: Load from directory
        Model model2 = loadFromDirectory(Paths.get("models"));
        System.out.println("Loaded model with " + model2.shapes().count() + " shapes");
        
        // Example 3: Load from mixed sources
        List<Path> sources = List.of(
            Paths.get("model/service.smithy"),
            Paths.get("model/types.json"),
            Paths.get("external/aws-models")
        );
        Model model3 = loadFromMixedSources(sources);
        System.out.println("Loaded model with " + model3.shapes().count() + " shapes");
    }
}
