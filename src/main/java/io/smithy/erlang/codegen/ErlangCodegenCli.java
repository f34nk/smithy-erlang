package io.smithy.erlang.codegen;

import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Command-line interface for generating Erlang code from Smithy JSON AST files.
 * 
 * Usage:
 *   java -jar smithy-erlang.jar \
 *     --input model.json \
 *     --service com.example#MyService \
 *     --module my_client \
 *     --output build/erlang
 */
public class ErlangCodegenCli {
    
    public static void main(String[] args) {
        try {
            CliConfig config = parseArgs(args);
            generateCode(config);
            System.out.println("Code generation completed successfully!");
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    /**
     * Generate Erlang code from configuration.
     */
    private static void generateCode(CliConfig config) throws IOException {
        System.out.println("Loading Smithy model from: " + config.input);
        
        // Load model from JSON AST file(s)
        Model model;
        if (config.input.toFile().isDirectory()) {
            model = JsonAstLoader.loadFromDirectory(config.input);
        } else if (config.input.toString().endsWith(".json")) {
            model = JsonAstLoader.loadFromJsonFile(config.input);
        } else {
            // Try mixed sources
            model = JsonAstLoader.loadFromMixedSources(List.of(config.input));
        }
        
        System.out.println("Model loaded: " + model.shapes().count() + " shapes");
        
        // Get the service shape
        ShapeId serviceId = ShapeId.from(config.service);
        ServiceShape service = model.expectShape(serviceId, ServiceShape.class);
        System.out.println("Found service: " + service.getId().getName());
        
        // Create settings
        Map<String, Object> settingsMap = new HashMap<>();
        settingsMap.put("service", config.service);
        settingsMap.put("module", config.module);
        if (config.protocol != null) {
            settingsMap.put("protocol", config.protocol);
        }
        settingsMap.put("edition", "2025");
        
        // Create output directory (use absolute path to avoid path duplication)
        Path absoluteOutput = config.output.toAbsolutePath().normalize();
        Path outputDir = absoluteOutput;
        outputDir.toFile().mkdirs();
        
        // Create file manifest with absolute path
        FileManifest fileManifest = FileManifest.create(absoluteOutput);
        
        System.out.println("Generating code to: " + absoluteOutput);
        
        // Run the plugin manually
        ErlangCodegenPlugin plugin = new ErlangCodegenPlugin();
        
        // Create a minimal plugin context
        software.amazon.smithy.build.PluginContext context = 
            software.amazon.smithy.build.PluginContext.builder()
                .model(model)
                .fileManifest(fileManifest)
                .settings(software.amazon.smithy.model.node.Node.objectNode()
                    .withMember("service", software.amazon.smithy.model.node.Node.from(config.service))
                    .withMember("module", software.amazon.smithy.model.node.Node.from(config.module)))
                .build();
        
        plugin.execute(context);
        
        System.out.println("Generated files:");
        System.out.println("  - " + outputDir.resolve(config.module + ".erl"));
    }
    
    /**
     * Parse command-line arguments.
     */
    private static CliConfig parseArgs(String[] args) {
        CliConfig config = new CliConfig();
        
        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            
            if (arg.equals("--help") || arg.equals("-h")) {
                printUsage();
                System.exit(0);
            }
            
            if (i + 1 >= args.length) {
                throw new IllegalArgumentException("Missing value for argument: " + arg);
            }
            
            String value = args[++i];
            
            switch (arg) {
                case "--input":
                case "-i":
                    config.input = Paths.get(value);
                    break;
                case "--service":
                case "-s":
                    config.service = value;
                    break;
                case "--module":
                case "-m":
                    config.module = value;
                    break;
                case "--output":
                case "-o":
                    config.output = Paths.get(value);
                    break;
                case "--protocol":
                case "-p":
                    config.protocol = value;
                    break;
                default:
                    throw new IllegalArgumentException("Unknown argument: " + arg);
            }
        }
        
        // Validate required arguments
        if (config.input == null) {
            throw new IllegalArgumentException("Missing required argument: --input");
        }
        if (config.service == null) {
            throw new IllegalArgumentException("Missing required argument: --service");
        }
        if (config.module == null) {
            throw new IllegalArgumentException("Missing required argument: --module");
        }
        if (config.output == null) {
            config.output = Paths.get("build/erlang");
        }
        
        return config;
    }
    
    /**
     * Print usage information.
     */
    private static void printUsage() {
        System.out.println("Smithy Erlang Code Generator");
        System.out.println();
        System.out.println("Usage:");
        System.out.println("  java -jar smithy-erlang.jar [options]");
        System.out.println();
        System.out.println("Options:");
        System.out.println("  -i, --input <path>      Input JSON AST file or directory (required)");
        System.out.println("  -s, --service <id>      Service shape ID (e.g., com.example#MyService) (required)");
        System.out.println("  -m, --module <name>     Erlang module name (required)");
        System.out.println("  -o, --output <path>     Output directory (default: build/erlang)");
        System.out.println("  -p, --protocol <proto>  Protocol to use (optional, auto-detected)");
        System.out.println("  -h, --help              Show this help message");
        System.out.println();
        System.out.println("Examples:");
        System.out.println("  # Generate from single JSON AST file");
        System.out.println("  java -jar smithy-erlang.jar \\");
        System.out.println("    --input model/weather.json \\");
        System.out.println("    --service example.weather#WeatherService \\");
        System.out.println("    --module weather_client \\");
        System.out.println("    --output generated");
        System.out.println();
        System.out.println("  # Generate from directory of JSON files");
        System.out.println("  java -jar smithy-erlang.jar \\");
        System.out.println("    --input models/aws-sdk \\");
        System.out.println("    --service com.amazonaws.s3#AmazonS3 \\");
        System.out.println("    --module s3_client");
    }
    
    /**
     * Configuration container.
     */
    private static class CliConfig {
        Path input;
        String service;
        String module;
        Path output;
        String protocol;
    }
}
