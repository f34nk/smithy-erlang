package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for loading Smithy models from JSON AST files.
 */
public class JsonAstLoaderTest {
    
    /**
     * Sample minimal JSON AST for testing.
     */
    private static final String MINIMAL_JSON_AST = "{\n" +
        "    \"smithy\": \"2.0\",\n" +
        "    \"shapes\": {\n" +
        "        \"example.test#TestService\": {\n" +
        "            \"type\": \"service\",\n" +
        "            \"version\": \"2024-01-01\",\n" +
        "            \"operations\": [\n" +
        "                {\"target\": \"example.test#GetData\"}\n" +
        "            ]\n" +
        "        },\n" +
        "        \"example.test#GetData\": {\n" +
        "            \"type\": \"operation\",\n" +
        "            \"input\": {\"target\": \"example.test#GetDataInput\"},\n" +
        "            \"output\": {\"target\": \"example.test#GetDataOutput\"}\n" +
        "        },\n" +
        "        \"example.test#GetDataInput\": {\n" +
        "            \"type\": \"structure\",\n" +
        "            \"members\": {\n" +
        "                \"id\": {\n" +
        "                    \"target\": \"smithy.api#String\",\n" +
        "                    \"traits\": {\n" +
        "                        \"smithy.api#required\": {}\n" +
        "                    }\n" +
        "                }\n" +
        "            }\n" +
        "        },\n" +
        "        \"example.test#GetDataOutput\": {\n" +
        "            \"type\": \"structure\",\n" +
        "            \"members\": {\n" +
        "                \"data\": {\n" +
        "                    \"target\": \"smithy.api#String\",\n" +
        "                    \"traits\": {\n" +
        "                        \"smithy.api#required\": {}\n" +
        "                    }\n" +
        "                }\n" +
        "            }\n" +
        "        }\n" +
        "    }\n" +
        "}";
    
    @Test
    public void testLoadFromJsonString() {
        // Load model from JSON string
        Model model = JsonAstLoader.loadFromJsonString(MINIMAL_JSON_AST);
        
        // Verify model is loaded
        assertNotNull(model);
        assertTrue(model.shapes().count() > 0);
        
        // Verify service exists
        ShapeId serviceId = ShapeId.from("example.test#TestService");
        assertTrue(model.getShape(serviceId).isPresent());
        
        ServiceShape service = model.expectShape(serviceId, ServiceShape.class);
        assertEquals("2024-01-01", service.getVersion());
        assertEquals(1, service.getOperations().size());
    }
    
    @Test
    public void testLoadFromJsonFile(@TempDir Path tempDir) throws IOException {
        // Create a temporary JSON AST file
        Path jsonFile = tempDir.resolve("model.json");
        Files.writeString(jsonFile, MINIMAL_JSON_AST);
        
        // Load model from file
        Model model = JsonAstLoader.loadFromJsonFile(jsonFile);
        
        // Verify model is loaded
        assertNotNull(model);
        ShapeId serviceId = ShapeId.from("example.test#TestService");
        assertTrue(model.getShape(serviceId).isPresent());
    }
    
    @Test
    public void testLoadFromDirectory(@TempDir Path tempDir) throws IOException {
        // Create multiple JSON AST files
        String service1Json = "{\n" +
            "    \"smithy\": \"2.0\",\n" +
            "    \"shapes\": {\n" +
            "        \"example.service1#Service1\": {\n" +
            "            \"type\": \"service\",\n" +
            "            \"version\": \"1.0\"\n" +
            "        }\n" +
            "    }\n" +
            "}";
        
        String service2Json = "{\n" +
            "    \"smithy\": \"2.0\",\n" +
            "    \"shapes\": {\n" +
            "        \"example.service2#Service2\": {\n" +
            "            \"type\": \"service\",\n" +
            "            \"version\": \"2.0\"\n" +
            "        }\n" +
            "    }\n" +
            "}";
        
        Files.writeString(tempDir.resolve("service1.json"), service1Json);
        Files.writeString(tempDir.resolve("service2.json"), service2Json);
        
        // Load all models from directory
        Model model = JsonAstLoader.loadFromDirectory(tempDir);
        
        // Verify both services are loaded
        assertNotNull(model);
        assertTrue(model.getShape(ShapeId.from("example.service1#Service1")).isPresent());
        assertTrue(model.getShape(ShapeId.from("example.service2#Service2")).isPresent());
    }
    
    @Test
    public void testLoadFromMixedSources(@TempDir Path tempDir) throws IOException {
        // Create a JSON AST file
        Path jsonFile = tempDir.resolve("json-model.json");
        Files.writeString(jsonFile, "{\n" +
            "    \"smithy\": \"2.0\",\n" +
            "    \"shapes\": {\n" +
            "        \"example.json#JsonService\": {\n" +
            "            \"type\": \"service\",\n" +
            "            \"version\": \"1.0\"\n" +
            "        }\n" +
            "    }\n" +
            "}");
        
        // Create a Smithy IDL file
        Path smithyFile = tempDir.resolve("smithy-model.smithy");
        Files.writeString(smithyFile, "$version: \"2.0\"\n" +
            "\n" +
            "namespace example.smithy\n" +
            "\n" +
            "service SmithyService {\n" +
            "    version: \"2.0\"\n" +
            "}");
        
        // Load from both sources
        Model model = JsonAstLoader.loadFromMixedSources(List.of(jsonFile, smithyFile));
        
        // Verify both services are loaded
        assertNotNull(model);
        assertTrue(model.getShape(ShapeId.from("example.json#JsonService")).isPresent());
        assertTrue(model.getShape(ShapeId.from("example.smithy#SmithyService")).isPresent());
    }
    
    @Test
    public void testLoadFromMultipleJsonFiles(@TempDir Path tempDir) throws IOException {
        // Create service definition
        Path serviceFile = tempDir.resolve("service.json");
        Files.writeString(serviceFile, "{\n" +
            "    \"smithy\": \"2.0\",\n" +
            "    \"shapes\": {\n" +
            "        \"example.multi#MultiService\": {\n" +
            "            \"type\": \"service\",\n" +
            "            \"version\": \"1.0\",\n" +
            "            \"operations\": [\n" +
            "                {\"target\": \"example.multi#GetItem\"}\n" +
            "            ]\n" +
            "        }\n" +
            "    }\n" +
            "}");
        
        // Create operations definition
        Path opsFile = tempDir.resolve("operations.json");
        Files.writeString(opsFile, "{\n" +
            "    \"smithy\": \"2.0\",\n" +
            "    \"shapes\": {\n" +
            "        \"example.multi#GetItem\": {\n" +
            "            \"type\": \"operation\",\n" +
            "            \"input\": {\"target\": \"example.multi#GetItemInput\"},\n" +
            "            \"output\": {\"target\": \"example.multi#GetItemOutput\"}\n" +
            "        }\n" +
            "    }\n" +
            "}");
        
        // Create types definition
        Path typesFile = tempDir.resolve("types.json");
        Files.writeString(typesFile, "{\n" +
            "    \"smithy\": \"2.0\",\n" +
            "    \"shapes\": {\n" +
            "        \"example.multi#GetItemInput\": {\n" +
            "            \"type\": \"structure\",\n" +
            "            \"members\": {\n" +
            "                \"id\": {\"target\": \"smithy.api#String\"}\n" +
            "            }\n" +
            "        },\n" +
            "        \"example.multi#GetItemOutput\": {\n" +
            "            \"type\": \"structure\",\n" +
            "            \"members\": {\n" +
            "                \"data\": {\"target\": \"smithy.api#String\"}\n" +
            "            }\n" +
            "        }\n" +
            "    }\n" +
            "}");
        
        // Load from all files
        List<Path> files = List.of(serviceFile, opsFile, typesFile);
        Model model = JsonAstLoader.loadFromJsonFiles(files);
        
        // Verify complete model is assembled
        assertNotNull(model);
        ServiceShape service = model.expectShape(
            ShapeId.from("example.multi#MultiService"), 
            ServiceShape.class
        );
        
        assertEquals(1, service.getOperations().size());
        assertTrue(model.getShape(ShapeId.from("example.multi#GetItem")).isPresent());
        assertTrue(model.getShape(ShapeId.from("example.multi#GetItemInput")).isPresent());
        assertTrue(model.getShape(ShapeId.from("example.multi#GetItemOutput")).isPresent());
    }
    
    @Test
    public void testLoadRealWeatherModel() {
        // Try to load the actual weather service model if it exists
        Path modelPath = Path.of("examples/weather-service/build/smithy/source/model/model.json");
        
        if (!Files.exists(modelPath)) {
            // Skip test if model doesn't exist yet
            return;
        }
        
        // Load the real model
        Model model = JsonAstLoader.loadFromJsonFile(modelPath);
        
        // Verify it contains the weather service
        assertNotNull(model);
        ShapeId serviceId = ShapeId.from("example.weather#WeatherService");
        assertTrue(model.getShape(serviceId).isPresent());
        
        ServiceShape service = model.expectShape(serviceId, ServiceShape.class);
        assertEquals("2024-01-01", service.getVersion());
        
        // Verify it has operations
        assertFalse(service.getOperations().isEmpty());
        System.out.println("Loaded weather service with " + 
                         service.getOperations().size() + " operations");
    }
}
