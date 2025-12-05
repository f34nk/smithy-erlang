package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.node.ObjectNode;
import software.amazon.smithy.model.shapes.ShapeId;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for ErlangSettings.
 */
class ErlangSettingsTest {
    
    private static final ShapeId TEST_SERVICE = ShapeId.from("com.example#TestService");
    
    // ========== Builder Tests ==========
    
    @Test
    @DisplayName("Builder creates settings with all fields")
    void testBuilderWithAllFields() {
        ErlangSettings settings = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName("test_service")
                .outputDir("output/generated")
                .protocol("aws.protocols#restJson1")
                .edition("2024")
                .build();
        
        assertEquals(TEST_SERVICE, settings.service());
        assertEquals("test_service", settings.moduleName());
        assertEquals("output/generated", settings.outputDir());
        assertEquals("aws.protocols#restJson1", settings.protocol());
        assertEquals("2024", settings.edition());
    }
    
    @Test
    @DisplayName("Builder uses default values for optional fields")
    void testBuilderDefaults() {
        ErlangSettings settings = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .build();
        
        assertEquals(TEST_SERVICE, settings.service());
        assertNull(settings.moduleName());
        assertEquals("src/generated", settings.outputDir());
        assertNull(settings.protocol());
        assertEquals("2025", settings.edition());
    }
    
    @Test
    @DisplayName("Builder throws NullPointerException when service is missing")
    void testBuilderRequiresService() {
        assertThrows(NullPointerException.class, () -> {
            ErlangSettings.builder().build();
        });
    }
    
    @Test
    @DisplayName("Builder allows null module name")
    void testBuilderAllowsNullModuleName() {
        ErlangSettings settings = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName(null)
                .build();
        
        assertNull(settings.moduleName());
    }
    
    // ========== from(ObjectNode) Tests ==========
    
    @Test
    @DisplayName("from() parses complete configuration")
    void testFromCompleteConfig() {
        ObjectNode node = Node.objectNodeBuilder()
                .withMember("service", "com.example#TestService")
                .withMember("module", "test_module")
                .withMember("outputDir", "custom/output")
                .withMember("protocol", "aws.protocols#awsJson1_1")
                .withMember("edition", "2023")
                .build();
        
        ErlangSettings settings = ErlangSettings.from(node);
        
        assertEquals(ShapeId.from("com.example#TestService"), settings.service());
        assertEquals("test_module", settings.moduleName());
        assertEquals("custom/output", settings.outputDir());
        assertEquals("aws.protocols#awsJson1_1", settings.protocol());
        assertEquals("2023", settings.edition());
    }
    
    @Test
    @DisplayName("from() uses defaults for missing optional fields")
    void testFromMinimalConfig() {
        ObjectNode node = Node.objectNodeBuilder()
                .withMember("service", "com.example#MinimalService")
                .build();
        
        ErlangSettings settings = ErlangSettings.from(node);
        
        assertEquals(ShapeId.from("com.example#MinimalService"), settings.service());
        assertNull(settings.moduleName());
        assertEquals("src/generated", settings.outputDir());
        assertNull(settings.protocol());
        assertEquals("2025", settings.edition());
    }
    
    @Test
    @DisplayName("from() throws when service is missing")
    void testFromMissingService() {
        ObjectNode node = Node.objectNodeBuilder()
                .withMember("module", "test_module")
                .build();
        
        assertThrows(NullPointerException.class, () -> {
            ErlangSettings.from(node);
        });
    }
    
    @Test
    @DisplayName("from() handles empty ObjectNode")
    void testFromEmptyNode() {
        ObjectNode node = Node.objectNode();
        
        assertThrows(NullPointerException.class, () -> {
            ErlangSettings.from(node);
        });
    }
    
    // ========== Accessor Tests ==========
    
    @Test
    @DisplayName("service() returns the service ShapeId")
    void testServiceAccessor() {
        ErlangSettings settings = ErlangSettings.builder()
                .service(ShapeId.from("com.example.api#MyService"))
                .build();
        
        assertEquals(ShapeId.from("com.example.api#MyService"), settings.service());
    }
    
    @Test
    @DisplayName("getProtocol() returns Optional")
    void testGetProtocolOptional() {
        ErlangSettings withProtocol = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .protocol("aws.protocols#restXml")
                .build();
        
        ErlangSettings withoutProtocol = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .build();
        
        assertEquals(Optional.of("aws.protocols#restXml"), withProtocol.getProtocol());
        assertEquals(Optional.empty(), withoutProtocol.getProtocol());
    }
    
    // ========== toBuilder() Tests ==========
    
    @Test
    @DisplayName("toBuilder() creates builder with same values")
    void testToBuilder() {
        ErlangSettings original = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName("original_module")
                .outputDir("original/dir")
                .protocol("aws.protocols#ec2Query")
                .edition("2022")
                .build();
        
        ErlangSettings copy = original.toBuilder().build();
        
        assertEquals(original, copy);
    }
    
    @Test
    @DisplayName("toBuilder() allows modification")
    void testToBuilderModification() {
        ErlangSettings original = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName("original_module")
                .build();
        
        ErlangSettings modified = original.toBuilder()
                .moduleName("modified_module")
                .outputDir("new/path")
                .build();
        
        assertEquals(TEST_SERVICE, modified.service());
        assertEquals("modified_module", modified.moduleName());
        assertEquals("new/path", modified.outputDir());
    }
    
    // ========== equals() and hashCode() Tests ==========
    
    @Test
    @DisplayName("equals() returns true for identical settings")
    void testEqualsIdentical() {
        ErlangSettings settings1 = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName("module")
                .outputDir("dir")
                .protocol("proto")
                .edition("2025")
                .build();
        
        ErlangSettings settings2 = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName("module")
                .outputDir("dir")
                .protocol("proto")
                .edition("2025")
                .build();
        
        assertEquals(settings1, settings2);
        assertEquals(settings1.hashCode(), settings2.hashCode());
    }
    
    @Test
    @DisplayName("equals() returns false for different settings")
    void testEqualsDifferent() {
        ErlangSettings settings1 = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName("module1")
                .build();
        
        ErlangSettings settings2 = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName("module2")
                .build();
        
        assertNotEquals(settings1, settings2);
    }
    
    @Test
    @DisplayName("equals() returns false for null")
    void testEqualsNull() {
        ErlangSettings settings = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .build();
        
        assertNotEquals(null, settings);
    }
    
    @Test
    @DisplayName("equals() returns false for different types")
    void testEqualsDifferentType() {
        ErlangSettings settings = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .build();
        
        assertNotEquals("not a settings", settings);
    }
    
    // ========== toString() Tests ==========
    
    @Test
    @DisplayName("toString() includes all fields")
    void testToString() {
        ErlangSettings settings = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName("test_mod")
                .outputDir("out")
                .protocol("proto")
                .edition("2025")
                .build();
        
        String str = settings.toString();
        
        assertTrue(str.contains("ErlangSettings"));
        assertTrue(str.contains("com.example#TestService"));
        assertTrue(str.contains("test_mod"));
        assertTrue(str.contains("out"));
        assertTrue(str.contains("proto"));
        assertTrue(str.contains("2025"));
    }
    
    // ========== Edge Cases ==========
    
    @Test
    @DisplayName("Handles service with complex namespace")
    void testComplexNamespace() {
        ShapeId complexService = ShapeId.from("com.example.deeply.nested.api#ComplexService");
        
        ErlangSettings settings = ErlangSettings.builder()
                .service(complexService)
                .build();
        
        assertEquals(complexService, settings.service());
    }
    
    @Test
    @DisplayName("Handles empty string values")
    void testEmptyStrings() {
        ErlangSettings settings = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName("")
                .outputDir("")
                .protocol("")
                .edition("")
                .build();
        
        assertEquals("", settings.moduleName());
        assertEquals("", settings.outputDir());
        assertEquals("", settings.protocol());
        assertEquals("", settings.edition());
    }
    
    @Test
    @DisplayName("Handles special characters in module name")
    void testSpecialCharacters() {
        ErlangSettings settings = ErlangSettings.builder()
                .service(TEST_SERVICE)
                .moduleName("my_module_v2")
                .outputDir("path/with/slashes")
                .build();
        
        assertEquals("my_module_v2", settings.moduleName());
        assertEquals("path/with/slashes", settings.outputDir());
    }
}
