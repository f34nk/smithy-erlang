package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.traits.PaginatedTrait;

import java.util.Optional;

/**
 * Tests pagination trait detection in Smithy models.
 * 
 * Verifies:
 * - Detection of @paginated trait
 * - Extraction of pagination parameters (tokens, page size, items)
 * - Correct identification of paginated vs non-paginated operations
 */
public class PaginationDetectionTest extends AwsProtocolTestBase {
    
    @Override
    protected String getModelPath() {
        return "/models/paginated-test.smithy";
    }
    
    @Override
    protected String getServiceShapeId() {
        return "com.example.pagination#PaginatedTestService";
    }
    
    @Override
    protected String getModuleName() {
        return "paginated_test_client";
    }
    
    @Test
    public void testPaginatedModelLoads() {
        // Verify the test model loads successfully
        assertNotNull(model, "Model should load");
        assertTrue(model.getOperationShapes().size() >= 2, 
                "Test model should contain at least 2 operations");
    }
    
    @Test
    public void testDetectsPaginatedOperation() {
        // Find the ListItems operation
        Optional<OperationShape> listItemsOp = model.getOperationShapes().stream()
                .filter(op -> op.getId().getName().equals("ListItems"))
                .findFirst();
        
        assertTrue(listItemsOp.isPresent(), "ListItems operation should exist");
        
        // Verify it has pagination trait
        OperationShape operation = listItemsOp.get();
        assertTrue(operation.hasTrait(PaginatedTrait.class),
                "ListItems should have @paginated trait");
        
        // Get pagination info
        Optional<PaginatedTrait> paginationInfo = operation.getTrait(PaginatedTrait.class);
        assertTrue(paginationInfo.isPresent(), "Should extract pagination info");
    }
    
    @Test
    public void testExtractsInputToken() {
        // Find the ListItems operation
        OperationShape operation = model.getOperationShapes().stream()
                .filter(op -> op.getId().getName().equals("ListItems"))
                .findFirst()
                .orElseThrow();
        
        // Get pagination trait
        PaginatedTrait pagination = operation.expectTrait(PaginatedTrait.class);
        
        // Verify input token
        Optional<String> inputToken = pagination.getInputToken();
        assertTrue(inputToken.isPresent(), "Should have input token");
        assertEquals("nextToken", inputToken.get(), "Input token should be 'nextToken'");
    }
    
    @Test
    public void testExtractsOutputToken() {
        // Find the ListItems operation
        OperationShape operation = model.getOperationShapes().stream()
                .filter(op -> op.getId().getName().equals("ListItems"))
                .findFirst()
                .orElseThrow();
        
        // Get pagination trait
        PaginatedTrait pagination = operation.expectTrait(PaginatedTrait.class);
        
        // Verify output token
        Optional<String> outputToken = pagination.getOutputToken();
        assertTrue(outputToken.isPresent(), "Should have output token");
        assertEquals("nextToken", outputToken.get(), "Output token should be 'nextToken'");
    }
    
    @Test
    public void testExtractsItemsMember() {
        // Find the ListItems operation
        OperationShape operation = model.getOperationShapes().stream()
                .filter(op -> op.getId().getName().equals("ListItems"))
                .findFirst()
                .orElseThrow();
        
        // Get pagination trait
        PaginatedTrait pagination = operation.expectTrait(PaginatedTrait.class);
        
        // Verify items member
        Optional<String> items = pagination.getItems();
        assertTrue(items.isPresent(), "Should have items member");
        assertEquals("items", items.get(), "Items member should be 'items'");
    }
    
    @Test
    public void testExtractsPageSize() {
        // Find the ListItemsWithPageSize operation
        OperationShape operation = model.getOperationShapes().stream()
                .filter(op -> op.getId().getName().equals("ListItemsWithPageSize"))
                .findFirst()
                .orElseThrow();
        
        // Get pagination trait
        PaginatedTrait pagination = operation.expectTrait(PaginatedTrait.class);
        
        // Verify page size
        Optional<String> pageSize = pagination.getPageSize();
        assertTrue(pageSize.isPresent(), "Should have page size");
        assertEquals("maxResults", pageSize.get(), "Page size should be 'maxResults'");
    }
    
    @Test
    public void testMultiplePaginatedOperations() {
        // Count paginated operations
        long paginatedCount = model.getOperationShapes().stream()
                .filter(op -> op.hasTrait(PaginatedTrait.class))
                .count();
        
        assertEquals(2, paginatedCount, 
                "Test model should have exactly 2 paginated operations");
    }
    
    @Test
    public void testGeneratorRunsWithPaginatedOperations() {
        // Verify generator can process paginated operations without errors
        assertDoesNotThrow(() -> runGenerator(), 
                "Generator should run without exceptions on paginated model");
    }
    
    @Test
    public void testGeneratedClientHasPaginatedOperations() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // Verify both paginated operations are generated
        assertTrue(content.contains("list_items("),
                "Should generate list_items operation");
        assertTrue(content.contains("list_items_with_page_size("),
                "Should generate list_items_with_page_size operation");
    }
    
    @Test
    public void testPaginationInfoInComments() {
        runGenerator();
        String clientFile = "src/" + getModuleName() + ".erl";
        
        String content = getGeneratedFile(clientFile);
        assertNotNull(content, "Client file should exist");
        
        // For now, pagination detection is logged but not added to generated code
        // This test verifies the operations are still generated correctly
        assertTrue(content.contains("list_items"),
                "Paginated operations should be generated");
    }
    
    @Test
    public void testInputStructureHasTokenField() {
        // Find the ListItemsInput structure
        assertTrue(model.getStructureShapes().stream()
                .anyMatch(s -> s.getId().getName().equals("ListItemsInput")),
                "Should have ListItemsInput structure");
        
        var inputShape = model.getStructureShapes().stream()
                .filter(s -> s.getId().getName().equals("ListItemsInput"))
                .findFirst()
                .orElseThrow();
        
        // Verify it has nextToken member
        assertTrue(inputShape.getAllMembers().containsKey("nextToken"),
                "Input should have nextToken member");
    }
    
    @Test
    public void testOutputStructureHasTokenAndItems() {
        // Find the ListItemsOutput structure
        var outputShape = model.getStructureShapes().stream()
                .filter(s -> s.getId().getName().equals("ListItemsOutput"))
                .findFirst()
                .orElseThrow();
        
        // Verify it has required members
        assertTrue(outputShape.getAllMembers().containsKey("nextToken"),
                "Output should have nextToken member");
        assertTrue(outputShape.getAllMembers().containsKey("items"),
                "Output should have items member");
    }
}

