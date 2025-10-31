package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for UriTemplate path parameter extraction.
 */
public class UriTemplateTest {
    
    @Test
    public void testSimpleSingleLabel() {
        UriTemplate template = new UriTemplate("/users/{userId}");
        
        assertEquals("/users/{userId}", template.getTemplate());
        assertEquals(1, template.getLabelCount());
        assertTrue(template.hasLabels());
        assertEquals("userId", template.getLabels().get(0));
        assertFalse(template.isGreedy(0));
        assertFalse(template.isGreedy("userId"));
        assertEquals("{userId}", template.getPlaceholder(0));
        assertEquals("{userId}", template.getPlaceholder("userId"));
    }
    
    @Test
    public void testMultipleLabels() {
        UriTemplate template = new UriTemplate("/resources/{id}/items/{itemId}");
        
        assertEquals(2, template.getLabelCount());
        assertEquals("id", template.getLabels().get(0));
        assertEquals("itemId", template.getLabels().get(1));
        assertFalse(template.isGreedy("id"));
        assertFalse(template.isGreedy("itemId"));
    }
    
    @Test
    public void testGreedyLabel() {
        UriTemplate template = new UriTemplate("/files/{key+}");
        
        assertEquals(1, template.getLabelCount());
        assertEquals("key", template.getLabels().get(0));
        assertTrue(template.isGreedy(0));
        assertTrue(template.isGreedy("key"));
        assertEquals("{key+}", template.getPlaceholder(0));
        assertEquals("{key+}", template.getPlaceholder("key"));
    }
    
    @Test
    public void testMixedLabels() {
        UriTemplate template = new UriTemplate("/bucket/{bucket}/files/{key+}");
        
        assertEquals(2, template.getLabelCount());
        assertEquals("bucket", template.getLabels().get(0));
        assertEquals("key", template.getLabels().get(1));
        assertFalse(template.isGreedy("bucket"));
        assertTrue(template.isGreedy("key"));
    }
    
    @Test
    public void testNoLabels() {
        UriTemplate template = new UriTemplate("/static/path");
        
        assertEquals(0, template.getLabelCount());
        assertFalse(template.hasLabels());
        assertTrue(template.getLabels().isEmpty());
    }
    
    @Test
    public void testRootPath() {
        UriTemplate template = new UriTemplate("/");
        
        assertEquals(0, template.getLabelCount());
        assertFalse(template.hasLabels());
    }
    
    @Test
    public void testComplexS3StylePath() {
        UriTemplate template = new UriTemplate("/{Bucket}/{Key}");
        
        assertEquals(2, template.getLabelCount());
        assertEquals("Bucket", template.getLabels().get(0));
        assertEquals("Key", template.getLabels().get(1));
    }
    
    @Test
    public void testPathWithQueryString() {
        // Query string should not be parsed as labels
        UriTemplate template = new UriTemplate("/users/{userId}?format=json");
        
        assertEquals(1, template.getLabelCount());
        assertEquals("userId", template.getLabels().get(0));
    }
    
    @Test
    public void testConsecutiveLabels() {
        UriTemplate template = new UriTemplate("/{first}/{second}/{third}");
        
        assertEquals(3, template.getLabelCount());
        assertEquals("first", template.getLabels().get(0));
        assertEquals("second", template.getLabels().get(1));
        assertEquals("third", template.getLabels().get(2));
    }
    
    @Test
    public void testLabelWithDashes() {
        UriTemplate template = new UriTemplate("/resource/{user-id}");
        
        assertEquals(1, template.getLabelCount());
        assertEquals("user-id", template.getLabels().get(0));
    }
    
    @Test
    public void testNullTemplate() {
        assertThrows(IllegalArgumentException.class, () -> {
            new UriTemplate(null);
        });
    }
    
    @Test
    public void testEmptyTemplate() {
        UriTemplate template = new UriTemplate("");
        
        assertEquals(0, template.getLabelCount());
        assertFalse(template.hasLabels());
    }
    
    @Test
    public void testIsGreedyForNonExistentLabel() {
        UriTemplate template = new UriTemplate("/users/{userId}");
        
        assertFalse(template.isGreedy("nonExistent"));
    }
    
    @Test
    public void testGetPlaceholderForNonExistentLabel() {
        UriTemplate template = new UriTemplate("/users/{userId}");
        
        assertNull(template.getPlaceholder("nonExistent"));
    }
    
    @Test
    public void testGetPlaceholderOutOfBounds() {
        UriTemplate template = new UriTemplate("/users/{userId}");
        
        assertThrows(IndexOutOfBoundsException.class, () -> {
            template.getPlaceholder(1);
        });
    }
    
    @Test
    public void testIsGreedyOutOfBounds() {
        UriTemplate template = new UriTemplate("/users/{userId}");
        
        assertThrows(IndexOutOfBoundsException.class, () -> {
            template.isGreedy(1);
        });
    }
    
    @Test
    public void testEquality() {
        UriTemplate template1 = new UriTemplate("/users/{userId}");
        UriTemplate template2 = new UriTemplate("/users/{userId}");
        UriTemplate template3 = new UriTemplate("/users/{id}");
        
        assertEquals(template1, template2);
        assertNotEquals(template1, template3);
        assertEquals(template1.hashCode(), template2.hashCode());
    }
    
    @Test
    public void testToString() {
        UriTemplate template = new UriTemplate("/users/{userId}");
        String str = template.toString();
        
        assertTrue(str.contains("/users/{userId}"));
        assertTrue(str.contains("userId"));
    }
    
    @Test
    public void testLabelsListIsImmutable() {
        UriTemplate template = new UriTemplate("/users/{userId}");
        
        assertThrows(UnsupportedOperationException.class, () -> {
            template.getLabels().add("newLabel");
        });
    }
    
    @Test
    public void testRealWorldS3GetObject() {
        UriTemplate template = new UriTemplate("/{Bucket}/{Key+}");
        
        assertEquals(2, template.getLabelCount());
        assertEquals("Bucket", template.getLabels().get(0));
        assertEquals("Key", template.getLabels().get(1));
        assertFalse(template.isGreedy("Bucket"));
        assertTrue(template.isGreedy("Key"));
    }
    
    @Test
    public void testRealWorldAPIGateway() {
        UriTemplate template = new UriTemplate("/2015-03-31/functions/{FunctionName}/invocations");
        
        assertEquals(1, template.getLabelCount());
        assertEquals("FunctionName", template.getLabels().get(0));
        assertFalse(template.isGreedy("FunctionName"));
    }
    
    @Test
    public void testRealWorldDynamoDB() {
        UriTemplate template = new UriTemplate("/");
        
        assertEquals(0, template.getLabelCount());
        assertFalse(template.hasLabels());
    }
}
