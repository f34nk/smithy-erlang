package io.smithy.erlang.codegen;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses URI templates to extract path parameter labels.
 * 
 * Supports standard labels like {userId} and greedy labels like {key+}.
 * 
 * Examples:
 * - /users/{userId} -> ["userId"]
 * - /files/{key+} -> ["key"]
 * - /resources/{id}/items/{itemId} -> ["id", "itemId"]
 */
public final class UriTemplate {
    private static final Pattern LABEL_PATTERN = Pattern.compile("\\{([^}]+)\\}");
    
    private final String template;
    private final List<String> labels;
    private final List<Boolean> greedyFlags;
    
    /**
     * Create a new URI template parser.
     * 
     * @param template The URI template string (e.g., "/users/{userId}")
     */
    public UriTemplate(String template) {
        if (template == null) {
            throw new IllegalArgumentException("URI template cannot be null");
        }
        
        this.template = template;
        this.labels = new ArrayList<>();
        this.greedyFlags = new ArrayList<>();
        
        extractLabels();
    }
    
    /**
     * Extract labels from the URI template.
     */
    private void extractLabels() {
        Matcher matcher = LABEL_PATTERN.matcher(template);
        
        while (matcher.find()) {
            String labelWithModifiers = matcher.group(1);
            
            // Check if this is a greedy label (ends with +)
            boolean isGreedy = labelWithModifiers.endsWith("+");
            String label = isGreedy ? labelWithModifiers.substring(0, labelWithModifiers.length() - 1) : labelWithModifiers;
            
            labels.add(label);
            greedyFlags.add(isGreedy);
        }
    }
    
    /**
     * Get the original URI template string.
     * 
     * @return The template string
     */
    public String getTemplate() {
        return template;
    }
    
    /**
     * Get the list of labels extracted from the template.
     * 
     * @return An immutable list of label names (without {} or + modifiers)
     */
    public List<String> getLabels() {
        return Collections.unmodifiableList(labels);
    }
    
    /**
     * Check if a label at the given index is greedy.
     * 
     * @param index The index of the label
     * @return true if the label is greedy (has + modifier)
     * @throws IndexOutOfBoundsException if index is out of range
     */
    public boolean isGreedy(int index) {
        return greedyFlags.get(index);
    }
    
    /**
     * Check if a label with the given name is greedy.
     * 
     * @param labelName The name of the label
     * @return true if the label is greedy, false if not found or not greedy
     */
    public boolean isGreedy(String labelName) {
        int index = labels.indexOf(labelName);
        if (index == -1) {
            return false;
        }
        return greedyFlags.get(index);
    }
    
    /**
     * Check if the template contains any labels.
     * 
     * @return true if there are labels in the template
     */
    public boolean hasLabels() {
        return !labels.isEmpty();
    }
    
    /**
     * Get the number of labels in the template.
     * 
     * @return The number of labels
     */
    public int getLabelCount() {
        return labels.size();
    }
    
    /**
     * Get the placeholder string for a label at the given index.
     * Includes the braces and any modifiers.
     * 
     * @param index The index of the label
     * @return The placeholder string (e.g., "{userId}" or "{key+}")
     * @throws IndexOutOfBoundsException if index is out of range
     */
    public String getPlaceholder(int index) {
        String label = labels.get(index);
        boolean greedy = greedyFlags.get(index);
        return greedy ? "{" + label + "+}" : "{" + label + "}";
    }
    
    /**
     * Get the placeholder string for a label with the given name.
     * 
     * @param labelName The name of the label
     * @return The placeholder string, or null if label not found
     */
    public String getPlaceholder(String labelName) {
        int index = labels.indexOf(labelName);
        if (index == -1) {
            return null;
        }
        return getPlaceholder(index);
    }
    
    @Override
    public String toString() {
        return "UriTemplate{template='" + template + "', labels=" + labels + "}";
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        UriTemplate other = (UriTemplate) obj;
        return template.equals(other.template);
    }
    
    @Override
    public int hashCode() {
        return template.hashCode();
    }
}
