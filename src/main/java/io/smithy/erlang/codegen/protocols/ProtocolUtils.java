package io.smithy.erlang.codegen.protocols;

import io.smithy.model.Model;
import io.smithy.model.shapes.MemberShape;
import io.smithy.model.shapes.OperationShape;
import io.smithy.model.shapes.ServiceShape;
import io.smithy.model.shapes.Shape;
import io.smithy.model.shapes.StructureShape;
import io.smithy.model.traits.HttpTrait;
import io.smithy.model.traits.HttpHeaderTrait;
import io.smithy.model.traits.HttpLabelTrait;
import io.smithy.model.traits.HttpPayloadTrait;
import io.smithy.model.traits.HttpQueryTrait;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Utility class providing common functionality for protocol implementations.
 */
public final class ProtocolUtils {
    
    private ProtocolUtils() {
        // Utility class - prevent instantiation
    }
    
    /**
     * Gets the HTTP method for an operation.
     * 
     * @param operation The operation shape
     * @param defaultMethod Default method if not specified in HTTP trait
     * @return HTTP method (GET, POST, PUT, DELETE, etc.)
     */
    public static String getHttpMethod(OperationShape operation, String defaultMethod) {
        return operation.getTrait(HttpTrait.class)
            .map(HttpTrait::getMethod)
            .orElse(defaultMethod);
    }
    
    /**
     * Gets the HTTP URI pattern for an operation.
     * 
     * @param operation The operation shape
     * @param defaultUri Default URI if not specified in HTTP trait
     * @return URI pattern (may contain {label} placeholders)
     */
    public static String getHttpUri(OperationShape operation, String defaultUri) {
        return operation.getTrait(HttpTrait.class)
            .map(HttpTrait::getUri)
            .map(uri -> uri.toString())
            .orElse(defaultUri);
    }
    
    /**
     * Gets the input shape for an operation.
     * 
     * @param operation The operation shape
     * @param model The Smithy model
     * @return Optional containing the input structure shape
     */
    public static Optional<StructureShape> getInputShape(OperationShape operation, Model model) {
        return operation.getInput()
            .flatMap(shapeId -> model.getShape(shapeId)
                .flatMap(Shape::asStructureShape));
    }
    
    /**
     * Gets the output shape for an operation.
     * 
     * @param operation The operation shape
     * @param model The Smithy model
     * @return Optional containing the output structure shape
     */
    public static Optional<StructureShape> getOutputShape(OperationShape operation, Model model) {
        return operation.getOutput()
            .flatMap(shapeId -> model.getShape(shapeId)
                .flatMap(Shape::asStructureShape));
    }
    
    /**
     * Gets all members bound to HTTP headers.
     * 
     * @param structure The structure shape
     * @return List of members with @httpHeader trait
     */
    public static List<MemberShape> getHeaderMembers(StructureShape structure) {
        return structure.getAllMembers().values().stream()
            .filter(member -> member.hasTrait(HttpHeaderTrait.class))
            .collect(Collectors.toList());
    }
    
    /**
     * Gets all members bound to HTTP query parameters.
     * 
     * @param structure The structure shape
     * @return List of members with @httpQuery trait
     */
    public static List<MemberShape> getQueryMembers(StructureShape structure) {
        return structure.getAllMembers().values().stream()
            .filter(member -> member.hasTrait(HttpQueryTrait.class))
            .collect(Collectors.toList());
    }
    
    /**
     * Gets all members bound to HTTP URI labels.
     * 
     * @param structure The structure shape
     * @return List of members with @httpLabel trait
     */
    public static List<MemberShape> getLabelMembers(StructureShape structure) {
        return structure.getAllMembers().values().stream()
            .filter(member -> member.hasTrait(HttpLabelTrait.class))
            .collect(Collectors.toList());
    }
    
    /**
     * Gets the member bound to HTTP payload.
     * 
     * @param structure The structure shape
     * @return Optional containing the member with @httpPayload trait
     */
    public static Optional<MemberShape> getPayloadMember(StructureShape structure) {
        return structure.getAllMembers().values().stream()
            .filter(member -> member.hasTrait(HttpPayloadTrait.class))
            .findFirst();
    }
    
    /**
     * Gets all members that should be included in the body.
     * This excludes members bound to headers, query params, URI labels, or payload.
     * 
     * @param structure The structure shape
     * @return List of members for body serialization
     */
    public static List<MemberShape> getBodyMembers(StructureShape structure) {
        return structure.getAllMembers().values().stream()
            .filter(member -> !member.hasTrait(HttpHeaderTrait.class))
            .filter(member -> !member.hasTrait(HttpQueryTrait.class))
            .filter(member -> !member.hasTrait(HttpLabelTrait.class))
            .filter(member -> !member.hasTrait(HttpPayloadTrait.class))
            .collect(Collectors.toList());
    }
    
    /**
     * Checks if an operation has any HTTP binding traits.
     * 
     * @param operation The operation shape
     * @return true if operation uses REST-style bindings
     */
    public static boolean hasHttpBindings(OperationShape operation) {
        return operation.hasTrait(HttpTrait.class);
    }
    
    /**
     * Gets the service name for protocol headers (e.g., X-Amz-Target).
     * 
     * @param service The service shape
     * @return Service name
     */
    public static String getServiceName(ServiceShape service) {
        return service.getId().getName();
    }
    
    /**
     * Gets the service version.
     * 
     * @param service The service shape
     * @return Service version
     */
    public static String getServiceVersion(ServiceShape service) {
        return service.getVersion();
    }
    
    /**
     * Converts a shape name to snake_case for Erlang function names.
     * 
     * @param name The shape name (PascalCase)
     * @return snake_case name
     */
    public static String toSnakeCase(String name) {
        return name.replaceAll("([a-z])([A-Z])", "$1_$2")
                   .replaceAll("([A-Z])([A-Z][a-z])", "$1_$2")
                   .toLowerCase();
    }
}
