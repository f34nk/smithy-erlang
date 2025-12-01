package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.ErlangSymbolProvider;
import io.smithy.erlang.codegen.ErlangWriter;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;

/**
 * Interface for AWS protocol implementations.
 * 
 * This interface defines the contract for generating Erlang code for different
 * AWS protocols (restJson1, awsJson1_0, awsJson1_1, awsQuery, ec2Query, restXml).
 * 
 * Each protocol implementation handles:
 * - Protocol-specific HTTP methods and URIs
 * - Request/response serialization format (JSON, XML, Query)
 * - Content-Type headers
 * - Error response parsing
 */
public interface Protocol {
    
    /**
     * Returns the protocol name (e.g., "restJson1", "awsJson1_0", "restXml").
     */
    String getName();
    
    /**
     * Returns the default HTTP method for this protocol.
     * 
     * @return HTTP method (e.g., "POST") or null for REST-based protocols
     *         where method is determined by operation traits
     */
    String getDefaultMethod();
    
    /**
     * Returns the default URI path for this protocol.
     * 
     * @return URI path (e.g., "/") or null for REST-based protocols
     *         where path is determined by operation traits
     */
    String getDefaultUri();
    
    /**
     * Returns the Content-Type header value for this protocol.
     * 
     * @param service The service shape
     * @return Content-Type value (e.g., "application/x-amz-json-1.0")
     */
    String getContentType(ServiceShape service);
    
    /**
     * Generates the complete operation function including request and response handling.
     * 
     * @param operation The operation shape to generate
     * @param service The service shape
     * @param model The Smithy model
     * @param symbolProvider Symbol provider for type names
     * @param writer Writer for generating Erlang code
     */
    void generateOperation(
        OperationShape operation,
        ServiceShape service,
        Model model,
        ErlangSymbolProvider symbolProvider,
        ErlangWriter writer
    );
    
    /**
     * Generates protocol-specific HTTP headers.
     * 
     * @param operation The operation shape
     * @param service The service shape
     * @param writer Writer for generating Erlang code
     */
    void generateHeaders(
        OperationShape operation,
        ServiceShape service,
        ErlangWriter writer
    );
    
    /**
     * Generates the request body serialization code.
     * 
     * @param operation The operation shape
     * @param model The Smithy model
     * @param writer Writer for generating Erlang code
     */
    void generateRequestBody(
        OperationShape operation,
        Model model,
        ErlangWriter writer
    );
    
    /**
     * Generates the response deserialization and parsing code.
     * 
     * @param operation The operation shape
     * @param model The Smithy model
     * @param writer Writer for generating Erlang code
     */
    void generateResponseDecoding(
        OperationShape operation,
        Model model,
        ErlangWriter writer
    );
}
