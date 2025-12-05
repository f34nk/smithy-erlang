package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangWriter;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

/**
 * Interface for protocol-specific code generation in the new architecture.
 * 
 * <p>This interface defines the contract for generating Erlang client code
 * for different AWS protocols. Unlike the legacy {@link Protocol} interface,
 * this one uses {@link ErlangContext} to encapsulate all code generation
 * dependencies, making it more aligned with Smithy's DirectedCodegen pattern.
 * 
 * <h2>Supported Protocols</h2>
 * <p>Implementations exist for all major AWS protocols:
 * <ul>
 *   <li>{@code aws.protocols#awsJson1_0} - AWS JSON 1.0 (DynamoDB)</li>
 *   <li>{@code aws.protocols#awsJson1_1} - AWS JSON 1.1 (Lambda, SNS)</li>
 *   <li>{@code aws.protocols#awsQuery} - AWS Query (SQS, IAM, STS)</li>
 *   <li>{@code aws.protocols#ec2Query} - EC2 Query (EC2)</li>
 *   <li>{@code aws.protocols#restXml} - REST-XML (S3, CloudFront)</li>
 *   <li>{@code aws.protocols#restJson1} - REST-JSON (API Gateway)</li>
 * </ul>
 * 
 * <h2>Generation Flow</h2>
 * <p>Protocol generators are typically invoked during service generation:
 * <pre>
 * ServiceGenerator
 *     └── ProtocolGenerator.generateOperation()
 *         ├── generateRequestSerializer()
 *         └── generateResponseDeserializer()
 * </pre>
 * 
 * <h2>Implementation Example</h2>
 * <pre>
 * public class AwsJsonProtocolGenerator implements ProtocolGenerator {
 *     
 *     {@literal @}Override
 *     public ShapeId getProtocol() {
 *         return ShapeId.from("aws.protocols#awsJson1_0");
 *     }
 *     
 *     {@literal @}Override
 *     public void generateOperation(OperationShape operation, 
 *                                    ErlangWriter writer, 
 *                                    ErlangContext context) {
 *         // Generate operation function
 *     }
 *     // ... other methods
 * }
 * </pre>
 * 
 * <h2>Migration Notes</h2>
 * <p>This interface is part of the Phase 6 architecture migration. Existing
 * protocol classes (e.g., {@code AwsJsonProtocol}, {@code RestXmlProtocol})
 * should be migrated to implement this interface.
 * 
 * @see Protocol (legacy interface)
 * @see ErlangContext
 * @see io.smithy.erlang.codegen.integrations.AwsProtocolIntegration
 */
public interface ProtocolGenerator {
    
    /**
     * Gets the protocol trait ID this generator handles.
     * 
     * <p>This should return the ShapeId of the protocol trait, e.g.:
     * <ul>
     *   <li>{@code ShapeId.from("aws.protocols#awsJson1_0")}</li>
     *   <li>{@code ShapeId.from("aws.protocols#restXml")}</li>
     *   <li>{@code ShapeId.from("aws.protocols#awsQuery")}</li>
     * </ul>
     *
     * @return The ShapeId of the protocol trait this generator handles
     */
    ShapeId getProtocol();
    
    /**
     * Gets a human-readable name for this protocol generator.
     * 
     * <p>Used for logging and debugging purposes.
     *
     * @return A descriptive name for the protocol generator
     */
    default String getName() {
        return getProtocol().getName();
    }
    
    /**
     * Generates the complete operation function.
     * 
     * <p>This method is responsible for generating the full Erlang function
     * for an operation, including:
     * <ul>
     *   <li>Function spec and signature</li>
     *   <li>Input validation</li>
     *   <li>Request serialization (via {@link #generateRequestSerializer})</li>
     *   <li>HTTP request construction and execution</li>
     *   <li>Response deserialization (via {@link #generateResponseDeserializer})</li>
     *   <li>Error handling</li>
     * </ul>
     *
     * @param operation The operation shape to generate
     * @param writer The Erlang code writer
     * @param context The code generation context
     */
    void generateOperation(OperationShape operation, ErlangWriter writer, ErlangContext context);
    
    /**
     * Generates request serialization code for an operation.
     * 
     * <p>This generates the code that transforms the Erlang input map
     * into the protocol-specific request format:
     * <ul>
     *   <li>AWS JSON: JSON body with {@code jsx:encode/1}</li>
     *   <li>REST-XML: XML body with {@code aws_xml:encode/1}</li>
     *   <li>AWS Query: Form-encoded body with {@code aws_query:encode/2}</li>
     *   <li>REST-JSON: JSON body with optional path/query/header bindings</li>
     * </ul>
     *
     * @param operation The operation shape
     * @param writer The Erlang code writer
     * @param context The code generation context
     */
    void generateRequestSerializer(OperationShape operation, ErlangWriter writer, ErlangContext context);
    
    /**
     * Generates response deserialization code for an operation.
     * 
     * <p>This generates the code that transforms the HTTP response
     * into the Erlang output map:
     * <ul>
     *   <li>Parsing the response body (JSON, XML, etc.)</li>
     *   <li>Extracting headers if needed</li>
     *   <li>Mapping response fields to output structure</li>
     *   <li>Error response parsing</li>
     * </ul>
     *
     * @param operation The operation shape
     * @param writer The Erlang code writer
     * @param context The code generation context
     */
    void generateResponseDeserializer(OperationShape operation, ErlangWriter writer, ErlangContext context);
    
    /**
     * Generates protocol-specific HTTP headers.
     * 
     * <p>Each protocol has its own required headers:
     * <ul>
     *   <li>AWS JSON: {@code X-Amz-Target}, {@code Content-Type: application/x-amz-json-1.x}</li>
     *   <li>AWS Query: {@code Content-Type: application/x-www-form-urlencoded}</li>
     *   <li>REST-XML: {@code Content-Type: application/xml}</li>
     *   <li>REST-JSON: {@code Content-Type: application/json}</li>
     * </ul>
     *
     * @param operation The operation shape
     * @param service The service shape
     * @param writer The Erlang code writer
     * @param context The code generation context
     */
    default void generateHeaders(OperationShape operation, ServiceShape service, 
                                  ErlangWriter writer, ErlangContext context) {
        // Default: no additional headers
    }
    
    /**
     * Gets the Content-Type header value for this protocol.
     *
     * @param service The service shape
     * @return The Content-Type header value
     */
    String getContentType(ServiceShape service);
    
    /**
     * Gets the default HTTP method for this protocol.
     * 
     * <p>For non-REST protocols (AWS JSON, AWS Query), this returns
     * a fixed method like "POST". For REST protocols, this may return
     * null if the method is determined by operation traits.
     *
     * @return The default HTTP method, or null if determined by traits
     */
    default String getDefaultMethod() {
        return "POST";
    }
    
    /**
     * Gets the default URI path for this protocol.
     * 
     * <p>For non-REST protocols, this typically returns "/". For REST
     * protocols, the path is determined by {@code @http} traits.
     *
     * @return The default URI path, or null if determined by traits
     */
    default String getDefaultUri() {
        return "/";
    }
    
    /**
     * Checks if this generator applies to a given service.
     * 
     * <p>This default implementation checks if the service has the
     * protocol trait returned by {@link #getProtocol()}.
     *
     * @param service The service shape to check
     * @return true if this generator handles the service's protocol
     */
    default boolean appliesTo(ServiceShape service) {
        return service.findTrait(getProtocol()).isPresent();
    }
    
    /**
     * Generates error parsing code.
     * 
     * <p>Each protocol has a different error response format:
     * <ul>
     *   <li>AWS JSON: {@code __type} field in JSON body</li>
     *   <li>AWS Query: {@code <ErrorResponse><Error><Code>} in XML</li>
     *   <li>REST-XML: {@code <Error><Code>} in XML</li>
     *   <li>REST-JSON: {@code x-amzn-ErrorType} header or body field</li>
     * </ul>
     *
     * @param operation The operation shape
     * @param writer The Erlang code writer
     * @param context The code generation context
     */
    default void generateErrorParser(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        // Default error parsing - can be overridden by protocol implementations
    }
}
