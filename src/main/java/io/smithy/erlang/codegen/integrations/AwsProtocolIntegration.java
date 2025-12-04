package io.smithy.erlang.codegen.integrations;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangIntegration;
import io.smithy.erlang.codegen.aws.AwsProtocol;
import io.smithy.erlang.codegen.aws.AwsProtocolDetector;
import io.smithy.erlang.codegen.generators.RuntimeModuleGenerator;
import software.amazon.smithy.model.shapes.ServiceShape;

import java.util.logging.Logger;

/**
 * Integration that provides protocol-specific runtime module support.
 * 
 * <p>This integration detects which AWS protocol a service uses and copies
 * the appropriate Erlang runtime modules for that protocol's serialization
 * and deserialization needs.
 * 
 * <p>Protocol-specific modules copied:
 * <ul>
 *   <li><strong>REST-XML, AWS Query, EC2 Query</strong>: {@code aws_xml.erl}</li>
 *   <li><strong>AWS Query, EC2 Query</strong>: {@code aws_query.erl}</li>
 *   <li><strong>REST-XML (S3)</strong>: {@code aws_s3.erl}</li>
 * </ul>
 * 
 * <h2>Protocol Detection</h2>
 * <p>The integration uses {@link AwsProtocolDetector} to determine which
 * AWS protocol trait is applied to the service:
 * <ul>
 *   <li>{@code aws.protocols#awsJson1_0} - AWS JSON 1.0</li>
 *   <li>{@code aws.protocols#awsJson1_1} - AWS JSON 1.1</li>
 *   <li>{@code aws.protocols#awsQuery} - AWS Query</li>
 *   <li>{@code aws.protocols#ec2Query} - EC2 Query</li>
 *   <li>{@code aws.protocols#restXml} - REST-XML</li>
 *   <li>{@code aws.protocols#restJson1} - REST-JSON</li>
 * </ul>
 * 
 * <h2>Priority</h2>
 * <p>This integration runs with priority 32, after authentication integrations
 * (priority 64) but before general code generation.
 * 
 * <h2>Example</h2>
 * <p>A service using REST-XML protocol:
 * <pre>
 * {@literal @}aws.protocols#restXml
 * service S3 { ... }
 * </pre>
 * <p>This will automatically copy {@code aws_xml.erl} and {@code aws_s3.erl}.
 * 
 * @see ErlangIntegration
 * @see AwsProtocolDetector
 * @see AwsProtocol
 */
public final class AwsProtocolIntegration implements ErlangIntegration {
    
    private static final Logger LOGGER = Logger.getLogger(AwsProtocolIntegration.class.getName());
    
    /**
     * Creates a new AWS protocol integration.
     */
    public AwsProtocolIntegration() {
        // Default constructor
    }
    
    /**
     * Gets the name of this integration.
     *
     * @return "AwsProtocolIntegration"
     */
    @Override
    public String name() {
        return "AwsProtocolIntegration";
    }
    
    /**
     * Gets the priority of this integration.
     * 
     * <p>Returns 32 to run after authentication integrations (priority 64)
     * but before general code generation. Protocol modules need to be
     * available when operations are generated.
     *
     * @return 32 (medium-high priority)
     */
    @Override
    public byte priority() {
        return 32;
    }
    
    /**
     * Preprocesses the model to copy protocol-specific runtime modules.
     * 
     * <p>Detects the AWS protocol used by the service and copies the
     * appropriate Erlang runtime modules for serialization/deserialization.
     *
     * @param context The code generation context
     */
    @Override
    public void preprocessModel(ErlangContext context) {
        ServiceShape service = context.serviceShape();
        
        if (service == null) {
            LOGGER.fine("No service shape found, skipping protocol integration");
            return;
        }
        
        AwsProtocol protocol = AwsProtocolDetector.detectProtocol(service);
        LOGGER.info("Detected protocol " + protocol + " for service: " + service.getId().getName());
        
        copyProtocolModules(protocol, context);
    }
    
    /**
     * Copies the runtime modules needed for a specific protocol.
     *
     * @param protocol The detected AWS protocol
     * @param context The code generation context
     */
    private void copyProtocolModules(AwsProtocol protocol, ErlangContext context) {
        switch (protocol) {
            case REST_XML:
                copyRestXmlModules(context);
                break;
                
            case AWS_QUERY:
            case EC2_QUERY:
                copyQueryProtocolModules(context);
                break;
                
            case AWS_JSON_1_0:
            case AWS_JSON_1_1:
                copyJsonProtocolModules(context);
                break;
                
            case REST_JSON_1:
                copyRestJsonModules(context);
                break;
                
            default:
                LOGGER.fine("No additional modules needed for protocol: " + protocol);
        }
    }
    
    /**
     * Copies modules needed for REST-XML protocol (S3, CloudFront, Route53).
     *
     * @param context The code generation context
     */
    private void copyRestXmlModules(ErlangContext context) {
        LOGGER.info("Copying REST-XML protocol modules");
        
        // aws_xml.erl - XML encoding/decoding
        boolean xmlCopied = RuntimeModuleGenerator.copyModule("aws_xml.erl", context);
        if (xmlCopied) {
            LOGGER.fine("Copied aws_xml.erl module");
        }
        
        // aws_s3.erl - S3-specific helpers (URL construction, multipart, etc.)
        boolean s3Copied = RuntimeModuleGenerator.copyModule("aws_s3.erl", context);
        if (s3Copied) {
            LOGGER.fine("Copied aws_s3.erl module");
        }
    }
    
    /**
     * Copies modules needed for AWS Query and EC2 Query protocols.
     *
     * @param context The code generation context
     */
    private void copyQueryProtocolModules(ErlangContext context) {
        LOGGER.info("Copying Query protocol modules");
        
        // aws_query.erl - Query string encoding
        boolean queryCopied = RuntimeModuleGenerator.copyModule("aws_query.erl", context);
        if (queryCopied) {
            LOGGER.fine("Copied aws_query.erl module");
        }
        
        // aws_xml.erl - Query protocols use XML for responses
        boolean xmlCopied = RuntimeModuleGenerator.copyModule("aws_xml.erl", context);
        if (xmlCopied) {
            LOGGER.fine("Copied aws_xml.erl module");
        }
    }
    
    /**
     * Copies modules needed for AWS JSON protocols.
     *
     * @param context The code generation context
     */
    private void copyJsonProtocolModules(ErlangContext context) {
        LOGGER.info("Copying AWS JSON protocol modules");
        // JSON protocols use jsx library which is a dependency
        // No additional runtime modules needed beyond the core aws_* modules
        LOGGER.fine("AWS JSON protocol uses jsx library - no additional modules needed");
    }
    
    /**
     * Copies modules needed for REST-JSON protocol.
     *
     * @param context The code generation context
     */
    private void copyRestJsonModules(ErlangContext context) {
        LOGGER.info("Copying REST-JSON protocol modules");
        // REST-JSON uses jsx library which is a dependency
        // No additional runtime modules needed beyond the core aws_* modules
        LOGGER.fine("REST-JSON protocol uses jsx library - no additional modules needed");
    }
    
    /**
     * Checks if the given service uses an XML-based protocol.
     *
     * @param service The service shape
     * @return true if the service uses REST-XML, AWS Query, or EC2 Query
     */
    public boolean usesXmlProtocol(ServiceShape service) {
        AwsProtocol protocol = AwsProtocolDetector.detectProtocol(service);
        return protocol.isXmlProtocol();
    }
    
    /**
     * Checks if the given service uses a Query-based protocol.
     *
     * @param service The service shape
     * @return true if the service uses AWS Query or EC2 Query
     */
    public boolean usesQueryProtocol(ServiceShape service) {
        AwsProtocol protocol = AwsProtocolDetector.detectProtocol(service);
        return protocol.isQueryProtocol();
    }
    
    /**
     * Gets the detected protocol for a service.
     *
     * @param service The service shape
     * @return The detected AWS protocol
     */
    public AwsProtocol getProtocol(ServiceShape service) {
        return AwsProtocolDetector.detectProtocol(service);
    }
}
