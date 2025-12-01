package io.smithy.erlang.codegen.aws;

import io.smithy.model.shapes.ServiceShape;
import io.smithy.model.traits.Trait;

import java.util.Optional;
import java.util.logging.Logger;

/**
 * Detects the AWS protocol used by a service based on its traits.
 * 
 * This detector examines the service shape for AWS protocol traits and
 * returns the corresponding AwsProtocol enum value. If no protocol trait
 * is found, it defaults to REST_JSON_1.
 * 
 * Supported protocols:
 * - aws.protocols#awsJson1_0
 * - aws.protocols#awsJson1_1
 * - aws.protocols#awsQuery
 * - aws.protocols#ec2Query
 * - aws.protocols#restXml
 * - aws.protocols#restJson1
 */
public final class AwsProtocolDetector {
    
    private static final Logger LOGGER = Logger.getLogger(AwsProtocolDetector.class.getName());
    
    private AwsProtocolDetector() {
        // Utility class - prevent instantiation
    }
    
    /**
     * Detects the AWS protocol from a service shape.
     * 
     * @param service The service shape to analyze
     * @return The detected AWS protocol, or REST_JSON_1 as default
     */
    public static AwsProtocol detectProtocol(ServiceShape service) {
        // Check for each AWS protocol trait in order of specificity
        
        // AWS JSON 1.0
        if (hasProtocolTrait(service, AwsProtocol.AWS_JSON_1_0)) {
            LOGGER.info("Detected AWS JSON 1.0 protocol for service: " + service.getId().getName());
            return AwsProtocol.AWS_JSON_1_0;
        }
        
        // AWS JSON 1.1
        if (hasProtocolTrait(service, AwsProtocol.AWS_JSON_1_1)) {
            LOGGER.info("Detected AWS JSON 1.1 protocol for service: " + service.getId().getName());
            return AwsProtocol.AWS_JSON_1_1;
        }
        
        // AWS Query
        if (hasProtocolTrait(service, AwsProtocol.AWS_QUERY)) {
            LOGGER.info("Detected AWS Query protocol for service: " + service.getId().getName());
            return AwsProtocol.AWS_QUERY;
        }
        
        // EC2 Query
        if (hasProtocolTrait(service, AwsProtocol.EC2_QUERY)) {
            LOGGER.info("Detected EC2 Query protocol for service: " + service.getId().getName());
            return AwsProtocol.EC2_QUERY;
        }
        
        // REST-XML
        if (hasProtocolTrait(service, AwsProtocol.REST_XML)) {
            LOGGER.info("Detected REST-XML protocol for service: " + service.getId().getName());
            return AwsProtocol.REST_XML;
        }
        
        // REST-JSON 1
        if (hasProtocolTrait(service, AwsProtocol.REST_JSON_1)) {
            LOGGER.info("Detected REST-JSON 1 protocol for service: " + service.getId().getName());
            return AwsProtocol.REST_JSON_1;
        }
        
        // Default to REST-JSON 1 for HTTP-based services
        LOGGER.warning("No AWS protocol trait found for service: " + service.getId().getName() 
                     + ". Defaulting to REST-JSON 1.");
        return AwsProtocol.REST_JSON_1;
    }
    
    /**
     * Checks if a service has a specific protocol trait.
     * 
     * @param service The service shape to check
     * @param protocol The protocol to check for
     * @return true if the service has the protocol trait
     */
    private static boolean hasProtocolTrait(ServiceShape service, AwsProtocol protocol) {
        String traitName = protocol.getTraitName();
        
        // Try to get the trait by its shape ID
        Optional<Trait> trait = service.getAllTraits().values().stream()
            .filter(t -> t.toShapeId().toString().equals(traitName))
            .findFirst();
        
        return trait.isPresent();
    }
    
    /**
     * Gets a human-readable description of the detected protocol.
     * 
     * @param service The service shape
     * @return Description of the protocol
     */
    public static String getProtocolDescription(ServiceShape service) {
        AwsProtocol protocol = detectProtocol(service);
        
        switch (protocol) {
            case AWS_JSON_1_0:
                return "AWS JSON Protocol 1.0 (DynamoDB-style)";
            case AWS_JSON_1_1:
                return "AWS JSON Protocol 1.1 (Kinesis-style)";
            case AWS_QUERY:
                return "AWS Query Protocol (SQS-style)";
            case EC2_QUERY:
                return "EC2 Query Protocol (EC2-style)";
            case REST_XML:
                return "REST-XML Protocol (S3-style)";
            case REST_JSON_1:
                return "REST-JSON Protocol 1 (API Gateway-style)";
            default:
                return "Unknown Protocol";
        }
    }
    
    /**
     * Checks if the service uses a REST-based protocol.
     * 
     * @param service The service shape
     * @return true if the service uses REST-XML or REST-JSON
     */
    public static boolean isRestProtocol(ServiceShape service) {
        return detectProtocol(service).isRestProtocol();
    }
    
    /**
     * Checks if the service uses a JSON-based protocol.
     * 
     * @param service The service shape
     * @return true if the service uses any JSON protocol
     */
    public static boolean isJsonProtocol(ServiceShape service) {
        return detectProtocol(service).isJsonProtocol();
    }
    
    /**
     * Checks if the service uses an XML-based protocol.
     * 
     * @param service The service shape
     * @return true if the service uses any XML protocol
     */
    public static boolean isXmlProtocol(ServiceShape service) {
        return detectProtocol(service).isXmlProtocol();
    }
    
    /**
     * Checks if the service uses a query-based protocol.
     * 
     * @param service The service shape
     * @return true if the service uses awsQuery or ec2Query
     */
    public static boolean isQueryProtocol(ServiceShape service) {
        return detectProtocol(service).isQueryProtocol();
    }
}
