package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.aws.AwsProtocol;

/**
 * Factory for creating Protocol instances from AwsProtocol enum values.
 * 
 * This factory maps the AwsProtocol enum to concrete Protocol implementation instances.
 */
public final class ProtocolFactory {
    
    private ProtocolFactory() {
        // Utility class - prevent instantiation
    }
    
    /**
     * Creates a Protocol instance from an AwsProtocol enum value.
     * 
     * @param awsProtocol The AWS protocol enum value
     * @return A Protocol implementation instance
     * @throws UnsupportedOperationException if the protocol is not yet implemented
     */
    public static Protocol createProtocol(AwsProtocol awsProtocol) {
        switch (awsProtocol) {
            case AWS_JSON_1_0:
                return new AwsJsonProtocol("1.0");
                
            case AWS_JSON_1_1:
                return new AwsJsonProtocol("1.1");
                
            case AWS_QUERY:
                throw new UnsupportedOperationException(
                    "AWS Query protocol not yet implemented. " +
                    "This protocol will be added in Step 7.4.");
                
            case EC2_QUERY:
                throw new UnsupportedOperationException(
                    "EC2 Query protocol not yet implemented. " +
                    "This protocol will be added in Step 7.4.");
                
            case REST_XML:
                throw new UnsupportedOperationException(
                    "REST-XML protocol not yet implemented. " +
                    "This protocol will be added in Step 7.3.");
                
            case REST_JSON_1:
                throw new UnsupportedOperationException(
                    "REST-JSON protocol not yet implemented. " +
                    "This protocol will be added in Step 7.5.");
                
            default:
                throw new UnsupportedOperationException(
                    "Unknown protocol: " + awsProtocol);
        }
    }
}
