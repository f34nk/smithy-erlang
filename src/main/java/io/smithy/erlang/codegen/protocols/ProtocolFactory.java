package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.aws.AwsProtocol;

/**
 * Factory for creating Protocol instances from AwsProtocol enum values.
 * 
 * This factory maps the AwsProtocol enum to concrete Protocol implementation instances.
 * 
 * @deprecated Use the new {@link ProtocolGenerator} implementations directly.
 *             This factory is maintained for backward compatibility with
 *             {@link io.smithy.erlang.codegen.ErlangClientPlugin}.
 * @see AwsJsonProtocolGenerator
 * @see AwsQueryProtocolGenerator
 * @see Ec2QueryProtocolGenerator
 * @see RestXmlProtocolGenerator
 * @see RestJsonProtocolGenerator
 */
@Deprecated
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
                return new AwsQueryProtocol();
                
            case EC2_QUERY:
                return new Ec2QueryProtocol();
                
            case REST_XML:
                return new RestXmlProtocol();
                
            case REST_JSON_1:
                return new RestJsonProtocol();
                
            default:
                throw new UnsupportedOperationException(
                    "Unknown protocol: " + awsProtocol);
        }
    }
}
