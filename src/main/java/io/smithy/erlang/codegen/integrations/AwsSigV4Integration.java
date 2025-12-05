package io.smithy.erlang.codegen.integrations;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangIntegration;
import io.smithy.erlang.codegen.generators.RuntimeModuleGenerator;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.Trait;

import java.util.Optional;
import java.util.logging.Logger;

/**
 * Integration that adds AWS SigV4 signing support to generated Erlang clients.
 * 
 * <p>This integration detects when a service uses AWS Signature Version 4
 * authentication (via the {@code aws.auth#sigv4} trait) and copies the
 * necessary Erlang runtime modules for request signing.
 * 
 * <p>When a service has the SigV4 trait, this integration copies:
 * <ul>
 *   <li>{@code aws_sigv4.erl} - AWS Signature Version 4 implementation</li>
 *   <li>{@code aws_credentials.erl} - Credential provider chain</li>
 *   <li>{@code aws_config.erl} - AWS region and endpoint configuration</li>
 * </ul>
 * 
 * <h2>SigV4 Trait Detection</h2>
 * <p>The integration checks for the {@code aws.auth#sigv4} trait on the
 * service shape. This trait is defined in the Smithy AWS traits package.
 * 
 * <h2>Priority</h2>
 * <p>This integration runs with priority 64 (early) to ensure authentication
 * modules are available before other code generation occurs.
 * 
 * <h2>Example Usage</h2>
 * <p>Services with SigV4 are automatically detected:
 * <pre>
 * {@literal @}aws.auth#sigv4(name: "dynamodb")
 * service DynamoDB { ... }
 * </pre>
 * 
 * @see ErlangIntegration
 * @see RuntimeModuleGenerator
 */
public final class AwsSigV4Integration implements ErlangIntegration {
    
    private static final Logger LOGGER = Logger.getLogger(AwsSigV4Integration.class.getName());
    
    /** The shape ID for the aws.auth#sigv4 trait. */
    private static final ShapeId SIGV4_TRAIT_ID = ShapeId.from("aws.auth#sigv4");
    
    /**
     * Creates a new AWS SigV4 integration.
     */
    public AwsSigV4Integration() {
        // Default constructor
    }
    
    /**
     * Gets the name of this integration.
     *
     * @return "AwsSigV4Integration"
     */
    @Override
    public String name() {
        return "AwsSigV4Integration";
    }
    
    /**
     * Gets the priority of this integration.
     * 
     * <p>Returns 64 to run early in the integration chain, ensuring that
     * authentication modules are available before other generators run.
     *
     * @return 64 (high priority)
     */
    @Override
    public byte priority() {
        return 64;
    }
    
    /**
     * Preprocesses the model to add SigV4 signing support.
     * 
     * <p>If the service has the {@code aws.auth#sigv4} trait, this method
     * copies the necessary runtime modules for AWS request signing.
     *
     * @param context The code generation context
     */
    @Override
    public void preprocessModel(ErlangContext context) {
        Model model = context.model();
        ServiceShape service = context.serviceShape();
        
        if (service == null) {
            LOGGER.fine("No service shape found, skipping SigV4 integration");
            return;
        }
        
        if (hasSigV4Trait(service)) {
            LOGGER.info("Service has SigV4 trait, copying authentication modules");
            copyAuthenticationModules(context);
        } else {
            LOGGER.fine("Service does not have SigV4 trait, skipping authentication modules");
        }
    }
    
    /**
     * Checks if a service shape has the aws.auth#sigv4 trait.
     *
     * @param service The service shape to check
     * @return true if the service has the SigV4 trait
     */
    public boolean hasSigV4Trait(ServiceShape service) {
        // Check for the trait by ID since the trait class may not be on classpath
        Optional<Trait> sigv4Trait = service.findTrait(SIGV4_TRAIT_ID);
        return sigv4Trait.isPresent();
    }
    
    /**
     * Gets the SigV4 signing name from the service trait.
     * 
     * <p>The signing name is specified in the {@code name} property of the
     * {@code aws.auth#sigv4} trait.
     *
     * @param service The service shape
     * @return Optional containing the signing name, or empty if not found
     */
    public Optional<String> getSigningName(ServiceShape service) {
        return service.findTrait(SIGV4_TRAIT_ID)
                .flatMap(trait -> trait.toNode().asObjectNode())
                .flatMap(node -> node.getStringMember("name"))
                .map(n -> n.getValue());
    }
    
    /**
     * Copies the authentication-related runtime modules.
     *
     * @param context The code generation context
     */
    private void copyAuthenticationModules(ErlangContext context) {
        // Copy aws_sigv4.erl - AWS Signature Version 4 implementation
        boolean sigv4Copied = RuntimeModuleGenerator.copyModule("aws_sigv4.erl", context);
        if (sigv4Copied) {
            LOGGER.fine("Copied aws_sigv4.erl module");
        }
        
        // Copy aws_credentials.erl - Credential provider chain
        boolean credentialsCopied = RuntimeModuleGenerator.copyModule("aws_credentials.erl", context);
        if (credentialsCopied) {
            LOGGER.fine("Copied aws_credentials.erl module");
        }
        
        // Copy aws_config.erl - AWS configuration (region, endpoints)
        boolean configCopied = RuntimeModuleGenerator.copyModule("aws_config.erl", context);
        if (configCopied) {
            LOGGER.fine("Copied aws_config.erl module");
        }
    }
}
