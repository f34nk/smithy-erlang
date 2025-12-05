package io.smithy.erlang.codegen.integrations;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangIntegration;
import io.smithy.erlang.codegen.generators.RuntimeModuleGenerator;
import software.amazon.smithy.model.shapes.ServiceShape;

import java.util.logging.Logger;

/**
 * Integration that adds retry support with exponential backoff.
 * 
 * <p>This integration copies the {@code aws_retry.erl} runtime module which
 * provides retry logic for transient errors. The module handles:
 * <ul>
 *   <li>HTTP 5xx (server errors)</li>
 *   <li>HTTP 429 (too many requests / throttling)</li>
 *   <li>Connection errors (timeout, econnrefused, ehostunreach)</li>
 * </ul>
 * 
 * <h2>Retry Strategy</h2>
 * <p>The retry module implements exponential backoff with jitter:
 * <ul>
 *   <li>Default: 3 max retries</li>
 *   <li>Initial backoff: 100ms</li>
 *   <li>Max backoff: 20 seconds</li>
 *   <li>Backoff multiplier: 2x</li>
 *   <li>Random jitter to prevent thundering herd</li>
 * </ul>
 * 
 * <h2>Priority</h2>
 * <p>This integration runs with priority 16, after authentication (64) and
 * protocol (32) integrations, since retry is a general cross-cutting concern
 * that depends on the operation being already set up.
 * 
 * <h2>Always Enabled</h2>
 * <p>Unlike SigV4 or protocol integrations that detect specific traits, the
 * retry integration is always enabled for all generated clients. Retry logic
 * is a best practice for any network operation.
 * 
 * <h2>Example Usage</h2>
 * <p>Generated clients automatically use retry:
 * <pre>
 * %% In generated client code:
 * aws_retry:with_retry(fun() -> make_request(Client, Input) end)
 * </pre>
 * 
 * <p>Custom retry options:
 * <pre>
 * aws_retry:with_retry(
 *     fun() -> make_request(Client, Input) end,
 *     #{max_retries => 5, initial_backoff => 200}
 * )
 * </pre>
 * 
 * @see ErlangIntegration
 * @see RuntimeModuleGenerator
 */
public final class AwsRetryIntegration implements ErlangIntegration {
    
    private static final Logger LOGGER = Logger.getLogger(AwsRetryIntegration.class.getName());
    
    /**
     * Creates a new AWS retry integration.
     */
    public AwsRetryIntegration() {
        // Default constructor
    }
    
    /**
     * Gets the name of this integration.
     *
     * @return "AwsRetryIntegration"
     */
    @Override
    public String name() {
        return "AwsRetryIntegration";
    }
    
    /**
     * Gets the priority of this integration.
     * 
     * <p>Returns 16 to run after authentication (64) and protocol (32)
     * integrations. Retry is a general cross-cutting concern that should
     * be applied after the core request infrastructure is in place.
     *
     * @return 16 (medium priority)
     */
    @Override
    public byte priority() {
        return 16;
    }
    
    /**
     * Preprocesses the model to add retry support.
     * 
     * <p>Copies the {@code aws_retry.erl} runtime module to provide
     * retry logic with exponential backoff for all generated clients.
     *
     * @param context The code generation context
     */
    @Override
    public void preprocessModel(ErlangContext context) {
        ServiceShape service = context.serviceShape();
        
        if (service == null) {
            LOGGER.fine("No service shape found, skipping retry integration");
            return;
        }
        
        LOGGER.info("Adding retry support for service: " + service.getId().getName());
        copyRetryModule(context);
    }
    
    /**
     * Copies the retry runtime module.
     *
     * @param context The code generation context
     */
    private void copyRetryModule(ErlangContext context) {
        boolean copied = RuntimeModuleGenerator.copyModule("aws_retry.erl", context);
        if (copied) {
            LOGGER.fine("Copied aws_retry.erl module");
        } else {
            LOGGER.warning("Failed to copy aws_retry.erl module");
        }
    }
}
