package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

/**
 * Protocol generator for AWS JSON 1.0 and 1.1 protocols.
 * 
 * <p>This generator handles code generation for services using the AWS JSON
 * protocol, which is used by DynamoDB, Lambda, Kinesis, SNS, and other
 * JSON-based AWS services.
 * 
 * <h2>Protocol Characteristics</h2>
 * <ul>
 *   <li>HTTP Method: Always POST</li>
 *   <li>URI Path: Always "/"</li>
 *   <li>Content-Type: {@code application/x-amz-json-1.0} or {@code 1.1}</li>
 *   <li>Request Body: JSON encoded with {@code jsx:encode/1}</li>
 *   <li>Response Body: JSON decoded with {@code jsx:decode/2}</li>
 *   <li>Target Header: {@code X-Amz-Target: {ServiceName}_{Version}.{OperationName}}</li>
 *   <li>Authentication: AWS SigV4</li>
 * </ul>
 * 
 * <h2>Services Using AWS JSON</h2>
 * <ul>
 *   <li>DynamoDB (JSON 1.0)</li>
 *   <li>Lambda (JSON 1.1)</li>
 *   <li>Kinesis (JSON 1.1)</li>
 *   <li>SNS (JSON 1.1)</li>
 *   <li>SWF (JSON 1.0)</li>
 * </ul>
 * 
 * <h2>Example Generated Code</h2>
 * <pre>
 * put_item(Client, Input) ->
 *     put_item(Client, Input, #{}).
 * 
 * put_item(Client, Input, Options) when is_map(Input), is_map(Options) ->
 *     RequestFun = fun() -> make_put_item_request(Client, Input) end,
 *     case maps:get(enable_retry, Options, true) of
 *         true -> aws_retry:with_retry(RequestFun, Options);
 *         false -> RequestFun()
 *     end.
 * </pre>
 * 
 * @see ProtocolGenerator
 */
public class AwsJsonProtocolGenerator implements ProtocolGenerator {
    
    /** Protocol trait ID for AWS JSON 1.0 */
    public static final ShapeId AWS_JSON_1_0 = ShapeId.from("aws.protocols#awsJson1_0");
    
    /** Protocol trait ID for AWS JSON 1.1 */
    public static final ShapeId AWS_JSON_1_1 = ShapeId.from("aws.protocols#awsJson1_1");
    
    private final ShapeId protocolId;
    private final String version;
    
    /**
     * Creates an AWS JSON 1.0 protocol generator.
     */
    public AwsJsonProtocolGenerator() {
        this(AWS_JSON_1_0);
    }
    
    /**
     * Creates an AWS JSON protocol generator for the specified version.
     *
     * @param protocolId The protocol trait ID (awsJson1_0 or awsJson1_1)
     */
    public AwsJsonProtocolGenerator(ShapeId protocolId) {
        this.protocolId = protocolId;
        this.version = protocolId.equals(AWS_JSON_1_0) ? "1.0" : "1.1";
    }
    
    /**
     * Creates an AWS JSON 1.1 protocol generator.
     *
     * @return A new generator for AWS JSON 1.1
     */
    public static AwsJsonProtocolGenerator awsJson11() {
        return new AwsJsonProtocolGenerator(AWS_JSON_1_1);
    }
    
    @Override
    public ShapeId getProtocol() {
        return protocolId;
    }
    
    @Override
    public String getName() {
        return "awsJson" + version;
    }
    
    @Override
    public String getDefaultMethod() {
        return "POST";
    }
    
    @Override
    public String getDefaultUri() {
        return "/";
    }
    
    @Override
    public String getContentType(ServiceShape service) {
        return "application/x-amz-json-" + version;
    }
    
    @Override
    public void generateOperation(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        Model model = context.model();
        ServiceShape service = context.serviceShape();
        
        String opName = EnhancedErlangSymbolProvider.toErlangName(operation.getId().getName());
        
        // Determine input and output types
        String inputType = "map()";
        String outputType = "map()";
        
        if (operation.getInput().isPresent()) {
            ShapeId inputId = operation.getInput().get();
            String inputRecordName = EnhancedErlangSymbolProvider.toErlangName(inputId.getName());
            inputType = inputRecordName + "()";
        }
        
        if (operation.getOutput().isPresent()) {
            ShapeId outputId = operation.getOutput().get();
            String outputRecordName = EnhancedErlangSymbolProvider.toErlangName(outputId.getName());
            outputType = outputRecordName + "()";
        }
        
        // Generate 2-arity wrapper
        writer.writeComment("Calls the " + operation.getId().getName() + " operation");
        writer.writeSpec(opName, "Client :: map(), Input :: " + inputType, "{ok, " + outputType + "} | {error, term()}");
        writer.write("$L(Client, Input) ->", opName);
        writer.indent();
        writer.write("$L(Client, Input, #{}).", opName);
        writer.dedent();
        writer.writeBlankLine();
        
        // Generate 3-arity version with retry support
        writer.writeComment("Calls the " + operation.getId().getName() + " operation with options");
        writer.writeComment("Options:");
        writer.writeComment("  - enable_retry: Enable automatic retry (default: true)");
        writer.writeComment("  - max_retries: Maximum retry attempts (default: 3)");
        writer.writeComment("  - initial_backoff: Initial backoff in ms (default: 100)");
        writer.writeComment("  - max_backoff: Maximum backoff in ms (default: 20000)");
        writer.writeSpec(opName, "Client :: map(), Input :: " + inputType + ", Options :: map()", "{ok, " + outputType + "} | {error, term()}");
        writer.write("$L(Client, Input, Options) when is_map(Input), is_map(Options) ->", opName);
        writer.indent();
        
        // Create the request function
        String internalFunctionName = "make_" + opName + "_request";
        writer.write("RequestFun = fun() -> $L(Client, Input) end,", internalFunctionName);
        writer.writeBlankLine();
        writer.writeRetryCase();
        writer.dedent();
        writer.writeBlankLine();
        
        // Generate internal request function
        generateInternalRequestFunction(operation, writer, context, opName, inputType, outputType);
    }
    
    /**
     * Generates the internal request function that performs the actual HTTP call.
     */
    private void generateInternalRequestFunction(OperationShape operation, ErlangWriter writer, 
            ErlangContext context, String opName, String inputType, String outputType) {
        
        String internalFunctionName = "make_" + opName + "_request";
        
        writer.writeComment("Internal function to make the " + operation.getId().getName() + " request");
        writer.writeSpec(internalFunctionName, "Client :: map(), Input :: " + inputType, "{ok, " + outputType + "} | {error, term()}");
        writer.write("$L(Client, Input) when is_map(Input) ->", internalFunctionName);
        writer.indent();
        
        // AWS JSON always uses POST to "/"
        writer.write("Method = <<\"POST\">>,");
        writer.write("Endpoint = maps:get(endpoint, Client),");
        writer.write("Path = <<\"/\">>,");
        writer.write("Url = <<Endpoint/binary, Path/binary>>,");
        writer.writeBlankLine();
        
        // Generate headers
        generateHeaders(operation, context.serviceShape(), writer, context);
        writer.writeBlankLine();
        
        // Generate request body
        generateRequestSerializer(operation, writer, context);
        writer.writeBlankLine();
        
        // Make the request with SigV4 signing
        writer.writeSignAndSendBlock(
            "application/x-amz-json-" + version,
            "Payload",
            "post",
            () -> writer.write("Request = {binary_to_list(Url), StringHeaders, ContentType, Payload},"),
            () -> generateResponseDeserializer(operation, writer, context),
            () -> generateErrorParser(operation, writer, context)
        );
        writer.dedent();
        writer.writeBlankLine();
    }
    
    @Override
    public void generateHeaders(OperationShape operation, ServiceShape service, 
            ErlangWriter writer, ErlangContext context) {
        
        writer.writeComment("AWS JSON protocol headers");
        
        // X-Amz-Target header format: {ServiceName}.{OperationName}
        // The service name (e.g., DynamoDB_20120810) already includes the API version
        String serviceName = service.getId().getName();
        String operationName = operation.getId().getName();
        String target = serviceName + "." + operationName;
        
        writer.write("Headers = [");
        writer.indent();
        writer.write("{<<\"X-Amz-Target\">>, <<\"$L\">>},", target);
        writer.write("{<<\"Content-Type\">>, <<\"application/x-amz-json-$L\">>}", version);
        writer.dedent();
        writer.write("],");
    }
    
    @Override
    public void generateRequestSerializer(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        writer.writeComment("Encode request body as JSON");
        
        if (operation.getInput().isPresent()) {
            writer.write("Payload = jsx:encode(Input),");
        } else {
            writer.write("Payload = <<\"{}\">>,");
        }
    }
    
    @Override
    public void generateResponseDeserializer(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        writer.writeComment("Decode JSON response");
        
        if (operation.getOutput().isPresent()) {
            writer.write("DecodedBody = jsx:decode(ResponseBody, [return_maps]),");
            writer.write("{ok, DecodedBody};");
        } else {
            writer.write("%% No output structure, return empty map");
            writer.write("{ok, #{}};");
        }
    }
    
    @Override
    public void generateErrorParser(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        writer.writeAwsJsonErrorParser(";");
    }
}
