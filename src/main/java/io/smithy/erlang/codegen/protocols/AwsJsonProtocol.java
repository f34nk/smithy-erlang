package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.ErlangSymbolProvider;
import io.smithy.erlang.codegen.ErlangWriter;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

/**
 * AWS JSON Protocol implementation for DynamoDB, Lambda, Kinesis, and other JSON-based AWS services.
 * 
 * Supports both awsJson1.0 and awsJson1.1 protocols.
 * 
 * Key characteristics:
 * - POST requests to "/"
 * - X-Amz-Target header: {ServiceName}_{Version}.{OperationName}
 * - Content-Type: application/x-amz-json-1.0 or application/x-amz-json-1.1
 * - JSON body encoding/decoding with jsx
 * - AWS SigV4 signing
 */
public class AwsJsonProtocol implements Protocol {
    
    private final String version; // "1.0" or "1.1"
    
    /**
     * Creates an AWS JSON protocol handler.
     * 
     * @param version The AWS JSON version ("1.0" or "1.1")
     */
    public AwsJsonProtocol(String version) {
        this.version = version;
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
    public void generateOperation(
            OperationShape operation,
            ServiceShape service,
            Model model,
            ErlangSymbolProvider symbolProvider,
            ErlangWriter writer) {
        
        String opName = ErlangSymbolProvider.toErlangName(operation.getId().getName());
        
        // Determine input and output types
        String inputType = "map()";
        String outputType = "map()";
        
        if (operation.getInput().isPresent()) {
            ShapeId inputId = operation.getInput().get();
            String inputRecordName = ErlangSymbolProvider.toErlangName(inputId.getName());
            inputType = inputRecordName + "()";
        }
        
        if (operation.getOutput().isPresent()) {
            ShapeId outputId = operation.getOutput().get();
            String outputRecordName = ErlangSymbolProvider.toErlangName(outputId.getName());
            outputType = outputRecordName + "()";
        }
        
        // Generate 2-arity wrapper
        writer.writeComment("Calls the " + operation.getId().getName() + " operation");
        writer.write("-spec $L(Client :: map(), Input :: $L) -> {ok, $L} | {error, term()}.",
                opName, inputType, outputType);
        writer.write("$L(Client, Input) ->", opName);
        writer.indent();
        writer.write("$L(Client, Input, #{}).", opName);
        writer.dedent();
        writer.write("");
        
        // Generate 3-arity version with retry support
        writer.writeComment("Calls the " + operation.getId().getName() + " operation with options");
        writer.writeComment("Options:");
        writer.writeComment("  - enable_retry: Enable automatic retry (default: true)");
        writer.writeComment("  - max_retries: Maximum retry attempts (default: 3)");
        writer.writeComment("  - initial_backoff: Initial backoff in ms (default: 100)");
        writer.writeComment("  - max_backoff: Maximum backoff in ms (default: 20000)");
        writer.write("-spec $L(Client :: map(), Input :: $L, Options :: map()) -> {ok, $L} | {error, term()}.",
                opName, inputType, outputType);
        writer.write("$L(Client, Input, Options) when is_map(Input), is_map(Options) ->", opName);
        writer.indent();
        
        // Create the request function
        String internalFunctionName = "make_" + opName + "_request";
        writer.write("RequestFun = fun() -> $L(Client, Input) end,", internalFunctionName);
        writer.write("");
        
        // Check if retry is enabled
        writer.write("case maps:get(enable_retry, Options, true) of");
        writer.indent();
        writer.write("true ->");
        writer.indent();
        writer.write("%% Retry enabled - wrap with retry logic");
        writer.write("aws_retry:with_retry(RequestFun, Options);");
        writer.dedent();
        writer.write("false ->");
        writer.indent();
        writer.write("%% Retry disabled - call directly");
        writer.write("RequestFun()");
        writer.dedent();
        writer.dedent();
        writer.write("end.");
        writer.dedent();
        writer.write("");
        
        // Generate internal request function
        writer.writeComment("Internal function to make the " + operation.getId().getName() + " request");
        writer.write("-spec $L(Client :: map(), Input :: $L) -> {ok, $L} | {error, term()}.",
                internalFunctionName, inputType, outputType);
        writer.write("$L(Client, Input) when is_map(Input) ->", internalFunctionName);
        writer.indent();
        
        // AWS JSON always uses POST to "/"
        writer.write("Method = <<\"POST\">>,");
        writer.write("Endpoint = maps:get(endpoint, Client),");
        writer.write("Path = <<\"/\">>,");
        writer.write("Url = <<Endpoint/binary, Path/binary>>,");
        writer.write("");
        
        // Generate headers
        generateHeaders(operation, service, writer);
        writer.write("");
        
        // Generate request body
        generateRequestBody(operation, model, writer);
        writer.write("");
        
        // Make the request with SigV4 signing
        writer.write("%% Sign and send request");
        writer.write("case aws_sigv4:sign_request(Client, Method, Url, Headers, Payload) of");
        writer.indent();
        writer.write("{ok, SignedHeaders} ->");
        writer.indent();
        writer.write("ContentType = \"application/x-amz-json-" + version + "\",");
        writer.write("case httpc:request(post, {binary_to_list(Url), SignedHeaders, ContentType, Payload}, [], [{body_format, binary}]) of");
        writer.indent();
        writer.write("{ok, {{_, 200, _}, _RespHeaders, ResponseBody}} ->");
        writer.indent();
        
        // Generate response decoding
        generateResponseDecoding(operation, model, writer);
        
        writer.dedent();
        writer.write("{ok, {{_, StatusCode, _}, _RespHeaders, ErrorBody}} ->");
        writer.indent();
        writer.write("{error, {http_error, StatusCode, ErrorBody}};");
        writer.dedent();
        writer.write("{error, Reason} ->");
        writer.indent();
        writer.write("{error, {http_error, Reason}}");
        writer.dedent();
        writer.dedent();
        writer.write("end;");
        writer.dedent();
        writer.write("{error, SignError} ->");
        writer.indent();
        writer.write("{error, {signing_error, SignError}}");
        writer.dedent();
        writer.dedent();
        writer.write("end.");
        writer.dedent();
        writer.write("");
    }
    
    @Override
    public void generateHeaders(
            OperationShape operation,
            ServiceShape service,
            ErlangWriter writer) {
        
        writer.writeComment("AWS JSON protocol headers");
        
        // Get service name and version
        String serviceName = service.getId().getName();
        String serviceVersion = service.getVersion();
        
        // X-Amz-Target header format: {ServiceName}_{Version}.{OperationName}
        String operationName = operation.getId().getName();
        String target = serviceName + "_" + serviceVersion + "." + operationName;
        
        writer.write("Headers = [");
        writer.indent();
        writer.write("{<<\"X-Amz-Target\">>, <<\"$L\">>},", target);
        writer.write("{<<\"Content-Type\">>, <<\"application/x-amz-json-$L\">>}", version);
        writer.dedent();
        writer.write("],");
    }
    
    @Override
    public void generateRequestBody(
            OperationShape operation,
            Model model,
            ErlangWriter writer) {
        
        writer.writeComment("Encode request body as JSON");
        
        if (operation.getInput().isPresent()) {
            writer.write("Payload = jsx:encode(Input),");
        } else {
            writer.write("Payload = <<\"{}\">>,");
        }
    }
    
    @Override
    public void generateResponseDecoding(
            OperationShape operation,
            Model model,
            ErlangWriter writer) {
        
        writer.writeComment("Decode JSON response");
        
        if (operation.getOutput().isPresent()) {
            writer.write("DecodedBody = jsx:decode(ResponseBody, [return_maps]),");
            writer.write("{ok, DecodedBody};");
        } else {
            writer.write("%% No output structure, return empty map");
            writer.write("{ok, #{}};");
        }
    }
}
