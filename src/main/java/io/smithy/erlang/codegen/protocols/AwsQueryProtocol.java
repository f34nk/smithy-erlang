package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.ErlangSymbolProvider;
import io.smithy.erlang.codegen.ErlangWriter;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

/**
 * AWS Query Protocol implementation for SQS, SNS, RDS, CloudFormation, and other Query-based AWS services.
 * 
 * Key characteristics:
 * - POST requests to "/"
 * - Content-Type: application/x-www-form-urlencoded
 * - Body: Form-urlencoded parameters with Action, Version, and request params
 * - Response: XML (decoded with aws_xml)
 * - AWS SigV4 signing
 * 
 * Request format:
 *   Action=OperationName&Version=2012-11-05&Param1=Value1&Param2=Value2
 * 
 * Response format:
 *   &lt;OperationNameResponse&gt;
 *       &lt;OperationNameResult&gt;
 *           ...
 *       &lt;/OperationNameResult&gt;
 *       &lt;ResponseMetadata&gt;
 *           &lt;RequestId&gt;...&lt;/RequestId&gt;
 *       &lt;/ResponseMetadata&gt;
 *   &lt;/OperationNameResponse&gt;
 * 
 * @deprecated Use {@link AwsQueryProtocolGenerator} instead. This class is maintained
 *             for backward compatibility with {@link io.smithy.erlang.codegen.ErlangClientPlugin}.
 * @see AwsQueryProtocolGenerator
 */
@Deprecated
public class AwsQueryProtocol implements Protocol {
    
    /**
     * Creates an AWS Query protocol handler.
     */
    public AwsQueryProtocol() {
    }
    
    @Override
    public String getName() {
        return "awsQuery";
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
        return "application/x-www-form-urlencoded";
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
        
        // AWS Query always uses POST to "/"
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
        writer.write("case aws_sigv4:sign_request(Method, Url, Headers, Payload, Client) of");
        writer.indent();
        writer.write("{ok, SignedHeaders} ->");
        writer.indent();
        writer.write("ContentType = \"application/x-www-form-urlencoded\",");
        writer.write("%% Convert binary headers to string format for httpc");
        writer.write("StringHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- SignedHeaders],");
        writer.write("case httpc:request(post, {binary_to_list(Url), StringHeaders, ContentType, Payload}, [], [{body_format, binary}]) of");
        writer.indent();
        writer.write("{ok, {{_, 200, _}, _RespHeaders, ResponseBody}} ->");
        writer.indent();
        
        // Generate response decoding
        generateResponseDecoding(operation, model, writer);
        
        writer.dedent();
        writer.write("{ok, {{_, StatusCode, _}, _RespHeaders, ErrorBody}} ->");
        writer.indent();
        writer.write("%% Parse AWS Query error response (XML)");
        writer.write("case aws_xml:decode(ErrorBody) of");
        writer.indent();
        writer.write("{ok, ErrorMap} ->");
        writer.indent();
        writer.write("case ErrorMap of");
        writer.indent();
        writer.write("#{<<\"ErrorResponse\">> := #{<<\"Error\">> := Error}} ->");
        writer.indent();
        writer.write("Code = maps:get(<<\"Code\">>, Error, <<\"Unknown\">>),");
        writer.write("Message = maps:get(<<\"Message\">>, Error, <<\"Unknown error\">>),");
        writer.write("{error, {aws_error, StatusCode, Code, Message}};");
        writer.dedent();
        writer.write("#{<<\"Error\">> := Error} ->");
        writer.indent();
        writer.write("%% Some services return Error directly");
        writer.write("Code = maps:get(<<\"Code\">>, Error, <<\"Unknown\">>),");
        writer.write("Message = maps:get(<<\"Message\">>, Error, <<\"Unknown error\">>),");
        writer.write("{error, {aws_error, StatusCode, Code, Message}};");
        writer.dedent();
        writer.write("_ ->");
        writer.indent();
        writer.write("{error, {http_error, StatusCode, ErrorBody}}");
        writer.dedent();
        writer.dedent();
        writer.write("end;");
        writer.dedent();
        writer.write("{error, _} ->");
        writer.indent();
        writer.write("{error, {http_error, StatusCode, ErrorBody}}");
        writer.dedent();
        writer.dedent();
        writer.write("end;");
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
        
        writer.writeComment("AWS Query protocol headers");
        writer.write("Headers = [");
        writer.indent();
        writer.write("{<<\"Content-Type\">>, <<\"application/x-www-form-urlencoded\">>}");
        writer.dedent();
        writer.write("],");
    }
    
    @Override
    public void generateRequestBody(
            OperationShape operation,
            Model model,
            ErlangWriter writer) {
        
        // Get operation name and service version for Action and Version parameters
        String operationName = operation.getId().getName();
        
        writer.writeComment("Encode request body as form-urlencoded with Action parameter");
        writer.write("Action = <<\"$L\">>,", operationName);
        
        if (operation.getInput().isPresent()) {
            writer.write("Payload = aws_query:encode(Action, Input),");
        } else {
            writer.write("Payload = aws_query:encode(Action, #{}),");
        }
    }
    
    @Override
    public void generateResponseDecoding(
            OperationShape operation,
            Model model,
            ErlangWriter writer) {
        
        String operationName = operation.getId().getName();
        String resultKey = operationName + "Result";
        
        writer.writeComment("Decode XML response");
        
        if (operation.getOutput().isPresent()) {
            writer.write("%% AWS Query response format: <OperationNameResponse><OperationNameResult>...</OperationNameResult></OperationNameResponse>");
            String responseKey = operationName + "Response";
            writer.write("case aws_xml:decode(ResponseBody) of");
            writer.indent();
            writer.write("{ok, XmlMap} ->");
            writer.indent();
            writer.write("case XmlMap of");
            writer.indent();
            writer.write("#{<<\"$L\">> := #{<<\"$L\">> := Result}} ->", responseKey, resultKey);
            writer.indent();
            writer.write("{ok, Result};");
            writer.dedent();
            writer.write("#{<<\"$L\">> := Response} ->", responseKey);
            writer.indent();
            writer.write("%% Some operations return result directly in response");
            writer.write("{ok, Response};");
            writer.dedent();
            writer.write("_ ->");
            writer.indent();
            writer.write("%% Return decoded XML as-is if structure doesn't match");
            writer.write("{ok, XmlMap}");
            writer.dedent();
            writer.dedent();
            writer.write("end;");
            writer.dedent();
            writer.write("{error, DecodeError} ->");
            writer.indent();
            writer.write("{error, {xml_decode_error, DecodeError}}");
            writer.dedent();
            writer.dedent();
            writer.write("end;");
        } else {
            writer.write("%% No output structure, return empty map");
            writer.write("{ok, #{}};");
        }
    }
}
