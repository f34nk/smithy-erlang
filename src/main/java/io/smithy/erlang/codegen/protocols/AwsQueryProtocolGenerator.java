package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

/**
 * Protocol generator for AWS Query protocol.
 * 
 * <p>This generator handles code generation for services using the AWS Query
 * protocol, which is used by SQS, SNS, RDS, CloudFormation, and other
 * form-encoded AWS services.
 * 
 * <h2>Protocol Characteristics</h2>
 * <ul>
 *   <li>HTTP Method: POST</li>
 *   <li>URI Path: /</li>
 *   <li>Content-Type: {@code application/x-www-form-urlencoded}</li>
 *   <li>Request Body: Form-encoded with Action and Version parameters</li>
 *   <li>Response Body: XML decoded with {@code aws_xml:decode/1}</li>
 *   <li>Authentication: AWS SigV4</li>
 * </ul>
 * 
 * <h2>Request Format</h2>
 * <pre>
 * Action=OperationName&amp;Version=2012-11-05&amp;Param1=Value1&amp;Param2=Value2
 * </pre>
 * 
 * <h2>Response Format</h2>
 * <pre>
 * &lt;OperationNameResponse&gt;
 *     &lt;OperationNameResult&gt;
 *         ...
 *     &lt;/OperationNameResult&gt;
 *     &lt;ResponseMetadata&gt;
 *         &lt;RequestId&gt;...&lt;/RequestId&gt;
 *     &lt;/ResponseMetadata&gt;
 * &lt;/OperationNameResponse&gt;
 * </pre>
 * 
 * <h2>Services Using AWS Query</h2>
 * <ul>
 *   <li>Amazon SQS</li>
 *   <li>Amazon SNS</li>
 *   <li>Amazon RDS</li>
 *   <li>AWS CloudFormation</li>
 *   <li>AWS IAM</li>
 *   <li>AWS STS</li>
 * </ul>
 * 
 * @see ProtocolGenerator
 */
public class AwsQueryProtocolGenerator implements ProtocolGenerator {
    
    /** Protocol trait ID for AWS Query */
    public static final ShapeId AWS_QUERY = ShapeId.from("aws.protocols#awsQuery");
    
    /**
     * Creates an AWS Query protocol generator.
     */
    public AwsQueryProtocolGenerator() {
    }
    
    @Override
    public ShapeId getProtocol() {
        return AWS_QUERY;
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
    public void generateOperation(OperationShape operation, ErlangWriter writer, ErlangContext context) {
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
        writer.writeSpec(opName, "Client :: map(), Input :: " + inputType + ", Options :: map()", "{ok, " + outputType + "} | {error, term()}");
        writer.write("$L(Client, Input, Options) when is_map(Input), is_map(Options) ->", opName);
        writer.indent();
        
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
     * Generates the internal request function.
     */
    private void generateInternalRequestFunction(OperationShape operation, ErlangWriter writer,
            ErlangContext context, String opName, String inputType, String outputType) {
        
        String internalFunctionName = "make_" + opName + "_request";
        
        writer.writeComment("Internal function to make the " + operation.getId().getName() + " request");
        writer.writeSpec(internalFunctionName, "Client :: map(), Input :: " + inputType, "{ok, " + outputType + "} | {error, term()}");
        writer.write("$L(Client, Input) when is_map(Input) ->", internalFunctionName);
        writer.indent();
        
        // AWS Query always uses POST to "/"
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
        generateHttpRequest(operation, writer, context);
        
        writer.dedent();
        writer.writeBlankLine();
    }
    
    @Override
    public void generateHeaders(OperationShape operation, ServiceShape service,
            ErlangWriter writer, ErlangContext context) {
        writer.writeComment("AWS Query protocol headers");
        writer.write("Headers = [");
        writer.indent();
        writer.write("{<<\"Content-Type\">>, <<\"application/x-www-form-urlencoded\">>}");
        writer.dedent();
        writer.write("],");
    }
    
    @Override
    public void generateRequestSerializer(OperationShape operation, ErlangWriter writer, ErlangContext context) {
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
    public void generateResponseDeserializer(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        String operationName = operation.getId().getName();
        String resultKey = operationName + "Result";
        String responseKey = operationName + "Response";
        
        writer.writeComment("Decode XML response");
        
        if (operation.getOutput().isPresent()) {
            writer.write("%% AWS Query response format: <OperationNameResponse><OperationNameResult>...</OperationNameResult></OperationNameResponse>");
            writer.write("case aws_xml:decode(ResponseBody) of");
            writer.indent();
            writer.write("{ok, XmlMap} ->");
            writer.indent();
            writer.write("case XmlMap of");
            writer.indent();
            writer.write("#{<<\"$L\">> := #{<<\"$L\">> := Result}} -> {ok, Result};", responseKey, resultKey);
            writer.write("#{<<\"$L\">> := Response} -> {ok, Response};", responseKey);
            writer.write("_ -> {ok, XmlMap}");
            writer.dedent();
            writer.write("end;");
            writer.dedent();
            writer.write("{error, DecodeError} -> {error, {xml_decode_error, DecodeError}}");
            writer.dedent();
            writer.write("end;");
        } else {
            writer.write("{ok, #{}};");
        }
    }
    
    @Override
    public void generateErrorParser(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        writer.writeAwsQueryErrorParser(";");
    }
    
    /**
     * Generates the HTTP request handling code.
     */
    private void generateHttpRequest(OperationShape operation, ErlangWriter writer, ErlangContext context) {
        writer.writeSignAndSendBlock(
            "application/x-www-form-urlencoded",
            "Payload",
            "post",
            () -> writer.write("Request = {binary_to_list(Url), StringHeaders, ContentType, Payload},"),
            () -> generateResponseDeserializer(operation, writer, context),
            () -> generateErrorParser(operation, writer, context)
        );
    }
}
