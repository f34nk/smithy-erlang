package io.smithy.erlang.codegen.protocols;

import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangWriter;
import io.smithy.erlang.codegen.symbol.EnhancedErlangSymbolProvider;
import software.amazon.smithy.model.shapes.OperationShape;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

/**
 * Protocol generator for EC2 Query protocol.
 * 
 * <p>This generator handles code generation for services using the EC2 Query
 * protocol, which is used by EC2, Auto Scaling, and other EC2-based AWS services.
 * EC2 Query is similar to AWS Query but has a different error response format.
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
 * Action=DescribeInstances&amp;Version=2016-11-15&amp;InstanceId.1=i-1234567890abcdef0
 * </pre>
 * 
 * <h2>Response Format</h2>
 * <pre>
 * &lt;DescribeInstancesResponse&gt;
 *     &lt;DescribeInstancesResult&gt;
 *         ...
 *     &lt;/DescribeInstancesResult&gt;
 *     &lt;ResponseMetadata&gt;
 *         &lt;RequestId&gt;...&lt;/RequestId&gt;
 *     &lt;/ResponseMetadata&gt;
 * &lt;/DescribeInstancesResponse&gt;
 * </pre>
 * 
 * <h2>EC2-Specific Error Format</h2>
 * <pre>
 * &lt;Response&gt;
 *     &lt;Errors&gt;
 *         &lt;Error&gt;
 *             &lt;Code&gt;InvalidParameterValue&lt;/Code&gt;
 *             &lt;Message&gt;...&lt;/Message&gt;
 *         &lt;/Error&gt;
 *     &lt;/Errors&gt;
 *     &lt;RequestId&gt;...&lt;/RequestId&gt;
 * &lt;/Response&gt;
 * </pre>
 * 
 * <h2>Services Using EC2 Query</h2>
 * <ul>
 *   <li>Amazon EC2</li>
 *   <li>Amazon Auto Scaling</li>
 *   <li>Amazon ELB (Classic)</li>
 *   <li>Amazon CloudWatch (legacy)</li>
 * </ul>
 * 
 * @see ProtocolGenerator
 * @see AwsQueryProtocolGenerator
 */
public class Ec2QueryProtocolGenerator implements ProtocolGenerator {
    
    /** Protocol trait ID for EC2 Query */
    public static final ShapeId EC2_QUERY = ShapeId.from("aws.protocols#ec2Query");
    
    /**
     * Creates an EC2 Query protocol generator.
     */
    public Ec2QueryProtocolGenerator() {
    }
    
    @Override
    public ShapeId getProtocol() {
        return EC2_QUERY;
    }
    
    @Override
    public String getName() {
        return "ec2Query";
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
        
        // EC2 Query always uses POST to "/"
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
        writer.writeComment("EC2 Query protocol headers");
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
            writer.write("%% EC2 Query response format: <OperationNameResponse><OperationNameResult>...</OperationNameResult></OperationNameResponse>");
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
        writer.write("%% Parse EC2 Query error response (XML)");
        writer.write("%% EC2 error format: <Response><Errors><Error>...</Error></Errors></Response>");
        writer.write("case aws_xml:decode(ErrorBody) of");
        writer.indent();
        writer.write("{ok, #{<<\"Response\">> := #{<<\"Errors\">> := Errors}}} ->");
        writer.indent();
        writer.write("case Errors of");
        writer.indent();
        writer.write("#{<<\"Error\">> := Error} when is_map(Error) ->");
        writer.indent();
        writer.write("Code = maps:get(<<\"Code\">>, Error, <<\"Unknown\">>),");
        writer.write("Message = maps:get(<<\"Message\">>, Error, <<\"Unknown error\">>),");
        writer.write("{error, {aws_error, StatusCode, Code, Message}};");
        writer.dedent();
        writer.write("#{<<\"Error\">> := ErrorList} when is_list(ErrorList) ->");
        writer.indent();
        writer.write("FirstError = lists:nth(1, ErrorList),");
        writer.write("Code = maps:get(<<\"Code\">>, FirstError, <<\"Unknown\">>),");
        writer.write("Message = maps:get(<<\"Message\">>, FirstError, <<\"Unknown error\">>),");
        writer.write("{error, {aws_error, StatusCode, Code, Message}};");
        writer.dedent();
        writer.write("_ -> {error, {http_error, StatusCode, ErrorBody}}");
        writer.dedent();
        writer.write("end;");
        writer.dedent();
        writer.write("{ok, _} -> {error, {http_error, StatusCode, ErrorBody}};");
        writer.write("{error, _} -> {error, {http_error, StatusCode, ErrorBody}}");
        writer.dedent();
        writer.write("end;");
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
