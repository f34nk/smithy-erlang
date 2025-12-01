package io.smithy.erlang.codegen.aws;

/**
 * Enumeration of AWS protocol types supported by the code generator.
 * 
 * Each protocol has unique characteristics for serialization, HTTP methods,
 * and response handling.
 */
public enum AwsProtocol {
    
    /**
     * AWS JSON Protocol version 1.0.
     * Used by: DynamoDB, CloudWatch, Auto Scaling, Application Auto Scaling
     * 
     * Characteristics:
     * - Content-Type: application/x-amz-json-1.0
     * - X-Amz-Target header with ServiceName_Version.OperationName
     * - POST requests to "/"
     * - JSON request/response bodies
     */
    AWS_JSON_1_0("awsJson1_0", "aws.protocols#awsJson1_0"),
    
    /**
     * AWS JSON Protocol version 1.1.
     * Used by: Kinesis, CloudTrail, CodeDeploy, Cognito Identity
     * 
     * Characteristics:
     * - Content-Type: application/x-amz-json-1.1
     * - X-Amz-Target header with ServiceName_Version.OperationName
     * - POST requests to "/"
     * - JSON request/response bodies
     */
    AWS_JSON_1_1("awsJson1_1", "aws.protocols#awsJson1_1"),
    
    /**
     * AWS Query Protocol.
     * Used by: SQS, SNS, RDS, CloudFormation, Elastic Load Balancing
     * 
     * Characteristics:
     * - Content-Type: application/x-www-form-urlencoded
     * - POST requests to "/"
     * - Form-encoded request body with Action and Version parameters
     * - XML response body wrapped in ActionNameResponse element
     */
    AWS_QUERY("awsQuery", "aws.protocols#awsQuery"),
    
    /**
     * EC2 Query Protocol.
     * Used by: EC2, Auto Scaling
     * 
     * Characteristics:
     * - Content-Type: application/x-www-form-urlencoded
     * - POST requests to "/"
     * - Form-encoded request body with Action and Version parameters
     * - XML response body wrapped in ActionNameResponse element
     * - Different error response format than awsQuery
     */
    EC2_QUERY("ec2Query", "aws.protocols#ec2Query"),
    
    /**
     * REST-XML Protocol.
     * Used by: S3, CloudFront, Route 53, SES
     * 
     * Characteristics:
     * - HTTP method determined by operation (@http trait)
     * - URI determined by operation (@http trait)
     * - XML request/response bodies
     * - Supports HTTP bindings (@httpHeader, @httpLabel, @httpQuery, @httpPayload)
     */
    REST_XML("restXml", "aws.protocols#restXml"),
    
    /**
     * REST-JSON Protocol version 1.
     * Used by: API Gateway, Lambda, Step Functions, EventBridge
     * 
     * Characteristics:
     * - HTTP method determined by operation (@http trait)
     * - URI determined by operation (@http trait)
     * - JSON request/response bodies
     * - Supports HTTP bindings (@httpHeader, @httpLabel, @httpQuery, @httpPayload)
     */
    REST_JSON_1("restJson1", "aws.protocols#restJson1");
    
    private final String protocolName;
    private final String traitName;
    
    AwsProtocol(String protocolName, String traitName) {
        this.protocolName = protocolName;
        this.traitName = traitName;
    }
    
    /**
     * Returns the protocol name (e.g., "awsJson1_0", "restXml").
     */
    public String getProtocolName() {
        return protocolName;
    }
    
    /**
     * Returns the Smithy trait name (e.g., "aws.protocols#awsJson1_0").
     */
    public String getTraitName() {
        return traitName;
    }
    
    /**
     * Checks if this is a REST-based protocol.
     */
    public boolean isRestProtocol() {
        return this == REST_XML || this == REST_JSON_1;
    }
    
    /**
     * Checks if this is a JSON-based protocol.
     */
    public boolean isJsonProtocol() {
        return this == AWS_JSON_1_0 || this == AWS_JSON_1_1 || this == REST_JSON_1;
    }
    
    /**
     * Checks if this is an XML-based protocol.
     */
    public boolean isXmlProtocol() {
        return this == AWS_QUERY || this == EC2_QUERY || this == REST_XML;
    }
    
    /**
     * Checks if this is a query-based protocol.
     */
    public boolean isQueryProtocol() {
        return this == AWS_QUERY || this == EC2_QUERY;
    }
    
    @Override
    public String toString() {
        return protocolName;
    }
}
