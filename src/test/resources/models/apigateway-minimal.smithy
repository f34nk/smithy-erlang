$version: "2.0"

namespace com.amazonaws.apigateway

use aws.protocols#restJson1
use smithy.api#http
use smithy.api#httpLabel
use smithy.api#httpQuery
use smithy.api#httpHeader
use smithy.api#httpPayload
use smithy.api#readonly
use smithy.api#idempotent

/// Minimal API Gateway service for testing REST-JSON protocol
@restJson1
service APIGateway {
    version: "2015-07-09"
    operations: [
        GetRestApi
        CreateRestApi
        DeleteRestApi
        GetResource
        CreateResource
        GetMethod
    ]
}

/// Retrieves information about a REST API
@readonly
@http(method: "GET", uri: "/restapis/{restApiId}")
operation GetRestApi {
    input: GetRestApiRequest
    output: RestApi
}

/// Creates a new REST API
@http(method: "POST", uri: "/restapis")
operation CreateRestApi {
    input: CreateRestApiRequest
    output: RestApi
}

/// Deletes a REST API
@idempotent
@http(method: "DELETE", uri: "/restapis/{restApiId}")
operation DeleteRestApi {
    input: DeleteRestApiRequest
    output: DeleteRestApiResponse
}

/// Retrieves information about a resource
@readonly
@http(method: "GET", uri: "/restapis/{restApiId}/resources/{resourceId}")
operation GetResource {
    input: GetResourceRequest
    output: Resource
}

/// Creates a new resource
@http(method: "POST", uri: "/restapis/{restApiId}/resources/{parentId}")
operation CreateResource {
    input: CreateResourceRequest
    output: Resource
}

/// Retrieves information about a method
@readonly
@http(method: "GET", uri: "/restapis/{restApiId}/resources/{resourceId}/methods/{httpMethod}")
operation GetMethod {
    input: GetMethodRequest
    output: Method
}

// ============================================================================
// Request structures
// ============================================================================

structure GetRestApiRequest {
    @httpLabel
    @required
    restApiId: String
}

structure CreateRestApiRequest {
    @required
    name: String
    
    description: String
    
    @httpQuery("mode")
    mode: String
    
    @httpHeader("X-Api-Key")
    apiKey: String
    
    minimumCompressionSize: Integer
    
    endpointConfiguration: EndpointConfiguration
}

structure DeleteRestApiRequest {
    @httpLabel
    @required
    restApiId: String
}

structure DeleteRestApiResponse {}

structure GetResourceRequest {
    @httpLabel
    @required
    restApiId: String
    
    @httpLabel
    @required
    resourceId: String
}

structure CreateResourceRequest {
    @httpLabel
    @required
    restApiId: String
    
    @httpLabel
    @required
    parentId: String
    
    @required
    pathPart: String
}

structure GetMethodRequest {
    @httpLabel
    @required
    restApiId: String
    
    @httpLabel
    @required
    resourceId: String
    
    @httpLabel
    @required
    httpMethod: String
}

// ============================================================================
// Response/Output structures
// ============================================================================

structure RestApi {
    id: String
    name: String
    description: String
    createdDate: Timestamp
    minimumCompressionSize: Integer
    endpointConfiguration: EndpointConfiguration
    warnings: ListOfString
}

structure Resource {
    id: String
    parentId: String
    pathPart: String
    path: String
    resourceMethods: MapOfMethod
}

structure Method {
    httpMethod: String
    authorizationType: String
    apiKeyRequired: Boolean
    methodResponses: MapOfMethodResponse
}

structure MethodResponse {
    statusCode: String
    responseParameters: MapOfString
    responseModels: MapOfString
}

structure EndpointConfiguration {
    types: ListOfEndpointType
    vpcEndpointIds: ListOfString
}

// ============================================================================
// Collection types
// ============================================================================

list ListOfString {
    member: String
}

list ListOfEndpointType {
    member: EndpointType
}

map MapOfMethod {
    key: String
    value: Method
}

map MapOfMethodResponse {
    key: String
    value: MethodResponse
}

map MapOfString {
    key: String
    value: String
}

// ============================================================================
// Enums
// ============================================================================

enum EndpointType {
    REGIONAL
    EDGE
    PRIVATE
}

// ============================================================================
// Errors
// ============================================================================

@error("client")
structure NotFoundException {
    @required
    message: String
}

@error("client")
structure BadRequestException {
    @required
    message: String
}
