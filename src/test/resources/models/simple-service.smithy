$version: "2.0"

namespace com.example.simple

use aws.protocols#restJson1
use smithy.api#http
use smithy.api#httpLabel
use smithy.api#readonly
use smithy.api#idempotent

/// A simple service for golden file testing
@restJson1
service SimpleService {
    version: "1.0"
    operations: [
        GetItem
        CreateItem
        DeleteItem
    ]
}

/// Retrieves an item by ID
@readonly
@http(method: "GET", uri: "/items/{id}")
operation GetItem {
    input: GetItemRequest
    output: GetItemResponse
}

/// Creates a new item
@http(method: "POST", uri: "/items")
operation CreateItem {
    input: CreateItemRequest
    output: CreateItemResponse
}

/// Deletes an item
@idempotent
@http(method: "DELETE", uri: "/items/{id}")
operation DeleteItem {
    input: DeleteItemRequest
    output: DeleteItemResponse
}

// ============================================================================
// Structures
// ============================================================================

structure GetItemRequest {
    @httpLabel
    @required
    id: String
}

structure GetItemResponse {
    id: String
    name: String
    status: ItemStatus
}

structure CreateItemRequest {
    @required
    name: String
    
    description: String
    
    status: ItemStatus
}

structure CreateItemResponse {
    id: String
    name: String
    status: ItemStatus
}

structure DeleteItemRequest {
    @httpLabel
    @required
    id: String
}

structure DeleteItemResponse {}

// ============================================================================
// Enums
// ============================================================================

enum ItemStatus {
    ACTIVE
    INACTIVE
    PENDING
}
