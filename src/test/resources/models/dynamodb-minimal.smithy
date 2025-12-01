$version: "2.0"

namespace com.amazonaws.dynamodb

use aws.protocols#awsJson1_0

/// Minimal DynamoDB service for testing AWS JSON 1.0 protocol
@awsJson1_0
service DynamoDB {
    version: "2012-08-10"
    operations: [
        GetItem
        PutItem
        Query
    ]
}

/// Retrieves a single item from a table
operation GetItem {
    input: GetItemInput
    output: GetItemOutput
    errors: [
        ResourceNotFoundException
    ]
}

/// Creates a new item or replaces an old item
operation PutItem {
    input: PutItemInput
    output: PutItemOutput
    errors: [
        ResourceNotFoundException
    ]
}

/// Queries items in a table
operation Query {
    input: QueryInput
    output: QueryOutput
    errors: [
        ResourceNotFoundException
    ]
}

structure GetItemInput {
    @required
    TableName: String
    
    @required
    Key: AttributeValueMap
    
    ConsistentRead: Boolean
}

structure GetItemOutput {
    Item: AttributeValueMap
}

structure PutItemInput {
    @required
    TableName: String
    
    @required
    Item: AttributeValueMap
    
    ReturnValues: ReturnValue
}

structure PutItemOutput {
    Attributes: AttributeValueMap
}

structure QueryInput {
    @required
    TableName: String
    
    @required
    KeyConditionExpression: String
    
    ExpressionAttributeValues: AttributeValueMap
    
    Limit: Integer
}

structure QueryOutput {
    Items: ItemList
    
    Count: Integer
    
    LastEvaluatedKey: AttributeValueMap
}

/// Map of attribute name to attribute value
map AttributeValueMap {
    key: String
    value: AttributeValue
}

/// List of items
list ItemList {
    member: AttributeValueMap
}

/// DynamoDB attribute value
/// Note: This is a simplified version for testing. The recursive types (M, L) 
/// are omitted to avoid StackOverflowError in the current code generator.
/// Full recursive type support will be added in future iterations.
structure AttributeValue {
    S: String
    N: String
    B: Blob
    SS: StringSet
    NS: NumberSet
    BS: BinarySet
    NULL: Boolean
    BOOL: Boolean
}

list StringSet {
    member: String
}

list NumberSet {
    member: String
}

list BinarySet {
    member: Blob
}

enum ReturnValue {
    NONE
    ALL_OLD
    UPDATED_OLD
    ALL_NEW
    UPDATED_NEW
}

@error("client")
structure ResourceNotFoundException {
    @required
    message: String
}
