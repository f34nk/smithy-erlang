$version: "2.0"

namespace com.amazonaws.sqs

use aws.protocols#awsQuery

/// Minimal SQS service for testing AWS Query protocol
@awsQuery
@xmlNamespace(uri: "http://sqs.amazonaws.com/doc/2012-11-05/")
service SQS {
    version: "2012-11-05"
    operations: [
        SendMessage
        ReceiveMessage
        DeleteMessage
    ]
}

/// Send a message to an SQS queue
operation SendMessage {
    input: SendMessageRequest
    output: SendMessageResult
}

/// Receive messages from an SQS queue
operation ReceiveMessage {
    input: ReceiveMessageRequest
    output: ReceiveMessageResult
}

/// Delete a message from an SQS queue
operation DeleteMessage {
    input: DeleteMessageRequest
    output: DeleteMessageResult
}

// ============================================================================
// Request/Response structures
// ============================================================================

structure SendMessageRequest {
    @required
    QueueUrl: String
    
    @required
    MessageBody: String
    
    DelaySeconds: Integer
}

structure SendMessageResult {
    MD5OfMessageBody: String
    MessageId: String
}

structure ReceiveMessageRequest {
    @required
    QueueUrl: String
    
    MaxNumberOfMessages: Integer
    
    VisibilityTimeout: Integer
    
    WaitTimeSeconds: Integer
}

structure ReceiveMessageResult {
    Messages: MessageList
}

structure DeleteMessageRequest {
    @required
    QueueUrl: String
    
    @required
    ReceiptHandle: String
}

structure DeleteMessageResult {}

// ============================================================================
// Supporting structures
// ============================================================================

structure Message {
    MessageId: String
    ReceiptHandle: String
    MD5OfBody: String
    Body: String
}

list MessageList {
    member: Message
}
