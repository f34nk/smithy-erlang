$version: "2.0"

namespace com.amazonaws.s3

use aws.api#service
use aws.protocols#restJson1

/// A simplified S3 service example for demonstration
@title("Amazon Simple Storage Service (Simplified Example)")
@restJson1
@service(
    sdkId: "S3"
    arnNamespace: "s3"
    cloudFormationName: "S3"
)
service AmazonS3 {
    version: "2006-03-01"
    operations: [
        GetObject
        PutObject
        ListBuckets
    ]
}

/// Retrieves an object from Amazon S3
@readonly
@http(method: "GET", uri: "/{Bucket}/{Key}")
operation GetObject {
    input: GetObjectRequest
    output: GetObjectOutput
    errors: [NoSuchKey]
}

/// Adds an object to a bucket
@idempotent
@http(method: "PUT", uri: "/{Bucket}/{Key}")
operation PutObject {
    input: PutObjectRequest
    output: PutObjectOutput
}

/// Returns a list of all buckets owned by the authenticated sender
@readonly
@http(method: "GET", uri: "/")
operation ListBuckets {
    input: ListBucketsRequest
    output: ListBucketsOutput
}

structure GetObjectRequest {
    /// The bucket name
    @required
    @httpLabel
    Bucket: String

    /// The object key
    @required
    @httpLabel
    Key: String
}

structure GetObjectOutput {
    /// Object data
    @httpPayload
    Body: Blob

    /// MIME type
    @httpHeader("Content-Type")
    ContentType: String

    /// Date and time the object was last modified
    @httpHeader("Last-Modified")
    LastModified: Timestamp
}

structure PutObjectRequest {
    @required
    @httpLabel
    Bucket: String

    @required
    @httpLabel
    Key: String

    @httpPayload
    Body: Blob

    @httpHeader("Content-Type")
    ContentType: String
}

structure PutObjectOutput {
    /// Entity tag for the uploaded object
    @httpHeader("ETag")
    ETag: String
}

structure ListBucketsRequest {}

structure ListBucketsOutput {
    /// The list of buckets
    Buckets: BucketList
}

list BucketList {
    member: Bucket
}

structure Bucket {
    /// The name of the bucket
    Name: String

    /// Date the bucket was created
    CreationDate: Timestamp
}

/// The specified key does not exist
@error("client")
@httpError(404)
structure NoSuchKey {
    message: String
}
