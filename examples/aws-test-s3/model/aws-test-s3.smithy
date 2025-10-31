$version: "2.0"

namespace com.example.s3.test

/// Minimal S3 subset for testing AWS features
service S3Test {
    version: "2024-10-31"
    operations: [
        GetObject
        PutObject
        ListBuckets
        ListObjects
        CreateMetadata
        Echo
    ]
}

/// Get an object from S3
@http(method: "GET", uri: "/{Bucket}/{Key}")
@readonly
operation GetObject {
    input: GetObjectInput
    output: GetObjectOutput
    errors: [
        NoSuchKey
        NoSuchBucket
    ]
}

structure GetObjectInput {
    @required
    @httpLabel
    Bucket: String
    
    @required
    @httpLabel
    Key: String
    
    @httpHeader("If-Match")
    IfMatch: String
    
    @httpHeader("Range")
    Range: String
}

structure GetObjectOutput {
    @httpPayload
    Body: Blob
    
    @httpHeader("Content-Type")
    ContentType: String
    
    @httpHeader("ETag")
    ETag: String
    
    @httpHeader("X-Content-Length")
    ContentLength: Long
}

/// Put an object into S3
@http(method: "PUT", uri: "/{Bucket}/{Key}")
@idempotent
operation PutObject {
    input: PutObjectInput
    output: PutObjectOutput
    errors: [
        NoSuchBucket
    ]
}

structure PutObjectInput {
    @required
    @httpLabel
    Bucket: String
    
    @required
    @httpLabel
    Key: String
    
    @httpPayload
    @required
    Body: Blob
    
    @httpHeader("Content-Type")
    ContentType: String
    
    @httpHeader("Cache-Control")
    CacheControl: String
}

structure PutObjectOutput {
    @httpHeader("ETag")
    ETag: String
}

/// List all buckets
@http(method: "GET", uri: "/")
@readonly
operation ListBuckets {
    input: ListBucketsInput
    output: ListBucketsOutput
}

structure ListBucketsInput {}

structure ListBucketsOutput {
    Buckets: BucketList
}

/// List objects in a bucket
@http(method: "GET", uri: "/{Bucket}")
@readonly
operation ListObjects {
    input: ListObjectsInput
    output: ListObjectsOutput
}

structure ListObjectsInput {
    @required
    @httpLabel
    Bucket: String
    
    @httpQuery("prefix")
    Prefix: String
    
    @httpQuery("max-keys")
    MaxKeys: Integer
    
    @httpQuery("delimiter")
    Delimiter: String
}

structure ListObjectsOutput {
    Objects: ObjectList
    
    @httpHeader("X-Amz-Request-Id")
    RequestId: String
}

list ObjectList {
    member: S3Object
}

structure S3Object {
    Key: String
    Size: Long
    ETag: String
}

list BucketList {
    member: Bucket
}

structure Bucket {
    Name: String
    CreationDate: Timestamp
}

@error("client")
@httpError(404)
structure NoSuchKey {
    @required
    Message: String
    
    Key: String
}

/// Create metadata (tests structure payload)
@http(method: "POST", uri: "/metadata")
operation CreateMetadata {
    input: CreateMetadataInput
    output: CreateMetadataOutput
}

structure CreateMetadataInput {
    @httpPayload
    @required
    Metadata: MetadataDocument
}

structure MetadataDocument {
    Title: String
    Author: String
    Tags: TagList
}

list TagList {
    member: String
}

structure CreateMetadataOutput {
    Id: String
}

/// Echo text (tests string payload)
@http(method: "POST", uri: "/echo")
operation Echo {
    input: EchoInput
    output: EchoOutput
}

structure EchoInput {
    @httpPayload
    @required
    Text: String
}

structure EchoOutput {
    @httpPayload
    Echo: String
}

@error("client")
@httpError(404)
structure NoSuchBucket {
    @required
    Message: String
    
    BucketName: String
}
