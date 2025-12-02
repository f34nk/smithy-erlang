$version: "2.0"

namespace com.amazonaws.s3

use aws.protocols#restXml
use smithy.api#http
use smithy.api#httpLabel
use smithy.api#httpQuery
use smithy.api#httpHeader
use smithy.api#httpPayload
use smithy.api#readonly
use smithy.api#idempotent

/// Minimal S3 service for testing REST-XML protocol
@restXml
@xmlNamespace(uri: "http://s3.amazonaws.com/doc/2006-03-01/")
service S3 {
    version: "2006-03-01"
    operations: [
        GetObject
        PutObject
        DeleteObject
        ListBuckets
        CreateBucket
    ]
}

/// Get an object from S3
@readonly
@http(method: "GET", uri: "/{Bucket}/{Key+}")
operation GetObject {
    input: GetObjectRequest
    output: GetObjectResponse
}

/// Put an object to S3
@idempotent
@http(method: "PUT", uri: "/{Bucket}/{Key+}")
operation PutObject {
    input: PutObjectRequest
    output: PutObjectResponse
}

/// Delete an object from S3
@idempotent
@http(method: "DELETE", uri: "/{Bucket}/{Key+}")
operation DeleteObject {
    input: DeleteObjectRequest
    output: DeleteObjectResponse
}

/// List all buckets
@readonly
@http(method: "GET", uri: "/")
operation ListBuckets {
    input: ListBucketsRequest
    output: ListBucketsResponse
}

/// Create bucket with metadata
@idempotent
@http(method: "PUT", uri: "/{Bucket}")
operation CreateBucket {
    input: CreateBucketRequest
    output: CreateBucketResponse
}

// ============================================================================
// Request/Response structures
// ============================================================================

structure GetObjectRequest {
    @httpLabel
    @required
    Bucket: String
    
    @httpLabel
    @required
    Key: String
    
    @httpQuery("versionId")
    VersionId: String
    
    @httpHeader("Range")
    Range: String
    
    @httpHeader("If-Match")
    IfMatch: String
}

structure GetObjectResponse {
    @httpHeader("Content-Type")
    ContentType: String
    
    @httpHeader("ETag")
    ETag: String
    
    @httpPayload
    Body: Blob
}

structure PutObjectRequest {
    @httpLabel
    @required
    Bucket: String
    
    @httpLabel
    @required
    Key: String
    
    @httpHeader("Content-Type")
    ContentType: String
    
    @httpHeader("Content-MD5")
    ContentMD5: String
    
    @httpPayload
    @required
    Body: Blob
}

structure PutObjectResponse {
    @httpHeader("ETag")
    ETag: String
    
    @httpHeader("VersionId")
    VersionId: String
}

structure DeleteObjectRequest {
    @httpLabel
    @required
    Bucket: String
    
    @httpLabel
    @required
    Key: String
    
    @httpQuery("versionId")
    VersionId: String
}

structure DeleteObjectResponse {
    @httpHeader("x-amz-delete-marker")
    DeleteMarker: Boolean
    
    @httpHeader("x-amz-version-id")
    VersionId: String
}

structure ListBucketsRequest {}

structure ListBucketsResponse {
    Buckets: BucketList
    
    Owner: Owner
}

structure CreateBucketRequest {
    @httpLabel
    @required
    Bucket: String
    
    // This member is NOT bound to path/query/header/payload
    // So it will be encoded as XML in the body
    LocationConstraint: String
    
    ACL: String
}

structure CreateBucketResponse {}

// ============================================================================
// Supporting structures
// ============================================================================

structure Bucket {
    Name: String
    CreationDate: Timestamp
}

structure Owner {
    ID: String
    DisplayName: String
}

list BucketList {
    member: Bucket
}
