$version: "2.0"

namespace com.amazonaws.sts

use aws.api#service
use aws.protocols#restJson1

/// A simplified AWS STS service for obtaining temporary security credentials
@title("AWS Security Token Service (Simplified)")
@restJson1
@service(
    sdkId: "STS"
    arnNamespace: "sts"
    cloudFormationName: "STS"
    cloudTrailEventSource: "sts.amazonaws.com"
)
service AWSSecurityTokenService {
    version: "2011-06-15"
    operations: [
        GetCallerIdentity
        AssumeRole
        GetSessionToken
    ]
}

/// Returns details about the IAM user or role whose credentials are used to call the operation
@readonly
@http(method: "GET", uri: "/caller-identity")
operation GetCallerIdentity {
    input: GetCallerIdentityRequest
    output: GetCallerIdentityResponse
}

/// Returns a set of temporary security credentials that you can use to access AWS resources
@http(method: "POST", uri: "/assume-role")
operation AssumeRole {
    input: AssumeRoleRequest
    output: AssumeRoleResponse
    errors: [
        MalformedPolicyDocumentException
        PackedPolicyTooLargeException
    ]
}

/// Returns a set of temporary credentials for an AWS account or IAM user
@http(method: "POST", uri: "/session-token")
operation GetSessionToken {
    input: GetSessionTokenRequest
    output: GetSessionTokenResponse
}

/// Input for GetCallerIdentity
structure GetCallerIdentityRequest {}

/// Response from GetCallerIdentity
structure GetCallerIdentityResponse {
    /// The unique identifier of the calling entity
    UserId: String

    /// The AWS account ID number of the account that owns or contains the calling entity
    Account: String

    /// The AWS ARN associated with the calling entity
    Arn: String
}

/// Input for AssumeRole
structure AssumeRoleRequest {
    /// The ARN of the role to assume
    @required
    RoleArn: String

    /// An identifier for the assumed role session
    @required
    RoleSessionName: String

    /// The ARNs of IAM managed policies to use as a session policy
    PolicyArns: PolicyDescriptorList

    /// An IAM policy in JSON format
    Policy: String

    /// The duration, in seconds, of the role session
    DurationSeconds: Integer

    /// A unique identifier used by third parties when assuming roles in customer accounts
    ExternalId: String
}

/// Response from AssumeRole
structure AssumeRoleResponse {
    /// The temporary security credentials
    Credentials: Credentials

    /// The ARN and the assumed role ID
    AssumedRoleUser: AssumedRoleUser
}

/// Input for GetSessionToken
structure GetSessionTokenRequest {
    /// The duration, in seconds, that the credentials should remain valid
    DurationSeconds: Integer

    /// The identification number of the MFA device
    SerialNumber: String

    /// The value provided by the MFA device
    TokenCode: String
}

/// Response from GetSessionToken
structure GetSessionTokenResponse {
    /// The temporary security credentials
    Credentials: Credentials
}

/// AWS credentials for API authentication
structure Credentials {
    /// The access key ID that identifies the temporary security credentials
    @required
    AccessKeyId: String

    /// The secret access key
    @required
    SecretAccessKey: String

    /// The token that users must pass to the service API
    @required
    SessionToken: String

    /// The date on which the current credentials expire
    @required
    Expiration: Timestamp
}

/// The identifiers for the temporary security credentials
structure AssumedRoleUser {
    /// A unique identifier that contains the role ID and the role session name
    @required
    AssumedRoleId: String

    /// The ARN of the temporary security credentials
    @required
    Arn: String
}

/// A list of policy descriptors
list PolicyDescriptorList {
    member: PolicyDescriptorType
}

/// A reference to the IAM managed policy
structure PolicyDescriptorType {
    /// The ARN of the managed policy
    arn: String
}

/// The request was rejected because the policy document was malformed
@error("client")
@httpError(400)
structure MalformedPolicyDocumentException {
    message: String
}

/// The request was rejected because the total packed size of the session policies exceeds the limit
@error("client")
@httpError(400)
structure PackedPolicyTooLargeException {
    message: String
}
