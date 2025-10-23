# AWS STS Simple Example

A simplified AWS Security Token Service client example demonstrating Erlang code generation from Smithy models using the smithy-erlang code generator.

## Overview

This example implements a subset of the AWS STS API with three operations:
- `GetCallerIdentity` - Returns details about the IAM identity used to call the operation
- `AssumeRole` - Returns temporary security credentials for accessing AWS resources
- `GetSessionToken` - Returns temporary session credentials for an AWS account or IAM user

The Smithy model uses AWS traits and the `restJson1` protocol.

## Quick Start

Make sure to build generator plugin:

```bash
cd smithy-erlang
make
```

Then, generate example code and run tests:

```bash
cd examples/aws-sts-simple
make
```

## Using the Client

```erlang
Config = #{endpoint => <<"https://sts.amazonaws.com">>}.
{ok, Client} = sts_client:new(Config).
```

### Get Caller Identity

```erlang
{ok, Result} = sts_client:get_caller_identity(Client, #{}).
```

### Assume Role

```erlang
{ok, Result} = sts_client:assume_role(Client, #{
    <<"RoleArn">> => <<"arn:aws:iam::123456789012:role/demo">>,
    <<"RoleSessionName">> => <<"session-name">>,
    <<"DurationSeconds">> => 3600,
    <<"ExternalId">> => <<"external-id-123">>
}).
```

### Get Session Token

```erlang
{ok, Result} = sts_client:get_session_token(Client, #{
    <<"DurationSeconds">> => 3600,
    <<"SerialNumber">> => <<"arn:aws:iam::123456789012:mfa/user">>,
    <<"TokenCode">> => <<"123456">>
}).
```

## Notes

This is a simplified example for demonstration purposes. A production STS client would require:
- AWS signature v4 authentication
- Request signing with proper credentials
- Retry logic and exponential backoff
- Complete operation set
- MFA device support
- Federation token handling
- SAML assertion processing
