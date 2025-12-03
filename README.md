# WORK IN PROGRESS

# Smithy Erlang Code Generator

[![CI](https://github.com/f34nk/smithy-erlang/actions/workflows/ci.yml/badge.svg)](https://github.com/f34nk/smithy-erlang/actions/workflows/ci.yml)

Generates Erlang client code from Smithy service models. Produces client modules, type definitions, and HTTP request/response handling for service operations.

https://smithy.io/2.0/index.html

## Features

### Core Code Generation
- Single-module code generation with types, records, and functions
- Type aliases in function specs for documentation
- Supports core Smithy shapes (structures, lists, maps, primitives, unions, enums)
- Union types as tagged tuples with encoding/decoding
- Enum types as atoms with validation
- Topological sorting for dependency-ordered type definitions
- Smithy Build plugin integration
- JSON AST file support

### Protocol Support
- All 5 AWS protocols supported:
  - awsJson1.0 and awsJson1.1 (DynamoDB, Lambda, Kinesis)
  - awsQuery (SQS, SNS, RDS)
  - ec2Query (EC2)
  - restXml (S3, CloudFront, Route 53)
  - restJson1 (API Gateway, Step Functions)
- HTTP protocol bindings: @httpLabel, @httpHeader, @httpQuery, @httpPayload
- URI template parsing and parameter substitution
- Field validation for @required trait

### AWS SDK Support
- AWS SigV4 request signing
- Credential provider chain (environment variables, config files, credential providers)
- Retry logic with exponential backoff and jitter
- Pagination with automatic helper function generation
- Region configuration support

### Testing and Quality
- Comprehensive test coverage with meck for HTTP mocking
- Dialyzer static analysis with managed suppressions
- GitHub CI/CD workflow

## Prerequisites

- Java 11+
- Gradle 7.0+
- Erlang/OTP 24+
- Smithy CLI
- rebar3

## Build and Install

```bash
make build
```

## Testing

Run generator tests:

```bash
make test
```

Run all example builds and tests:

```bash
make examples
```
See [examples](https://github.com/f34nk/smithy-erlang/tree/main/examples)

Run end to end demo:

```bash
make demo
```

The demo creates a mocked AWS S3 bucket using [moto](https://github.com/getmoto/moto) and executes functions from the generated `s3_client`.

See [aws-demo/src/aws_demo_app.erl](https://github.com/f34nk/smithy-erlang/blob/main/examples/aws-demo/src/aws_demo_app.erl)

## Basic Usage

Create `smithy-build.json`:

```json
{
  "version": "1.0",
  "sources": ["model"],
  "maven": {
    "dependencies": ["io.smithy.erlang:smithy-erlang:0.1.0"],
    "repositories": [{"url": "file://${user.home}/.m2/repository"}]
  },
  "plugins": {
    "erlang-client-codegen": {
      "service": "com.example#MyClient",
      "module": "my_client",
      "outputDir": "src/generated"
    }
  }
}
```

Run generation:

```bash
smithy build
```

Generated files in `src/generated/`:
- `my_client.erl` - A single module with types, records, and operation functions (generated from the smithy model)
- `aws_sigv4.erl` - AWS Signature V4 request signing
- `aws_credentials.erl` - Credential provider chain
- `aws_retry.erl` - Retry logic with exponential backoff
- `aws_config.erl` - AWS configuration management
- `aws_xml.erl` - XML parsing for REST-XML protocol
- `aws_query.erl` - AWS Query protocol support
- `aws_s3.erl` - S3-specific URL routing utilities

## AWS SDK Support Status

### Implemented Features
- AWS SigV4 request signing (canonical request, string-to-sign, signature calculation)
- Credential provider chain (environment variables, credential files, provider chain)
- Region configuration from environment and config files
- Retry logic with exponential backoff and configurable jitter
- Pagination with automatic helper function generation
- HTTP protocol bindings (@httpLabel, @httpHeader, @httpQuery, @httpPayload)
- URI template parameter substitution
- Field validation for @required trait
- Union types as tagged tuples
- Enum types as atoms with validation

### Missing AWS Features
- Streaming support for large payloads
- Waiters for long-running operations
- Presigned URL generation
- S3 multipart upload
- Advanced field validation (@range, @length, @pattern)
- AWS-specific traits (endpoint discovery, account ID routing)
- Cross-region request routing
- Request compression

### Type System Trade-offs
The generator uses type aliases in function specs for documentation while using maps at runtime. This provides:
- Clear documentation via type names
- Zero runtime overhead (no record conversion)
- Dialyzer warnings suppressed where specs intentionally differ from implementation

## License

Apache License 2.0
