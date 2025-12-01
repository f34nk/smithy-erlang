# Smithy Erlang Code Generator

[![CI](https://github.com/frank-eickhoff/smithy-erlang/actions/workflows/ci.yml/badge.svg)](https://github.com/frank-eickhoff/smithy-erlang/actions/workflows/ci.yml)

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
- restJson1 protocol with JSON serialization/deserialization
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

Run specific example tests:

```bash
make examples/storage-service
make examples/user-service
make examples/paginated-service
```

All examples include comprehensive test suites using meck for HTTP mocking. Tests verify:
- Request construction with proper HTTP bindings
- SigV4 signature calculation
- Retry logic with exponential backoff
- Pagination helper functions
- Union and enum type handling
- Field validation
- Response parsing and error handling


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
      "service": "com.example#MyService",
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
- `my_client.erl` - Single module with types, records, and operation functions
- `aws_sigv4.erl` - AWS Signature V4 request signing
- `aws_credentials.erl` - Credential provider chain
- `aws_retry.erl` - Retry logic with exponential backoff
- `aws_config.erl` - AWS configuration management

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
