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
    "erlang-codegen": {
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
- `aws_config.erl` - AWS configuration management
- `aws_sigv4.erl` - AWS Signature V4 request signing
- `aws_credentials.erl` - Credential provider chain
- `aws_retry.erl` - Retry logic with exponential backoff
- `aws_xml.erl` - XML parsing for REST-XML protocol
- `aws_query.erl` - AWS Query protocol support
- `aws_s3.erl` - S3-specific URL routing utilities
- `aws_endpoints.erl` - Endpoint resolution and AWS region support

## Architecture

The generator follows Smithy's recommended `DirectedCodegen` pattern for extensibility and maintainability.

Reference: [Creating a Code Generator](https://smithy.io/2.0/guides/building-codegen/index.html)

### Core Components

| Component | Description |
|-----------|-------------|
| `ErlangCodegenPlugin` | Main Smithy Build plugin entry point |
| `ErlangGenerator` | DirectedCodegen implementation for shape-by-shape generation |
| `ErlangContext` | Centralized access to model, settings, and dependencies |
| `ErlangSettings` | Immutable configuration from smithy-build.json |
| `ErlangWriter` | SymbolWriter extension for Erlang code output |
| `EnhancedErlangSymbolProvider` | Smithy-to-Erlang type mapping |

### Extension System

The generator supports custom integrations via Java SPI (Service Provider Interface):

```java
public class CustomIntegration implements ErlangIntegration {
    @Override
    public String name() { return "CustomIntegration"; }
    
    @Override
    public void preprocessModel(ErlangContext context) {
        // Copy runtime modules or perform setup before generation
    }
    
    @Override
    public void postprocessGeneration(ErlangContext context) {
        // Run after generation completes
    }
}
```

Register in `META-INF/services/io.smithy.erlang.codegen.ErlangIntegration`:
```
com.example.CustomIntegration
```

### Built-in Integrations

| Integration | Purpose |
|-------------|---------|
| `AwsSigV4Integration` | Copies AWS SigV4 signing modules |
| `AwsProtocolIntegration` | Copies protocol-specific runtime modules |
| `AwsRetryIntegration` | Copies retry logic module |

See [ARCHITECTURE.md](https://github.com/f34nk/smithy-erlang/blob/main/ARCHITECTURE.md) for detailed architecture documentation.

## Smithy Traits

[Smithy traits](https://smithy.io/2.0/spec/model.html#traits) are declarative metadata that tell code generators how to generate code, without embedding that logic in the model itself. They separate "what the API looks like" from "how to implement it".

Smithy-erlang **reads and uses** built-in traits via Java's Smithy libraries. Please check out [TRAITS.md](https://github.com/f34nk/smithy-erlang/blob/main/TRAITS.md) with all Smithy traits and their support status in smithy-erlang.

## AWS SDK Feature Support

### ✅ Fully Implemented

| Feature | Description | Status |
|---------|-------------|--------|
| **AWS Protocols** | awsJson1.0, awsJson1.1, awsQuery, ec2Query, restXml, restJson1 | ✅ Complete |
| **SigV4 Signing** | Canonical request, string-to-sign, signature calculation | ✅ Complete |
| **Credentials** | Environment variables, `~/.aws/credentials`, provider chain | ✅ Complete |
| **Retry Logic** | Exponential backoff with jitter, configurable attempts | ✅ Complete |
| **Pagination** | Automatic helper function generation for paginated operations | ✅ Complete |
| **HTTP Bindings** | `@httpLabel`, `@httpHeader`, `@httpQuery`, `@httpPayload` | ✅ Complete |
| **Endpoint Resolution** | Static endpoints via `endpoints.json`, region support | ✅ Complete |
| **Basic Validation** | `@required` trait validation | ✅ Complete |

### ⚠️ Partially Implemented / Limitations

| Feature | Current State | Limitation |
|---------|---------------|------------|
| **Service Trait Extraction** | Uses hardcoded trait detection | Does not dynamically extract `@aws.api#service` trait metadata |
| **Error Handling** | Basic error parsing | Service-specific error types not fully modeled |
| **Documentation** | Function specs only | `@documentation` trait not yet extracted to EDoc |

### ❌ Not Yet Implemented

| Feature | Priority | Description |
|---------|----------|-------------|
| **Streaming** | High | `@streaming` trait for large payload uploads/downloads |
| **Presigned URLs** | High | Generate shareable URLs without exposing credentials |
| **Waiters** | Medium | `@waitable` trait for polling long-running operations |
| **S3 Multipart** | Medium | Large file uploads (>5GB) with concurrent parts |
| **Advanced Validation** | Medium | `@range`, `@length`, `@pattern` constraint traits |
| **Request Compression** | Low | `@requestCompression` trait for gzip encoding |
| **Endpoint Discovery** | Low | `@clientEndpointDiscovery` dynamic endpoint lookup |
| **Account ID Routing** | Low | `@accountIdEndpointMode` account-scoped endpoints |
| **Cross-Region Routing** | Low | S3 access points, bucket region detection |

### Type System Trade-offs

The generator uses type aliases in function specs for documentation while using maps at runtime:

```erlang
-type get_object_input() :: #{
    <<"Bucket">> := binary(),
    <<"Key">> := binary()
}.

-spec get_object(Client :: map(), Input :: get_object_input()) -> 
    {ok, get_object_output()} | {error, term()}.
```

This provides:
- Clear documentation via type names
- Zero runtime overhead (no record conversion)
- Dialyzer warnings suppressed where specs intentionally differ from implementation

## License

Apache License 2.0
