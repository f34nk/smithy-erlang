# Changelog

## [Unreleased] - AWS Protocol Support Complete

### Added
- REST-JSON protocol implementation for API Gateway-style services
- All 5 AWS protocols now supported (awsJson1.0, awsJson1.1, awsQuery, ec2Query, restXml, restJson1)

## [Unreleased] - AWS SDK Support

### Added
- AWS SigV4 request signing with canonical request generation, string-to-sign, and signature calculation
- Credential provider chain with environment variables, config files, and credential providers
- Retry logic with exponential backoff, configurable max retries, and jitter
- Pagination support with automatic helper function generation for paginated operations
- Union types as tagged tuples with encoding and decoding functions
- Enum types as atoms with validation and encoding/decoding
- Field validation for @required trait
- HTTP protocol bindings: @httpLabel (path parameters), @httpHeader, @httpQuery, @httpPayload
- URI template parser with proper parameter substitution
- Single-module code generation (types, records, and functions in one file)
- Type aliases in function specs for documentation while using maps at runtime
- Dialyzer suppression for intentional type spec mismatches
- Four example services: user-service, storage-service, paginated-service, aws-s3-client
- Comprehensive test coverage with 199+ tests across examples
- GitHub CI/CD workflow

### Changed
- Refactored from separate header files to single-module approach
- Records now only in type specs, maps used at runtime
- Improved error handling with detailed validation messages

### Limitations
- No streaming support for large payloads
- No waiters support
- No presigned URL generation
- Field validation limited to @required trait (@range, @length not enforced)
- No support for AWS-specific traits beyond SigV4

### Dependencies
- Java 11+
- Smithy 1.51.0
- Erlang/OTP 24+
- rebar3
- jsx (Erlang JSON library)

### Fixed Issues
- URI template parameters now properly substituted
- HTTP bindings respected (headers, query params, payloads)
- AWS SigV4 authentication headers generated correctly

## [Unreleased] - First Draft

### Added
- Initial release of smithy-erlang code generator
- Service definition code generation
- Operation code generation with HTTP method support
- Structure type generation with record definitions
- Topological sorting of type dependencies (prevents forward references)
- Basic restJson1 protocol support
- JSON serialization/deserialization using jsx
- HTTP client implementation using Erlang httpc
- Smithy Build plugin integration
- ServiceLoader discovery mechanism
- Client initialization with endpoint configuration
- Basic error handling with {ok, Result} | {error, Reason} tuples
- Example projects: aws-s3-simple, aws-sts-simple, user-service
- Comprehensive test suites using meck for HTTP mocking

### Limitations
- No URI template parameter substitution (@httpLabel ignored)
- No HTTP header binding (@httpHeader ignored)
- No query parameter binding (@httpQuery ignored)
- No payload binding (@httpPayload ignored)
- No AWS authentication (Signature V4)
- Union and enum types treated as basic types
- No field validation (@required, @range, @length not enforced)
- No pagination support
- No retry logic
- No streaming support
