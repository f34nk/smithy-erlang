# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

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

### Dependencies
- Java 11+
- Smithy 1.51.0
- Erlang/OTP 24+
- rebar3
- jsx (Erlang JSON library)

### Known Issues
- Generated URIs contain literal {placeholder} strings instead of substituted values
- All request data sent in JSON body regardless of HTTP bindings
- No authentication headers added to requests

[Unreleased]: https://github.com/f34nk/smithy-erlang/compare/main...first-draft
