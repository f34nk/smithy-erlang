# Smithy Erlang Code Generator

Generates Erlang client code from Smithy service models. Produces client modules, type definitions, and HTTP request/response handling for service operations.

https://smithy.io/2.0/index.html

## "First draft"

I use this project mainly to investigate the pros and cons for using something like Smithy for the Erlang world. 

Smithy is a feature rich, schema-first, type DSL for API modelling in a multi service and multi language environment. 

>What problems does Smithy address that haven't already been solved by existing Erlang tools and patterns? 
>Does a schema-first approach align with—or work against—Erlang's philosophy of dynamic typing and pattern-based dispatching? 
>Rather than introducing friction, could Smithy serve as a complementary external type system that adds strict contracts at scale?

For this draft, basic code generation works, but many Smithy features are not yet implemented. See [IMPLEMENTATION.md](IMPLEMENTATION.md) for detailed feature coverage and limitations.

## Features

- Generates Erlang client and types modules from Smithy models
- Supports core Smithy shapes (structures, lists, maps, primitives)
- Basic restJson1 protocol support with JSON serialization
- Smithy Build plugin integration
- JSON AST file support
- Topological sorting for dependency-ordered type definitions

## Prerequisites

- Java 11+
- Gradle 7.0+
- Erlang/OTP 24+
- Smithy CLI
- rebar3

## Build and Install

```bash
./gradlew clean build publishToMavenLocal
```

## Testing

Run generator tests:

```bash
./gradlew test
```

All examples include test suites using meck for HTTP mocking. Tests verify request construction, response parsing, and error handling without making network calls.


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
- `my_client.erl` - Client API with operation functions
- `my_client_types.erl` - Type records

## Generated Code

### Client Module

```erlang
-module(my_client).
-export([new/1, my_operation/2]).

-spec new(Config :: map()) -> {ok, map()}.
new(Config) -> {ok, Config}.

-spec my_operation(Client :: map(), Input :: map()) -> 
    {ok, map()} | {error, term()}.
my_operation(Client, Input) ->
    % HTTP request with JSON encoding
    ...
```

### Types Module

```erlang
-module(my_client_types).

-record(my_input, {
    field1 :: binary(),
    field2 :: integer()
}).

-record(my_output, {
    result :: binary()
}).
```

## Usage Example

```erlang
% Create client
Config = #{endpoint => <<"https://api.example.com">>}.
{ok, Client} = my_client:new(Config).

% Call operation
Input = #{<<"field1">> => <<"value">>, <<"field2">> => 42}.
{ok, Result} = my_client:my_operation(Client, Input).

% Handle errors
case my_client:my_operation(Client, Input) of
    {ok, Response} -> process(Response);
    {error, {StatusCode, ErrorBody}} -> handle_error(StatusCode, ErrorBody);
    {error, Reason} -> handle_failure(Reason)
end.
```

## Limitations

Key limitations of the current implementation:

- No URI template parameter substitution (@httpLabel ignored)
- HTTP headers and payloads not processed (@httpHeader, @httpPayload ignored)
- No field validation (@required, @range, @length not enforced)
- No AWS authentication or request signing
- Union and enum types treated as basic types
- No pagination, retry logic, or streaming support

See [IMPLEMENTATION.md](IMPLEMENTATION.md) for complete feature coverage and workarounds.

## License

Apache License 2.0
