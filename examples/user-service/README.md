# User Client Example

A complete example demonstrating Smithy type definitions and their usage in Erlang.

## Model Format

This example uses **Smithy IDL** (`model/user.smithy`) as the source format.

The build process:
1. Converts Smithy IDL to Smithy JSON AST (via `smithy build`)
2. Generates Erlang code from JSON AST (via erlang-client-codegen plugin)

## Prerequisites

- **Java 11+** - For the Erlang code generator
- **Erlang/OTP 24+** - For running generated code
- **Rebar3** - For building and testing
- **Smithy CLI** - For converting IDL to JSON AST
```bash
brew install smithy  # macOS
# or download from https://smithy.io/2.0/guides/smithy-cli/index.html
```

## Quick Start

Make sure to build generator plugin:

```bash
cd smithy-erlang
make
```

Then, generate example code and run tests:

```bash
cd examples/user-service
make
```

## Using the Client

```erlang
%% Create a client
Client = #{endpoint => <<"http://localhost:8080">>}.

%% Get a user
user_client:get_user(Client, #{<<"userId">> => <<"123">>}).

%% Create a user
user_client:create_user(Client, #{
    <<"name">> => <<"Alice">>,
    <<"email">> => <<"alice@example.com">>,
    <<"age">> => 28,
    <<"status">> => <<"active">>
}).
```

## Adding a New Field

Edit `model/user.smithy`:

```smithy
structure User {
    @required userId: String
    @required name: String
    @required email: String
    age: Integer
    status: UserStatus
    createdAt: Timestamp
    updatedAt: Timestamp
    // Add new field:
    phoneNumber: String  // ← New
}
```

## Adding a New Operation

```smithy
/// Delete a user
@idempotent
@http(method: "DELETE", uri: "/users/{userId}")
operation DeleteUser {
    input: DeleteUserInput
    errors: [UserNotFound]
}

structure DeleteUserInput {
    @required
    @httpLabel
    userId: String
}
```

Then regenerate and the new `delete_user/2` function will be available in `user_client.erl`.

## See Also

- [Smithy IDL Documentation](https://smithy.io/2.0/spec/idl.html)
