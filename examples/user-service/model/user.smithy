$version: "2.0"

// A simple user management API
//
// This is the source model in Smithy IDL format.
// The build process converts it to JSON AST, which is then used for code generation.

namespace com.example.user

use aws.protocols#restJson1

/// A simple user management service
@restJson1
service UserService {
    version: "2024-01-01"
    operations: [
        GetUser
        CreateUser
        UpdateUser
    ]
}

/// Get a user by ID
@readonly
@http(method: "GET", uri: "/users/{userId}")
operation GetUser {
    input: GetUserInput
    output: GetUserOutput
    errors: [UserNotFound]
}

/// Create a new user
@http(method: "POST", uri: "/users")
operation CreateUser {
    input: CreateUserInput
    output: CreateUserOutput
    errors: [InvalidUserData]
}

/// Update an existing user
@idempotent
@http(method: "PUT", uri: "/users/{userId}")
operation UpdateUser {
    input: UpdateUserInput
    output: UpdateUserOutput
    errors: [UserNotFound, InvalidUserData]
}

// Input/Output structures

structure GetUserInput {
    @required
    @httpLabel
    userId: String
}

structure GetUserOutput {
    @required
    user: User
}

structure CreateUserInput {
    @required
    name: String
    
    @required
    email: String
    
    age: Integer
    
    status: UserStatus
}

structure CreateUserOutput {
    @required
    user: User
    
    @required
    userId: String
}

structure UpdateUserInput {
    @required
    @httpLabel
    userId: String
    
    name: String
    email: String
    age: Integer
    status: UserStatus
}

structure UpdateUserOutput {
    @required
    user: User
}

// Domain types

/// A user in the system
structure User {
    @required
    userId: String
    
    @required
    name: String
    
    @required
    email: String
    
    age: Integer
    
    status: UserStatus
    
    createdAt: Timestamp
    
    updatedAt: Timestamp
}

/// User account status
enum UserStatus {
    ACTIVE = "active"
    INACTIVE = "inactive"
    SUSPENDED = "suspended"
}

// Error types

@error("client")
@httpError(404)
structure UserNotFound {
    @required
    message: String
    
    userId: String
}

@error("client")
@httpError(400)
structure InvalidUserData {
    @required
    message: String
    
    field: String
}
