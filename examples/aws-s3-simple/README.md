# AWS S3 Simple Example

A simplified Amazon S3 client example demonstrating Erlang code generation from Smithy models using the smithy-erlang code generator.

## Overview

This example implements a subset of the AWS S3 API with three operations:
- `GetObject` - Retrieve an object from a bucket
- `PutObject` - Upload an object to a bucket
- `ListBuckets` - List all available buckets

The Smithy model uses AWS traits and the `restJson1` protocol.

## Quick Start

Make sure to build generator plugin:

```bash
cd smithy-erlang
make
```

Then, generate example code and run tests:

```bash
cd examples/aws-s3-simple
make
```

## Using the Client

```erlang
Config = #{endpoint => <<"https://s3.amazonaws.com">>}.
{ok, Client} = s3_client:new(Config).
```

### List Buckets

```erlang
{ok, Result} = s3_client:list_buckets(Client, #{}).
```

### Get Object

```erlang
{ok, Result} = s3_client:get_object(Client, #{
    <<"Bucket">> => <<"my-bucket">>,
    <<"Key">> => <<"file.txt">>
}).
```

### Put Object

```erlang
{ok, Result} = s3_client:put_object(Client, #{
    <<"Bucket">> => <<"my-bucket">>,
    <<"Key">> => <<"file.txt">>,
    <<"Body">> => <<"content">>,
    <<"ContentType">> => <<"text/plain">>
}).
```

## Notes

This is a simplified example for demonstration purposes. A production S3 client would require:
- AWS signature v4 authentication
- Request signing
- Retry logic and error handling
- Complete operation set
- Streaming support for large objects
- Multipart upload handling
