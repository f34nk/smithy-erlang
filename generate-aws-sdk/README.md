# Generate AWS SDK for Erlang

Generate Erlang client code from official AWS API Smithy models.

## Prerequisites

- [Smithy CLI](https://smithy.io/2.0/guides/smithy-cli/index.html)
- `jq` for JSON parsing
- `smithy-erlang` published to local Maven repository (`../gradlew publishToMavenLocal`)

## Quick Start

```bash
# Setup projects (clones AWS models, creates build configs)
make setup SDKS=s3

# or
make setup SDKS=s3,dynamodb,lambda

# Build all SDKs
make build

# Build a specific SDK
make output/s3
```

## How It Works

1. **setup.sh** clones [aws/api-models-aws](https://github.com/aws/api-models-aws)
2. For each selected SDK (currently `s3`):
   - Parses the Smithy JSON model to extract service metadata
   - Creates `output/<sdk-id>/` with model files and `smithy-build.json`
3. **make build** runs `smithy build` for each SDK project
4. Generated Erlang code lands in `output/<sdk-id>/src/`

## Directory Structure

```
generate-aws-sdk/
├── api-models-aws/     # Cloned AWS models (git ignored)
├── output/             # Generated SDK projects
│   └── s3/
│       ├── model/      # Copied Smithy JSON
│       ├── smithy-build.json
│       └── src/  # Generated Erlang code
├── setup.sh            # Project setup script
└── Makefile            # Build orchestration
```
