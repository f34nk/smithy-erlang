# Smithy Erlang Architecture

This document describes the architecture of the Smithy Erlang code generator, which follows Smithy's recommended `DirectedCodegen` pattern for extensibility and maintainability.

Reference: [Creating a Code Generator](https://smithy.io/2.0/guides/building-codegen/index.html)

## Overview

The code generator transforms Smithy service models into Erlang client modules. It uses a plugin-based architecture that allows customization at multiple points in the generation process.

```
┌────────────────────────────────────────────────────────────────────┐
│                         Smithy Build                               │
│  ┌───────────────┐    ┌──────────────────┐    ┌─────────────────┐  │
│  │ smithy-build  │───▶│ ErlangCodegen    │───▶│ Generated       │  │
│  │ .json         │    │ Plugin           │    │ .erl files      │  │
│  └───────────────┘    └──────────────────┘    └─────────────────┘  │
└────────────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. ErlangCodegenPlugin

The main entry point that implements `SmithyBuildPlugin`. It orchestrates code generation using `CodegenDirector`.

```java
public final class ErlangCodegenPlugin implements SmithyBuildPlugin {
    @Override
    public void execute(PluginContext context) {
        CodegenDirector<ErlangWriter, ErlangIntegration, ErlangContext, ErlangSettings> director =
            new CodegenDirector<>();
        
        director.directedCodegen(new ErlangGenerator());
        director.integrationClass(ErlangIntegration.class);
        // ... configuration ...
        director.run();
    }
}
```

### 2. ErlangSettings

Immutable configuration object parsed from `smithy-build.json`:

```java
public final class ErlangSettings {
    private final ShapeId service;    // Target service shape
    private final String moduleName;  // Output module name
    private final String outputDir;   // Output directory
    private final String protocol;    // Optional protocol override
    private final String edition;     // Smithy edition
}
```

### 3. ErlangContext

Implements `CodegenContext` to provide access to all generation dependencies:

```java
public final class ErlangContext implements CodegenContext<ErlangSettings, ErlangWriter, ErlangIntegration> {
    private final Model model;
    private final ErlangSettings settings;
    private final SymbolProvider symbolProvider;
    private final FileManifest fileManifest;
    private final List<ErlangIntegration> integrations;
    private final WriterDelegator<ErlangWriter> writerDelegator;
}
```

### 4. ErlangGenerator

Implements `DirectedCodegen` to handle shape-by-shape code generation:

```java
public class ErlangGenerator implements DirectedCodegen<ErlangContext, ErlangSettings, ErlangIntegration> {
    
    @Override
    public SymbolProvider createSymbolProvider(CreateSymbolProviderDirective<ErlangSettings> directive) {
        return new EnhancedErlangSymbolProvider(directive.model(), directive.settings());
    }
    
    @Override
    public void generateService(GenerateServiceDirective<ErlangContext, ErlangSettings> directive) {
        ClientModuleWriter writer = ClientModuleWriter.fromContext(directive.context());
        writer.generate();
        writer.copyRuntimeModules();
    }
    
    @Override
    public void generateStructure(GenerateStructureDirective<ErlangContext, ErlangSettings> directive) {
        // Structure generation logic
    }
    
    // ... other shape generators ...
}
```

### 5. ErlangWriter

Extends `SymbolWriter` for Erlang-specific code generation:

```java
public class ErlangWriter extends SymbolWriter<ErlangWriter, ErlangImportContainer> {
    
    public ErlangWriter(String moduleName) {
        super(new ErlangImportContainer());
        // Register custom formatters
        putFormatter('T', this::formatErlangType);  // $T for types
        putFormatter('N', this::formatErlangName);  // $N for names
    }
    
    public ErlangWriter writeModuleHeader(String moduleName) { ... }
    public ErlangWriter writeExport(String functionName, int arity) { ... }
    public ErlangWriter writeSpec(String name, String returnType) { ... }
    public ErlangWriter writeFunction(String name, String args, Runnable body) { ... }
}
```

### 6. EnhancedErlangSymbolProvider

Implements `SymbolProvider` for Smithy-to-Erlang type mapping:

```java
public final class EnhancedErlangSymbolProvider implements SymbolProvider {
    
    @Override
    public Symbol toSymbol(Shape shape) {
        return shape.accept(new SymbolVisitor());
    }
    
    public static String toErlangName(String name) {
        // Convert PascalCase to snake_case
        return name.replaceAll("([a-z0-9])([A-Z])", "$1_$2").toLowerCase();
    }
}
```

## Integration System

### ErlangIntegration Interface

Extends `SmithyIntegration` for pluggable extensions:

```java
public interface ErlangIntegration 
    extends SmithyIntegration<ErlangSettings, ErlangWriter, ErlangContext> {
    
    default String name() { return getClass().getCanonicalName(); }
    
    default byte priority() { return 0; }
    
    default void preprocessModel(ErlangContext context) {
        // Default: no-op
    }
    
    default void postprocessGeneration(ErlangContext context) {
        // Default: no-op
    }
}
```

### Built-in Integrations

| Integration | Priority | Purpose |
|-------------|----------|---------|
| `AwsSigV4Integration` | 64 | Copies AWS SigV4 signing modules |
| `AwsProtocolIntegration` | 32 | Copies protocol-specific modules |
| `AwsRetryIntegration` | 16 | Copies retry logic module |

### SPI Discovery

Integrations are discovered via Java's `ServiceLoader` mechanism:

```
src/main/resources/META-INF/services/io.smithy.erlang.codegen.ErlangIntegration
```

Contents:
```
io.smithy.erlang.codegen.integrations.AwsSigV4Integration
io.smithy.erlang.codegen.integrations.AwsProtocolIntegration
io.smithy.erlang.codegen.integrations.AwsRetryIntegration
```

## Protocol Generators

### ProtocolGenerator Interface

```java
public interface ProtocolGenerator {
    AwsProtocol getProtocol();
    
    void generateOperation(ErlangContext context, OperationShape operation, ErlangWriter writer);
    void generateRequestSerializer(ErlangContext context, OperationShape operation, ErlangWriter writer);
    void generateResponseDeserializer(ErlangContext context, OperationShape operation, ErlangWriter writer);
}
```

### Available Protocol Generators

| Protocol | Generator Class | Services |
|----------|----------------|----------|
| AWS JSON 1.0/1.1 | `AwsJsonProtocolGenerator` | DynamoDB, Lambda, Kinesis |
| AWS Query | `AwsQueryProtocolGenerator` | SQS, SNS, RDS |
| EC2 Query | `Ec2QueryProtocolGenerator` | EC2, Auto Scaling |
| REST-XML | `RestXmlProtocolGenerator` | S3, CloudFront, Route 53 |
| REST-JSON | `RestJsonProtocolGenerator` | API Gateway, Step Functions |

## Code Generation Flow

```
┌───────────────────────────────────────────────────────────────────┐
│                           Code Generation Flow                    │
├───────────────────────────────────────────────────────────────────┤
│                                                                   │
│  1. Plugin Initialization                                         │
│     ┌──────────────┐    ┌──────────────┐    ┌──────────────┐      │
│     │ Load Model   │───▶│ Parse        │───▶│ Create       │      │
│     │              │    │ Settings     │    │ Context      │      │
│     └──────────────┘    └──────────────┘    └──────────────┘      │
│                                                                   │
│  2. Integration Discovery (SPI)                                   │
│     ┌──────────────┐    ┌──────────────┐    ┌──────────────┐      │
│     │ ServiceLoader│───▶│ Sort by      │───▶│ Apply        │      │
│     │ .load()      │    │ Priority     │    │ preprocessors│      │
│     └──────────────┘    └──────────────┘    └──────────────┘      │
│                                                                   │
│  3. Shape Generation (DirectedCodegen)                            │
│     ┌──────────────┐    ┌──────────────┐    ┌──────────────┐      │
│     │ Service      │───▶│ Structures   │───▶│ Unions/Enums │      │
│     │ generateService│  │ generateStruct│   │ generateEnum │      │
│     └──────────────┘    └──────────────┘    └──────────────┘      │
│                                                                   │
│  4. Protocol-Specific Generation                                  │
│     ┌──────────────┐    ┌──────────────┐    ┌──────────────┐      │
│     │ Detect       │───▶│ Select       │───▶│ Generate     │      │
│     │ Protocol     │    │ Generator    │    │ Operations   │      │
│     └──────────────┘    └──────────────┘    └──────────────┘      │
│                                                                   │
│  5. File Output                                                   │
│     ┌──────────────┐    ┌──────────────┐    ┌──────────────┐      │
│     │ Flush        │───▶│ Copy Runtime │───▶│ Write .erl   │      │
│     │ Writers      │    │ Modules      │    │ Files        │      │
│     └──────────────┘    └──────────────┘    └──────────────┘      │
│                                                                   │
└───────────────────────────────────────────────────────────────────┘
```

## Class Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              Class Relationships                            │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌───────────────────┐         ┌───────────────────┐                        │
│  │ SmithyBuildPlugin │◁────────│ ErlangCodegenPlugin│                       │
│  └───────────────────┘         └─────────┬─────────┘                        │
│                                          │ uses                             │
│                                          ▼                                  │
│  ┌───────────────────┐         ┌───────────────────┐                        │
│  │ DirectedCodegen   │◁────────│ ErlangGenerator   │                        │
│  └───────────────────┘         └─────────┬─────────┘                        │
│                                          │ creates                          │
│                          ┌───────────────┼───────────────┐                  │
│                          ▼               ▼               ▼                  │
│  ┌───────────────────┐  ┌───────────────────┐  ┌───────────────────┐        │
│  │EnhancedErlang     │  │ ErlangContext     │  │ ServiceGenerator  │        │
│  │SymbolProvider     │  │                   │  │                   │        │
│  └───────────────────┘  └───────────────────┘  └───────────────────┘        │
│                                   │                                         │
│                                   │ contains                                │
│                                   ▼                                         │
│  ┌───────────────────┐  ┌───────────────────┐  ┌───────────────────┐        │
│  │ ErlangSettings    │  │ ErlangWriter      │  │ ErlangIntegration │        │
│  └───────────────────┘  └───────────────────┘  └───────────────────┘        │
│                                   △                      △                  │
│                                   │                      │                  │
│                          ┌────────┴────────┐    ┌────────┴─────────┐        │
│                          │ SymbolWriter    │    │ SmithyIntegration│        │
│                          └─────────────────┘    └──────────────────┘        │
│                                                                             │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │                    Protocol Generators                                │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌───────────┐     │  │
│  │  │AwsJson      │  │AwsQuery     │  │RestXml      │  │RestJson   │     │  │
│  │  │Protocol     │  │Protocol     │  │Protocol     │  │Protocol   │     │  │
│  │  │Generator    │  │Generator    │  │Generator    │  │Generator  │     │  │
│  │  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘  └─────┬─────┘     │  │
│  │         └─────────────┬──┴─────────────┬──┴───────────────┘           │  │
│  │                       ▼                                               │  │
│  │              ┌─────────────────┐                                      │  │
│  │              │ProtocolGenerator│ (interface)                          │  │
│  │              └─────────────────┘                                      │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Creating Custom Integrations

### Step 1: Implement ErlangIntegration

```java
package com.example;

import io.smithy.erlang.codegen.ErlangIntegration;
import io.smithy.erlang.codegen.ErlangContext;
import io.smithy.erlang.codegen.ErlangSettings;
import software.amazon.smithy.model.Model;

public class CustomLoggingIntegration implements ErlangIntegration {
    
    @Override
    public String name() {
        return "CustomLogging";
    }
    
    @Override
    public byte priority() {
        return 50; // Higher priority runs earlier
    }
    
    @Override
    public void preprocessModel(ErlangContext context) {
        // Perform setup before generation
        System.out.println("Generating client for: " + context.settings().service());
    }
    
    @Override
    public void postprocessGeneration(ErlangContext context) {
        // Run after generation is complete
        System.out.println("Generation complete!");
    }
}
```

### Step 2: Register via SPI

Create `META-INF/services/io.smithy.erlang.codegen.ErlangIntegration`:

```
com.example.CustomLoggingIntegration
```

### Step 3: Package and Include

Add your JAR to the Smithy build classpath in `smithy-build.json`:

```json
{
  "maven": {
    "dependencies": [
      "io.smithy.erlang:smithy-erlang:0.1.0",
      "com.example:my-integration:1.0.0"
    ]
  }
}
```

## Available Extension Points

| Hook | When Called | Use Case |
|------|-------------|----------|
| `preprocessModel(ErlangContext)` | Before generation | Setup, copying files |
| `postprocessGeneration(ErlangContext)` | After generation | Validation, additional file generation |

## Best Practices

1. **Use high priority for file copying**: Integrations that copy runtime modules should use higher priority (e.g., 64) to ensure files are available before generation.

2. **Keep integrations focused**: Each integration should handle one concern (e.g., SigV4, retry, protocol-specific modules).

3. **Use the context**: Access model, settings, and other dependencies through `ErlangContext` rather than storing references.

4. **Follow naming conventions**: Erlang names should be snake_case, generated via `EnhancedErlangSymbolProvider.toErlangName()`.

5. **Test with golden files**: Use snapshot testing to catch unintended changes in generated code.
