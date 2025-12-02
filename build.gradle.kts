plugins {
    java
    `maven-publish`
    id("com.github.johnrengelman.shadow") version "8.1.1"
}

group = "io.smithy.erlang"
version = "0.1.0"

repositories {
    mavenCentral()
}

dependencies {
    // Smithy core dependencies
    implementation("software.amazon.smithy:smithy-codegen-core:1.51.0")
    implementation("software.amazon.smithy:smithy-model:1.51.0")
    implementation("software.amazon.smithy:smithy-build:1.51.0")
    implementation("software.amazon.smithy:smithy-utils:1.51.0")
    implementation("software.amazon.smithy:smithy-aws-traits:1.51.0")
    implementation("software.amazon.smithy:smithy-protocol-traits:1.51.0")
    implementation("software.amazon.smithy:smithy-rules-engine:1.51.0")
    implementation("software.amazon.smithy:smithy-waiters:1.51.0")
    implementation("software.amazon.smithy:smithy-smoke-test-traits:1.51.0")
    
    // Testing
    testImplementation("org.junit.jupiter:junit-jupiter:5.10.0")
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.10.0")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.10.0")
    testImplementation("org.hamcrest:hamcrest:2.2")
    
    // AWS traits for test models
    testImplementation("software.amazon.smithy:smithy-aws-traits:1.51.0")
}

java {
    sourceCompatibility = JavaVersion.VERSION_11
    targetCompatibility = JavaVersion.VERSION_11
}

tasks.test {
    useJUnitPlatform()
}

// Configure resource directories for templates
sourceSets {
    main {
        resources {
            srcDir("src/main/resources")
        }
    }
}

// Create the plugin descriptor for Smithy Build
tasks.register("generatePluginDescriptor") {
    doLast {
        val servicesDir = file("$buildDir/resources/main/META-INF/services")
        servicesDir.mkdirs()
        
        file("$servicesDir/software.amazon.smithy.build.SmithyBuildPlugin").writeText(
            "io.smithy.erlang.codegen.ErlangClientPlugin"
        )
    }
}

tasks.named<ProcessResources>("processResources") {
    dependsOn("generatePluginDescriptor")
    duplicatesStrategy = DuplicatesStrategy.INCLUDE
}

// Configure shadow JAR for CLI
tasks.shadowJar {
    archiveBaseName.set("smithy-erlang")
    archiveClassifier.set("cli")  // Add classifier to avoid conflict
    manifest {
        attributes["Main-Class"] = "io.smithy.erlang.codegen.ErlangCodegenCli"
    }
    mergeServiceFiles()
    // Merge Smithy model manifests
    append("META-INF/smithy/manifest")
}

// Make build depend on shadowJar
tasks.build {
    dependsOn(tasks.shadowJar)
}

publishing {
    publications {
        create<MavenPublication>("maven") {
            // Publish only the regular JAR (for use as Smithy Build plugin)
            // Don't include shadow JAR in Maven publication
            from(components["java"])
            
            // Metadata
            groupId = "io.smithy.erlang"
            artifactId = "smithy-erlang"
        }
    }
}

// Ensure shadow JAR doesn't interfere with Maven publication
tasks.withType<PublishToMavenRepository>().configureEach {
    dependsOn(tasks.jar)
    // Don't depend on shadowJar for publishing
}
