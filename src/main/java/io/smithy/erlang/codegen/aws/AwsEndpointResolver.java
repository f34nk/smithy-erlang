package io.smithy.erlang.codegen.aws;

import software.amazon.smithy.model.node.ArrayNode;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.node.ObjectNode;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Logger;

/**
 * Resolves AWS service endpoints using the bundled endpoints.json file.
 * 
 * This resolver supports:
 * - Standard AWS regions (aws partition)
 * - China regions (aws-cn partition)
 * - GovCloud regions (aws-us-gov partition)
 * - Global services (IAM, Route53, CloudFront, etc.)
 * - Service-specific endpoint overrides
 * 
 * Usage:
 * <pre>
 * AwsEndpointResolver resolver = new AwsEndpointResolver();
 * EndpointInfo endpoint = resolver.resolveEndpoint("s3", "us-east-1");
 * String url = endpoint.getUrl(); // "https://s3.us-east-1.amazonaws.com"
 * </pre>
 */
public final class AwsEndpointResolver {
    
    private static final Logger LOGGER = Logger.getLogger(AwsEndpointResolver.class.getName());
    private static final String ENDPOINTS_PATH = "aws/endpoints.json";
    
    // Known global services that use a single endpoint regardless of region
    private static final Set<String> GLOBAL_SERVICES = Set.of(
        "iam",
        "route53",
        "cloudfront",
        "waf",
        "organizations",
        "sts"  // STS has both global and regional endpoints
    );
    
    private final Map<String, PartitionInfo> partitions;
    
    /**
     * Creates a new endpoint resolver, loading endpoints.json from resources.
     * 
     * @throws RuntimeException if endpoints.json cannot be loaded
     */
    public AwsEndpointResolver() {
        this.partitions = loadEndpoints();
    }
    
    /**
     * Resolves the endpoint for a service in a specific region.
     * 
     * @param serviceName The AWS service name (e.g., "s3", "dynamodb", "iam")
     * @param region The AWS region (e.g., "us-east-1", "eu-west-1")
     * @return EndpointInfo containing the resolved endpoint URL and signing info
     */
    public EndpointInfo resolveEndpoint(String serviceName, String region) {
        // Determine the partition based on region
        PartitionInfo partition = findPartitionForRegion(region);
        if (partition == null) {
            // Default to aws partition if region not found
            partition = partitions.get("aws");
            if (partition == null) {
                throw new IllegalStateException("No aws partition found in endpoints.json");
            }
        }
        
        String dnsSuffix = partition.dnsSuffix;
        String signingRegion = region;
        String hostname;
        
        // Check for service-specific endpoint configuration
        ServiceInfo serviceInfo = partition.services.get(serviceName);
        
        if (serviceInfo != null) {
            // Check if this service has a specific endpoint for this region
            EndpointConfig regionEndpoint = serviceInfo.endpoints.get(region);
            
            if (regionEndpoint != null && regionEndpoint.hostname != null) {
                // Use the explicit hostname
                hostname = regionEndpoint.hostname;
                
                // Check for credential scope override
                if (regionEndpoint.credentialScope != null) {
                    if (regionEndpoint.credentialScope.region != null) {
                        signingRegion = regionEndpoint.credentialScope.region;
                    }
                }
            } else {
                // Check for global endpoint (e.g., aws-global)
                EndpointConfig globalEndpoint = serviceInfo.endpoints.get("aws-global");
                if (globalEndpoint != null && globalEndpoint.hostname != null) {
                    hostname = globalEndpoint.hostname;
                    if (globalEndpoint.credentialScope != null && 
                        globalEndpoint.credentialScope.region != null) {
                        signingRegion = globalEndpoint.credentialScope.region;
                    }
                } else {
                    // Use service defaults or partition defaults
                    hostname = buildDefaultHostname(serviceName, region, dnsSuffix, 
                                                   serviceInfo.defaults, partition.defaults);
                }
            }
        } else {
            // No service-specific config, use partition defaults
            hostname = buildDefaultHostname(serviceName, region, dnsSuffix, 
                                           null, partition.defaults);
        }
        
        return EndpointInfo.builder()
            .hostname(hostname)
            .signingRegion(signingRegion)
            .signingName(serviceName)
            .dnsSuffix(dnsSuffix)
            .partition(partition.partitionName)
            .build();
    }
    
    /**
     * Checks if a service is a global service (uses a single endpoint).
     * 
     * @param serviceName The AWS service name
     * @return true if the service is global
     */
    public boolean isGlobalService(String serviceName) {
        if (GLOBAL_SERVICES.contains(serviceName.toLowerCase())) {
            return true;
        }
        
        // Also check if the service only has an aws-global endpoint in the partition
        PartitionInfo aws = partitions.get("aws");
        if (aws != null) {
            ServiceInfo serviceInfo = aws.services.get(serviceName);
            if (serviceInfo != null) {
                // If it has aws-global but no regional endpoints, it's global
                if (serviceInfo.endpoints.containsKey("aws-global") && 
                    serviceInfo.endpoints.size() <= 3) { // aws-global, iam, iam-fips are common patterns
                    return true;
                }
            }
        }
        
        return false;
    }
    
    /**
     * Returns the partition for a given region.
     * 
     * @param region The AWS region
     * @return The partition name (e.g., "aws", "aws-cn", "aws-us-gov")
     */
    public String getPartitionForRegion(String region) {
        PartitionInfo partition = findPartitionForRegion(region);
        return partition != null ? partition.partitionName : "aws";
    }
    
    /**
     * Returns the DNS suffix for a partition.
     * 
     * @param partition The partition name (e.g., "aws", "aws-cn")
     * @return The DNS suffix (e.g., "amazonaws.com", "amazonaws.com.cn")
     */
    public String getDnsSuffix(String partition) {
        PartitionInfo info = partitions.get(partition);
        return info != null ? info.dnsSuffix : "amazonaws.com";
    }
    
    /**
     * Gets the signing region for global services.
     * 
     * @param serviceName The service name
     * @return The signing region, or empty if not a global service
     */
    public Optional<String> getGlobalServiceSigningRegion(String serviceName) {
        PartitionInfo aws = partitions.get("aws");
        if (aws != null) {
            ServiceInfo serviceInfo = aws.services.get(serviceName);
            if (serviceInfo != null) {
                EndpointConfig globalEndpoint = serviceInfo.endpoints.get("aws-global");
                if (globalEndpoint != null && globalEndpoint.credentialScope != null) {
                    return Optional.ofNullable(globalEndpoint.credentialScope.region);
                }
            }
        }
        return Optional.empty();
    }
    
    /**
     * Finds the partition that contains the given region.
     */
    private PartitionInfo findPartitionForRegion(String region) {
        for (PartitionInfo partition : partitions.values()) {
            if (partition.regions.contains(region)) {
                return partition;
            }
            // Also check if region matches the partition's regex
            if (partition.regionRegex != null && region.matches(partition.regionRegex)) {
                return partition;
            }
        }
        return null;
    }
    
    /**
     * Builds the default hostname using the hostname template.
     */
    private String buildDefaultHostname(String service, String region, String dnsSuffix,
                                        DefaultsConfig serviceDefaults, DefaultsConfig partitionDefaults) {
        // Get hostname template, preferring service defaults over partition defaults
        String template = "{service}.{region}.{dnsSuffix}";
        
        if (serviceDefaults != null && serviceDefaults.hostname != null) {
            template = serviceDefaults.hostname;
        } else if (partitionDefaults != null && partitionDefaults.hostname != null) {
            template = partitionDefaults.hostname;
        }
        
        // Substitute placeholders
        return template
            .replace("{service}", service)
            .replace("{region}", region)
            .replace("{dnsSuffix}", dnsSuffix);
    }
    
    /**
     * Loads endpoints.json from resources.
     */
    private Map<String, PartitionInfo> loadEndpoints() {
        try (InputStream stream = getClass().getClassLoader().getResourceAsStream(ENDPOINTS_PATH)) {
            if (stream == null) {
                throw new RuntimeException("Cannot find " + ENDPOINTS_PATH + " in resources");
            }
            
            String content = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
            ObjectNode root = Node.parse(content).expectObjectNode();
            ArrayNode partitionsArray = root.expectArrayMember("partitions");
            
            Map<String, PartitionInfo> result = new HashMap<>();
            
            for (Node partitionNode : partitionsArray.getElements()) {
                ObjectNode partition = partitionNode.expectObjectNode();
                PartitionInfo info = parsePartition(partition);
                result.put(info.partitionName, info);
            }
            
            LOGGER.info("Loaded " + result.size() + " partitions from endpoints.json");
            return result;
            
        } catch (IOException e) {
            throw new RuntimeException("Failed to load endpoints.json", e);
        }
    }
    
    /**
     * Parses a partition object from endpoints.json.
     */
    private PartitionInfo parsePartition(ObjectNode partition) {
        PartitionInfo info = new PartitionInfo();
        
        info.partitionName = partition.getStringMemberOrDefault("partition", "aws");
        info.dnsSuffix = partition.getStringMemberOrDefault("dnsSuffix", "amazonaws.com");
        info.regionRegex = partition.getStringMember("regionRegex")
            .map(n -> n.getValue())
            .orElse(null);
        
        // Parse defaults
        partition.getObjectMember("defaults").ifPresent(defaults -> {
            info.defaults = parseDefaults(defaults);
        });
        
        // Parse regions
        partition.getObjectMember("regions").ifPresent(regions -> {
            regions.getMembers().forEach((key, value) -> {
                info.regions.add(key.getValue());
            });
        });
        
        // Parse services
        partition.getObjectMember("services").ifPresent(services -> {
            services.getMembers().forEach((key, value) -> {
                String serviceName = key.getValue();
                ObjectNode serviceNode = value.expectObjectNode();
                ServiceInfo serviceInfo = parseService(serviceNode);
                info.services.put(serviceName, serviceInfo);
            });
        });
        
        return info;
    }
    
    /**
     * Parses a service object from endpoints.json.
     */
    private ServiceInfo parseService(ObjectNode service) {
        ServiceInfo info = new ServiceInfo();
        
        // Parse service defaults
        service.getObjectMember("defaults").ifPresent(defaults -> {
            info.defaults = parseDefaults(defaults);
        });
        
        // Parse endpoints
        service.getObjectMember("endpoints").ifPresent(endpoints -> {
            endpoints.getMembers().forEach((key, value) -> {
                String endpointName = key.getValue();
                ObjectNode endpointNode = value.expectObjectNode();
                EndpointConfig config = parseEndpoint(endpointNode);
                info.endpoints.put(endpointName, config);
            });
        });
        
        return info;
    }
    
    /**
     * Parses defaults from endpoints.json.
     */
    private DefaultsConfig parseDefaults(ObjectNode defaults) {
        DefaultsConfig config = new DefaultsConfig();
        config.hostname = defaults.getStringMember("hostname")
            .map(n -> n.getValue())
            .orElse(null);
        return config;
    }
    
    /**
     * Parses an endpoint configuration from endpoints.json.
     */
    private EndpointConfig parseEndpoint(ObjectNode endpoint) {
        EndpointConfig config = new EndpointConfig();
        config.hostname = endpoint.getStringMember("hostname")
            .map(n -> n.getValue())
            .orElse(null);
        
        endpoint.getObjectMember("credentialScope").ifPresent(scope -> {
            config.credentialScope = new CredentialScope();
            config.credentialScope.region = scope.getStringMember("region")
                .map(n -> n.getValue())
                .orElse(null);
            config.credentialScope.service = scope.getStringMember("service")
                .map(n -> n.getValue())
                .orElse(null);
        });
        
        return config;
    }
    
    // Internal data classes for parsed endpoint data
    
    private static class PartitionInfo {
        String partitionName;
        String dnsSuffix;
        String regionRegex;
        DefaultsConfig defaults;
        Set<String> regions = new java.util.HashSet<>();
        Map<String, ServiceInfo> services = new HashMap<>();
    }
    
    private static class ServiceInfo {
        DefaultsConfig defaults;
        Map<String, EndpointConfig> endpoints = new HashMap<>();
    }
    
    private static class DefaultsConfig {
        String hostname;
    }
    
    private static class EndpointConfig {
        String hostname;
        CredentialScope credentialScope;
    }
    
    private static class CredentialScope {
        String region;
        String service;
    }
}
