package io.smithy.erlang.codegen.aws;

import java.util.Objects;
import java.util.Optional;

/**
 * Contains resolved endpoint information for an AWS service.
 * 
 * This class holds all the information needed to connect to an AWS service endpoint:
 * - The endpoint URL (hostname)
 * - The signing region (may differ from the request region for global services)
 * - The signing name (service identifier used in SigV4)
 * - The DNS suffix for the partition
 */
public final class EndpointInfo {
    
    private final String hostname;
    private final String signingRegion;
    private final String signingName;
    private final String dnsSuffix;
    private final String partition;
    
    private EndpointInfo(Builder builder) {
        this.hostname = Objects.requireNonNull(builder.hostname, "hostname is required");
        this.signingRegion = builder.signingRegion;
        this.signingName = builder.signingName;
        this.dnsSuffix = builder.dnsSuffix;
        this.partition = builder.partition;
    }
    
    /**
     * Returns the resolved endpoint hostname (e.g., "s3.us-east-1.amazonaws.com").
     */
    public String getHostname() {
        return hostname;
    }
    
    /**
     * Returns the full endpoint URL with HTTPS protocol.
     */
    public String getUrl() {
        return "https://" + hostname;
    }
    
    /**
     * Returns the region to use for request signing.
     * May differ from the request region for global services (e.g., IAM uses us-east-1).
     */
    public Optional<String> getSigningRegion() {
        return Optional.ofNullable(signingRegion);
    }
    
    /**
     * Returns the service name to use for request signing.
     */
    public Optional<String> getSigningName() {
        return Optional.ofNullable(signingName);
    }
    
    /**
     * Returns the DNS suffix for the partition (e.g., "amazonaws.com").
     */
    public Optional<String> getDnsSuffix() {
        return Optional.ofNullable(dnsSuffix);
    }
    
    /**
     * Returns the partition identifier (e.g., "aws", "aws-cn", "aws-us-gov").
     */
    public Optional<String> getPartition() {
        return Optional.ofNullable(partition);
    }
    
    /**
     * Creates a new builder for EndpointInfo.
     */
    public static Builder builder() {
        return new Builder();
    }
    
    @Override
    public String toString() {
        return "EndpointInfo{" +
                "hostname='" + hostname + '\'' +
                ", signingRegion='" + signingRegion + '\'' +
                ", signingName='" + signingName + '\'' +
                ", dnsSuffix='" + dnsSuffix + '\'' +
                ", partition='" + partition + '\'' +
                '}';
    }
    
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EndpointInfo that = (EndpointInfo) o;
        return Objects.equals(hostname, that.hostname) &&
               Objects.equals(signingRegion, that.signingRegion) &&
               Objects.equals(signingName, that.signingName) &&
               Objects.equals(dnsSuffix, that.dnsSuffix) &&
               Objects.equals(partition, that.partition);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(hostname, signingRegion, signingName, dnsSuffix, partition);
    }
    
    /**
     * Builder for EndpointInfo.
     */
    public static final class Builder {
        private String hostname;
        private String signingRegion;
        private String signingName;
        private String dnsSuffix;
        private String partition;
        
        private Builder() {}
        
        public Builder hostname(String hostname) {
            this.hostname = hostname;
            return this;
        }
        
        public Builder signingRegion(String signingRegion) {
            this.signingRegion = signingRegion;
            return this;
        }
        
        public Builder signingName(String signingName) {
            this.signingName = signingName;
            return this;
        }
        
        public Builder dnsSuffix(String dnsSuffix) {
            this.dnsSuffix = dnsSuffix;
            return this;
        }
        
        public Builder partition(String partition) {
            this.partition = partition;
            return this;
        }
        
        public EndpointInfo build() {
            return new EndpointInfo(this);
        }
    }
}
