$version: "2.0"

namespace com.amazonaws.ec2

use aws.protocols#ec2Query

/// Minimal EC2 service for testing EC2 Query protocol
@ec2Query
@xmlNamespace(uri: "http://ec2.amazonaws.com/doc/2016-11-15/")
service EC2 {
    version: "2016-11-15"
    operations: [
        DescribeInstances
        RunInstances
        TerminateInstances
    ]
}

/// Describe EC2 instances
operation DescribeInstances {
    input: DescribeInstancesRequest
    output: DescribeInstancesResult
}

/// Launch new EC2 instances
operation RunInstances {
    input: RunInstancesRequest
    output: RunInstancesResult
}

/// Terminate EC2 instances
operation TerminateInstances {
    input: TerminateInstancesRequest
    output: TerminateInstancesResult
}

// ============================================================================
// Request/Response structures
// ============================================================================

structure DescribeInstancesRequest {
    InstanceIds: InstanceIdList
    
    Filters: FilterList
    
    MaxResults: Integer
    
    NextToken: String
}

structure DescribeInstancesResult {
    Reservations: ReservationList
    
    NextToken: String
}

structure RunInstancesRequest {
    @required
    ImageId: String
    
    MinCount: Integer
    
    MaxCount: Integer
    
    InstanceType: String
    
    KeyName: String
    
    SecurityGroupIds: SecurityGroupIdList
}

structure RunInstancesResult {
    Instances: InstanceList
    
    ReservationId: String
}

structure TerminateInstancesRequest {
    @required
    InstanceIds: InstanceIdList
}

structure TerminateInstancesResult {
    TerminatingInstances: InstanceStateChangeList
}

// ============================================================================
// Supporting structures
// ============================================================================

structure Instance {
    InstanceId: String
    InstanceType: String
    State: InstanceState
    ImageId: String
    LaunchTime: String
}

structure InstanceState {
    Code: Integer
    Name: String
}

structure InstanceStateChange {
    InstanceId: String
    CurrentState: InstanceState
    PreviousState: InstanceState
}

structure Reservation {
    ReservationId: String
    Instances: InstanceList
    OwnerId: String
}

structure Filter {
    Name: String
    Values: StringList
}

list InstanceList {
    member: Instance
}

list InstanceIdList {
    member: String
}

list ReservationList {
    member: Reservation
}

list InstanceStateChangeList {
    member: InstanceStateChange
}

list SecurityGroupIdList {
    member: String
}

list FilterList {
    member: Filter
}

list StringList {
    member: String
}
