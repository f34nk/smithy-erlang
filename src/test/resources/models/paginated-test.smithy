$version: "2.0"

namespace com.example.pagination

/// Test service with paginated operations
service PaginatedTestService {
    version: "2024-01-01"
    operations: [
        ListItems
        ListItemsWithPageSize
    ]
}

/// Paginated list operation
@paginated(
    inputToken: "nextToken"
    outputToken: "nextToken"
    items: "items"
)
@http(method: "GET", uri: "/items")
@readonly
operation ListItems {
    input: ListItemsInput
    output: ListItemsOutput
}

/// Paginated list operation with page size
@paginated(
    inputToken: "nextToken"
    outputToken: "nextToken"
    pageSize: "maxResults"
    items: "items"
)
@http(method: "GET", uri: "/items-sized")
@readonly
operation ListItemsWithPageSize {
    input: ListItemsWithPageSizeInput
    output: ListItemsWithPageSizeOutput
}

structure ListItemsInput {
    /// Token for retrieving the next page
    @httpQuery("nextToken")
    nextToken: String
}

structure ListItemsOutput {
    /// List of items
    items: ItemList
    
    /// Token for the next page
    nextToken: String
}

structure ListItemsWithPageSizeInput {
    /// Token for retrieving the next page
    @httpQuery("nextToken")
    nextToken: String
    
    /// Maximum number of items to return
    @httpQuery("maxResults")
    maxResults: Integer
}

structure ListItemsWithPageSizeOutput {
    /// List of items
    items: ItemList
    
    /// Token for the next page
    nextToken: String
}

structure Item {
    /// Unique identifier
    id: String
    
    /// Item name
    name: String
}

list ItemList {
    member: Item
}

