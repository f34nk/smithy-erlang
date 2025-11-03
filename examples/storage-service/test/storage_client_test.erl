-module(storage_client_test).
-include_lib("eunit/include/eunit.hrl").

%% Basic test to verify module loads
module_loads_test() ->
    ?assert(code:ensure_loaded(storage_client) =:= {module, storage_client}).

%% Test that the types header file is generated and contains union type definition
types_header_has_union_type_test() ->
    %% Read the generated header file
    HeaderPath = "src/generated/storage_client_types.hrl",
    {ok, Content} = file:read_file(HeaderPath),
    
    %% Verify union type comment is present
    ?assert(binary:match(Content, <<"Union type for StorageType">>) =/= nomatch),
    
    %% Verify actual union type definition is present
    ?assert(binary:match(Content, <<"-type storage_type() ::">>) =/= nomatch),
    
    %% Verify tagged tuple format
    ?assert(binary:match(Content, <<"{s3, #s3storage{}}">>) =/= nomatch),
    ?assert(binary:match(Content, <<"{glacier, #glacier_storage{}}">>) =/= nomatch),
    ?assert(binary:match(Content, <<"{efs, #efs_storage{}}">>) =/= nomatch).

%% Test that union member structures are generated
union_member_structures_generated_test() ->
    HeaderPath = "src/generated/storage_client_types.hrl",
    {ok, Content} = file:read_file(HeaderPath),
    
    %% Verify all union member structures are present
    ?assert(binary:match(Content, <<"Record for S3Storage">>) =/= nomatch),
    ?assert(binary:match(Content, <<"Record for GlacierStorage">>) =/= nomatch),
    ?assert(binary:match(Content, <<"Record for EfsStorage">>) =/= nomatch).

%% Note: Union encoding/decoding tests will be added in Steps 4.3-4.4
%% when encode/decode functions are implemented.
