-module(storage_client_test).
-include_lib("eunit/include/eunit.hrl").

%% Basic test to verify module loads
module_loads_test() ->
    ?assert(code:ensure_loaded(storage_client) =:= {module, storage_client}).

%% Test that the types header file is generated and contains union marker
types_header_has_union_marker_test() ->
    %% Read the generated header file
    HeaderPath = "src/generated/storage_client_types.hrl",
    {ok, Content} = file:read_file(HeaderPath),
    
    %% Verify union marker is present
    ?assert(binary:match(Content, <<"Union: StorageType">>) =/= nomatch),
    
    %% Verify TODO comment for Step 4.2 is present
    ?assert(binary:match(Content, <<"TODO: Generate union type definition in Step 4.2">>) =/= nomatch).

%% Test that union member structures are generated
union_member_structures_generated_test() ->
    HeaderPath = "src/generated/storage_client_types.hrl",
    {ok, Content} = file:read_file(HeaderPath),
    
    %% Verify all union member structures are present
    ?assert(binary:match(Content, <<"Record for S3Storage">>) =/= nomatch),
    ?assert(binary:match(Content, <<"Record for GlacierStorage">>) =/= nomatch),
    ?assert(binary:match(Content, <<"Record for EfsStorage">>) =/= nomatch).

%% Note: Full union functionality tests will be added in Steps 4.2-4.4
%% when union encoding/decoding is implemented.
