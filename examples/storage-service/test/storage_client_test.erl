-module(storage_client_test).
-include_lib("eunit/include/eunit.hrl").

%% Basic test to verify module loads
module_loads_test() ->
    ?assert(code:ensure_loaded(storage_client) =:= {module, storage_client}).

%% Test that the generated module contains union type definition
types_module_has_union_type_test() ->
    %% Read the generated module file
    ModulePath = "src/generated/storage_client.erl",
    {ok, Content} = file:read_file(ModulePath),
    
    %% Verify actual union type definition is present
    ?assert(binary:match(Content, <<"-type storage_type() ::">>) =/= nomatch),
    
    %% Verify tagged tuple format with type references
    ?assert(binary:match(Content, <<"{s3, s3_storage()}">>) =/= nomatch),
    ?assert(binary:match(Content, <<"{glacier, glacier_storage()}">>) =/= nomatch),
    ?assert(binary:match(Content, <<"{efs, efs_storage()}">>) =/= nomatch).

%% Test that union member type aliases are generated
union_member_types_generated_test() ->
    ModulePath = "src/generated/storage_client.erl",
    {ok, Content} = file:read_file(ModulePath),
    
    %% Verify all union member type aliases are present
    ?assert(binary:match(Content, <<"-type s3_storage() ::">>) =/= nomatch),
    ?assert(binary:match(Content, <<"-type glacier_storage() ::">>) =/= nomatch),
    ?assert(binary:match(Content, <<"-type efs_storage() ::">>) =/= nomatch).
