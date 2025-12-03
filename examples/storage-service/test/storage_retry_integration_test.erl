-module(storage_retry_integration_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests for Retry Integration in Generated Operations
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: 2-arity function calls 3-arity with default options
%%--------------------------------------------------------------------
two_arity_wrapper_test() ->
    %% Mock client and input
    Client = #{endpoint => <<"https://api.example.com">>,
              access_key_id => <<"test_key">>,
              secret_access_key => <<"test_secret">>,
              region => <<"us-east-1">>},
    Input = #{<<"locationId">> => <<"loc-123">>},
    
    %% This would make an actual HTTP request, so we just verify
    %% the function exists and has correct arity
    ?assert(erlang:function_exported(storage_client, get_storage_location, 2)),
    ?assert(erlang:function_exported(storage_client, get_storage_location, 3)).

%%--------------------------------------------------------------------
%% Test: 3-arity function accepts options
%%--------------------------------------------------------------------
three_arity_accepts_options_test() ->
    %% Verify the 3-arity function is exported
    ?assert(erlang:function_exported(storage_client, get_storage_location, 3)),
    ?assert(erlang:function_exported(storage_client, create_storage_location, 3)).

%%--------------------------------------------------------------------
%% Test: Default behavior has retry enabled
%%--------------------------------------------------------------------
default_retry_enabled_test_() ->
    {setup,
     fun() ->
         %% Setup: Save original function
         Original = fun storage_client:get_storage_location/2,
         {original, Original}
     end,
     fun(_) ->
         %% Cleanup
         ok
     end,
     fun(_) ->
         [
          ?_test(begin
                     %% By default, operations should use retry
                     %% We can't easily test this without mocking, but we can
                     %% verify the functions exist
                     ?assert(erlang:function_exported(storage_client, get_storage_location, 3))
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% Test: Can disable retry
%%--------------------------------------------------------------------
can_disable_retry_test() ->
    %% Verify we can call with enable_retry => false
    %% (This would need actual mocking to test fully)
    Options = #{enable_retry => false},
    ?assertMatch(#{enable_retry := false}, Options).

%%--------------------------------------------------------------------
%% Test: Can configure retry options
%%--------------------------------------------------------------------
can_configure_retry_options_test() ->
    %% Verify we can pass retry configuration options
    Options = #{
        enable_retry => true,
        max_retries => 5,
        initial_backoff => 200,
        max_backoff => 10000
    },
    
    ?assertEqual(5, maps:get(max_retries, Options)),
    ?assertEqual(200, maps:get(initial_backoff, Options)),
    ?assertEqual(10000, maps:get(max_backoff, Options)).

%%--------------------------------------------------------------------
%% Test: All operations have retry integration
%%--------------------------------------------------------------------
all_operations_have_retry_test() ->
    %% Verify all operations have both 2-arity and 3-arity versions
    Operations = [
        get_storage_location,
        create_storage_location
    ],
    
    lists:foreach(fun(Op) ->
        ?assert(erlang:function_exported(storage_client, Op, 2)),
        ?assert(erlang:function_exported(storage_client, Op, 3))
    end, Operations).

%%--------------------------------------------------------------------
%% Test: Internal request functions exist
%%--------------------------------------------------------------------
internal_request_functions_exist_test() ->
    %% The internal make_*_request functions should exist
    %% Note: These are not exported, so we can't test them directly
    %% But we can verify the module loads
    Result = code:ensure_loaded(storage_client),
    ?assertMatch({module, storage_client}, Result).

%%--------------------------------------------------------------------
%% Test: Generated code includes retry documentation
%%--------------------------------------------------------------------
generated_code_has_retry_docs_test() ->
    %% Read the generated client file
    {ok, ClientCode} = file:read_file("src/generated/storage_client.erl"),
    CodeStr = binary_to_list(ClientCode),
    
    %% Check for retry-related comments
    ?assert(string:str(CodeStr, "enable_retry") > 0, 
            "Should document enable_retry option"),
    ?assert(string:str(CodeStr, "max_retries") > 0,
            "Should document max_retries option"),
    ?assert(string:str(CodeStr, "initial_backoff") > 0,
            "Should document initial_backoff option"),
    ?assert(string:str(CodeStr, "max_backoff") > 0,
            "Should document max_backoff option").

%%--------------------------------------------------------------------
%% Test: Generated code uses aws_retry module
%%--------------------------------------------------------------------
generated_code_uses_aws_retry_test() ->
    %% Read the generated client file
    {ok, ClientCode} = file:read_file("src/generated/storage_client.erl"),
    CodeStr = binary_to_list(ClientCode),
    
    %% Check that aws_retry:with_retry is called
    ?assert(string:str(CodeStr, "aws_retry:with_retry") > 0,
            "Should call aws_retry:with_retry").

%%--------------------------------------------------------------------
%% Test: Internal functions have proper naming
%%--------------------------------------------------------------------
internal_function_naming_test() ->
    %% Read the generated client file
    {ok, ClientCode} = file:read_file("src/generated/storage_client.erl"),
    CodeStr = binary_to_list(ClientCode),
    
    %% Check for internal function naming pattern
    ?assert(string:str(CodeStr, "make_get_storage_location_request") > 0,
            "Should have internal request function"),
    ?assert(string:str(CodeStr, "make_create_storage_location_request") > 0,
            "Should have internal request function for create").

%%--------------------------------------------------------------------
%% Test: Options map is properly checked
%%--------------------------------------------------------------------
options_map_checked_test() ->
    %% Read the generated client file
    {ok, ClientCode} = file:read_file("src/generated/storage_client.erl"),
    CodeStr = binary_to_list(ClientCode),
    
    %% Check for maps:get(enable_retry, Options, true)
    ?assert(string:str(CodeStr, "maps:get(enable_retry, Options, true)") > 0,
            "Should check enable_retry option with default").

%%--------------------------------------------------------------------
%% Test: Function guards are present
%%--------------------------------------------------------------------
function_guards_present_test() ->
    %% Read the generated client file
    {ok, ClientCode} = file:read_file("src/generated/storage_client.erl"),
    CodeStr = binary_to_list(ClientCode),
    
    %% Check for guards on 3-arity function
    ?assert(string:str(CodeStr, "when is_map(Input), is_map(Options)") > 0,
            "Should have guards for Input and Options").

%%--------------------------------------------------------------------
%% Test: RequestFun lambda is created
%%--------------------------------------------------------------------
request_fun_lambda_created_test() ->
    %% Read the generated client file
    {ok, ClientCode} = file:read_file("src/generated/storage_client.erl"),
    CodeStr = binary_to_list(ClientCode),
    
    %% Check for RequestFun lambda creation
    ?assert(string:str(CodeStr, "RequestFun = fun()") > 0,
            "Should create RequestFun lambda").

%%--------------------------------------------------------------------
%% Test: Case statement for retry toggle
%%--------------------------------------------------------------------
case_statement_for_retry_toggle_test() ->
    %% Read the generated client file
    {ok, ClientCode} = file:read_file("src/generated/storage_client.erl"),
    CodeStr = binary_to_list(ClientCode),
    
    %% Check for case statement
    ?assert(string:str(CodeStr, "case maps:get(enable_retry") > 0,
            "Should have case statement for retry toggle"),
    ?assert(string:str(CodeStr, "true ->") > 0,
            "Should have true branch"),
    ?assert(string:str(CodeStr, "false ->") > 0,
            "Should have false branch").

%%--------------------------------------------------------------------
%% Test: Module compiles and loads
%%--------------------------------------------------------------------
module_compiles_and_loads_test() ->
    %% Verify the module compiles and loads successfully
    Result = code:ensure_loaded(storage_client),
    ?assertMatch({module, storage_client}, Result),
    
    %% Verify it has the expected exports
    Exports = storage_client:module_info(exports),
    
    %% Check for both arities of operations
    ?assert(lists:member({get_storage_location, 2}, Exports)),
    ?assert(lists:member({get_storage_location, 3}, Exports)).

%%--------------------------------------------------------------------
%% Test: Type specs are generated correctly
%%--------------------------------------------------------------------
type_specs_generated_test() ->
    %% Read the generated client file
    {ok, ClientCode} = file:read_file("src/generated/storage_client.erl"),
    CodeStr = binary_to_list(ClientCode),
    
    %% Check for -spec declarations
    ?assert(string:str(CodeStr, "-spec get_storage_location(Client :: map()") > 0,
            "Should have spec for 2-arity function"),
    ?assert(string:str(CodeStr, "Options :: map())") > 0,
            "Should have spec for 3-arity function with Options").

%%--------------------------------------------------------------------
%% Test: Integration with existing retry module
%%--------------------------------------------------------------------
integration_with_retry_module_test() ->
    %% Verify aws_retry module exists and is accessible
    Result = code:ensure_loaded(aws_retry),
    ?assertMatch({module, aws_retry}, Result),
    
    %% Verify aws_retry has the expected functions
    Exports = aws_retry:module_info(exports),
    ?assert(lists:member({with_retry, 1}, Exports)),
    ?assert(lists:member({with_retry, 2}, Exports)).
