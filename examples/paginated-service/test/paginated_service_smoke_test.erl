-module(paginated_service_smoke_test).

-include_lib("eunit/include/eunit.hrl").
-include("paginated_service_types.hrl").

%%====================================================================
%% Smoke Tests: Pagination Helper Functions
%% 
%% These are basic smoke tests that verify the pagination helpers exist
%% and have the correct signatures. Full functional tests with mocking
%% can be added in paginated_service_test.erl.
%%====================================================================

%%--------------------------------------------------------------------
%% Module and Function Existence Tests
%%--------------------------------------------------------------------

module_loads_test() ->
    %% Verify the module loads
    ?assert(code:is_loaded(paginated_service) =/= false orelse 
            code:ensure_loaded(paginated_service) =:= {module, paginated_service}).

basic_functions_exported_test() ->
    %% Verify basic functions exist
    ?assert(erlang:function_exported(paginated_service, new, 1)),
    ?assert(erlang:function_exported(paginated_service, list_items, 2)),
    ?assert(erlang:function_exported(paginated_service, list_items, 3)).

pagination_helpers_exported_test() ->
    %% Verify pagination helper functions are exported
    ?assert(erlang:function_exported(paginated_service, list_items_all_pages, 2)),
    ?assert(erlang:function_exported(paginated_service, list_items_all_pages, 3)),
    ?assert(erlang:function_exported(paginated_service, list_items_with_page_size_all_pages, 2)),
    ?assert(erlang:function_exported(paginated_service, list_items_with_page_size_all_pages, 3)).

%%--------------------------------------------------------------------
%% Client Creation Test
%%--------------------------------------------------------------------

client_creation_test() ->
    %% Verify client creation works
    Config = #{endpoint => <<"https://api.example.com">>},
    Result = paginated_service:new(Config),
    ?assertMatch({ok, _}, Result),
    {ok, Client} = Result,
    ?assert(is_map(Client)),
    ?assertEqual(<<"https://api.example.com">>, maps:get(endpoint, Client)).

%%--------------------------------------------------------------------
%% Documentation Tests
%%--------------------------------------------------------------------

pagination_helper_count_test() ->
    %% Verify we have pagination helpers for each paginated operation
    %% The model has 2 paginated operations (ListItems, ListItemsWithPageSize)
    Exports = paginated_service:module_info(exports),
    
    %% Count *_all_pages functions
    AllPagesExports = [{Name, Arity} || {Name, Arity} <- Exports, 
                                         Arity =:= 2 orelse Arity =:= 3,
                                         lists:suffix("_all_pages", atom_to_list(Name))],
    
    %% Should have 4 exports (2 operations × 2 arities)
    ?assertEqual(4, length(AllPagesExports)).

%%--------------------------------------------------------------------
%% README: Example Usage Patterns
%%--------------------------------------------------------------------

%% These tests serve as documentation for how to use the pagination helpers.
%% They don't actually call the API, but show the expected usage patterns.

example_usage_basic_test() ->
    %% Example: Basic usage of pagination helper
    Client = #{endpoint => <<"https://api.example.com">>},
    Input = #{},
    
    %% Expected usage (would make HTTP calls):
    %% {ok, AllItems} = paginated_service:list_items_all_pages(Client, Input)
    
    %% For this test, just verify the function signature is correct
    ?assert(erlang:function_exported(paginated_service, list_items_all_pages, 2)).

example_usage_with_options_test() ->
    %% Example: Usage with retry options
    Client = #{endpoint => <<"https://api.example.com">>},
    Input = #{},
    _Options = #{
        enable_retry => true,
        max_retries => 5,
        initial_backoff => 100
    },
    
    %% Expected usage (would make HTTP calls):
    %% {ok, AllItems} = paginated_service:list_items_all_pages(Client, Input, Options)
    
    %% For this test, just verify the function signature is correct
    ?assert(erlang:function_exported(paginated_service, list_items_all_pages, 3)).

example_usage_with_page_size_test() ->
    %% Example: Usage with page size control
    Client = #{endpoint => <<"https://api.example.com">>},
    Input = #{<<"maxResults">> => 100},
    
    %% Expected usage (would make HTTP calls):
    %% {ok, AllItems} = paginated_service:list_items_with_page_size_all_pages(Client, Input)
    
    %% For this test, just verify the function signature is correct
    ?assert(erlang:function_exported(paginated_service, list_items_with_page_size_all_pages, 2)).
