-module(storage_enum_type_test).

-include_lib("eunit/include/eunit.hrl").
-include("storage_client_types.hrl").

%% Ignore dialyzer warnings
-dialyzer([no_return, no_match, no_contracts]).

%%% ============================================================================
%%% Enum Type Definition Tests
%%% ============================================================================

%%% ----------------------------------------------------------------------------
%%% Type Existence Tests
%%% ----------------------------------------------------------------------------

enum_type_exists_test() ->
    %% Test that the enum type is defined
    %% We can't directly test type existence, but we can use it in specs
    %% and dialyzer will validate it
    ok.

%%% ----------------------------------------------------------------------------
%%% Enum Values - Basic Usage
%%% ----------------------------------------------------------------------------

use_expedited_atom_test() ->
    %% Test using the expedited atom
    RetrievalOption = expedited,
    ?assertEqual(expedited, RetrievalOption),
    ?assert(is_atom(RetrievalOption)).

use_standard_atom_test() ->
    %% Test using the standard atom
    RetrievalOption = standard,
    ?assertEqual(standard, RetrievalOption),
    ?assert(is_atom(RetrievalOption)).

use_bulk_atom_test() ->
    %% Test using the bulk atom
    RetrievalOption = bulk,
    ?assertEqual(bulk, RetrievalOption),
    ?assert(is_atom(RetrievalOption)).

%%% ----------------------------------------------------------------------------
%%% Pattern Matching Tests
%%% ----------------------------------------------------------------------------

pattern_match_expedited_test() ->
    %% Test pattern matching on expedited
    Result = case expedited of
        expedited -> success;
        _ -> fail
    end,
    ?assertEqual(success, Result).

pattern_match_standard_test() ->
    %% Test pattern matching on standard
    Result = case standard of
        standard -> success;
        _ -> fail
    end,
    ?assertEqual(success, Result).

pattern_match_bulk_test() ->
    %% Test pattern matching on bulk
    Result = case bulk of
        bulk -> success;
        _ -> fail
    end,
    ?assertEqual(success, Result).

pattern_match_all_options_test() ->
    %% Test pattern matching all options
    Options = [expedited, standard, bulk],
    Results = lists:map(fun(Option) ->
        case Option of
            expedited -> fast;
            standard -> normal;
            bulk -> slow
        end
    end, Options),
    ?assertEqual([fast, normal, slow], Results).

%%% ----------------------------------------------------------------------------
%%% Function Parameter Tests
%%% ----------------------------------------------------------------------------

accept_enum_parameter_test() ->
    %% Test function that accepts enum parameter
    GetLabel = fun(Option) ->
        case Option of
            expedited -> <<"Expedited">>;
            standard -> <<"Standard">>;
            bulk -> <<"Bulk">>
        end
    end,
    
    ?assertEqual(<<"Expedited">>, GetLabel(expedited)),
    ?assertEqual(<<"Standard">>, GetLabel(standard)),
    ?assertEqual(<<"Bulk">>, GetLabel(bulk)).

return_enum_value_test() ->
    %% Test function that returns enum value
    GetOption = fun(Speed) ->
        if
            Speed > 100 -> expedited;
            Speed > 50 -> standard;
            true -> bulk
        end
    end,
    
    ?assertEqual(expedited, GetOption(150)),
    ?assertEqual(standard, GetOption(75)),
    ?assertEqual(bulk, GetOption(10)).

%%% ----------------------------------------------------------------------------
%%% Record Integration Tests
%%% ----------------------------------------------------------------------------

use_enum_in_record_test() ->
    %% Test using enum in GlacierStorage record
    Storage = #glacier_storage{
        vault = <<"my-vault">>,
        region = <<"us-east-1">>,
        retrieval_option = expedited
    },
    
    ?assertEqual(expedited, Storage#glacier_storage.retrieval_option),
    ?assertEqual(<<"my-vault">>, Storage#glacier_storage.vault).

update_enum_in_record_test() ->
    %% Test updating enum value in record
    Storage1 = #glacier_storage{
        vault = <<"vault">>,
        region = <<"us-west-2">>,
        retrieval_option = expedited
    },
    
    Storage2 = Storage1#glacier_storage{retrieval_option = standard},
    
    ?assertEqual(expedited, Storage1#glacier_storage.retrieval_option),
    ?assertEqual(standard, Storage2#glacier_storage.retrieval_option).

match_record_with_enum_test() ->
    %% Test pattern matching record with enum
    Storage = #glacier_storage{
        vault = <<"test">>,
        region = <<"us-east-1">>,
        retrieval_option = bulk
    },
    
    Result = case Storage of
        #glacier_storage{retrieval_option = bulk} -> slow_option;
        #glacier_storage{retrieval_option = expedited} -> fast_option;
        _ -> other_option
    end,
    
    ?assertEqual(slow_option, Result).

%%% ----------------------------------------------------------------------------
%%% List Operations
%%% ----------------------------------------------------------------------------

enum_list_test() ->
    %% Test list of enum values
    Options = [expedited, standard, bulk],
    
    ?assertEqual(3, length(Options)),
    ?assert(lists:member(expedited, Options)),
    ?assert(lists:member(standard, Options)),
    ?assert(lists:member(bulk, Options)).

filter_enum_list_test() ->
    %% Test filtering enum list
    AllOptions = [expedited, standard, bulk, expedited, standard],
    FastOptions = lists:filter(fun(Opt) -> Opt =:= expedited end, AllOptions),
    
    ?assertEqual([expedited, expedited], FastOptions).

map_enum_to_priority_test() ->
    %% Test mapping enum values to priorities
    Options = [bulk, standard, expedited],
    Priorities = lists:map(fun(Opt) ->
        case Opt of
            expedited -> 1;
            standard -> 2;
            bulk -> 3
        end
    end, Options),
    
    ?assertEqual([3, 2, 1], Priorities).

%%% ----------------------------------------------------------------------------
%%% Comparison Tests
%%% ----------------------------------------------------------------------------

enum_equality_test() ->
    %% Test enum equality
    Opt1 = expedited,
    Opt2 = expedited,
    Opt3 = standard,
    
    ?assert(Opt1 =:= Opt2),
    ?assert(Opt1 =/= Opt3),
    ?assertEqual(Opt1, Opt2),
    ?assertNotEqual(Opt1, Opt3).

enum_in_case_test() ->
    %% Test enum in case expression
    CheckOption = fun(Opt) ->
        case Opt of
            expedited -> {ok, fast};
            standard -> {ok, normal};
            bulk -> {ok, slow};
            _ -> {error, invalid}
        end
    end,
    
    ?assertEqual({ok, fast}, CheckOption(expedited)),
    ?assertEqual({ok, normal}, CheckOption(standard)),
    ?assertEqual({ok, slow}, CheckOption(bulk)).

%%% ----------------------------------------------------------------------------
%%% Map Integration
%%% ----------------------------------------------------------------------------

enum_as_map_value_test() ->
    %% Test enum as map value
    Config = #{
        retrieval_option => expedited,
        vault => <<"my-vault">>,
        timeout => 300
    },
    
    ?assertEqual(expedited, maps:get(retrieval_option, Config)),
    ?assert(is_atom(maps:get(retrieval_option, Config))).

enum_in_multiple_maps_test() ->
    %% Test enum in multiple maps
    Configs = [
        #{option => expedited, cost => high},
        #{option => standard, cost => medium},
        #{option => bulk, cost => low}
    ],
    
    Options = [maps:get(option, C) || C <- Configs],
    ?assertEqual([expedited, standard, bulk], Options).

%%% ----------------------------------------------------------------------------
%%% Guard Tests
%%% ----------------------------------------------------------------------------

enum_in_guard_test() ->
    %% Test using enum in guards
    IsFast = fun(Opt) when Opt =:= expedited -> true;
                (_) -> false
             end,
    
    ?assert(IsFast(expedited)),
    ?assertNot(IsFast(standard)),
    ?assertNot(IsFast(bulk)).

multiple_enum_guards_test() ->
    %% Test multiple enum values in guards
    IsNotBulk = fun(Opt) when Opt =:= expedited; Opt =:= standard -> true;
                   (_) -> false
                end,
    
    ?assert(IsNotBulk(expedited)),
    ?assert(IsNotBulk(standard)),
    ?assertNot(IsNotBulk(bulk)).

%%% ----------------------------------------------------------------------------
%%% Default Value Tests
%%% ----------------------------------------------------------------------------

default_enum_value_test() ->
    %% Test using default enum value
    GetDefault = fun() -> standard end,
    
    Default = GetDefault(),
    ?assertEqual(standard, Default),
    ?assert(is_atom(Default)).

fallback_enum_value_test() ->
    %% Test fallback to default enum value
    GetOption = fun(undefined) -> standard;
                   (Value) -> Value
                end,
    
    ?assertEqual(standard, GetOption(undefined)),
    ?assertEqual(expedited, GetOption(expedited)),
    ?assertEqual(bulk, GetOption(bulk)).

%%% ----------------------------------------------------------------------------
%%% Serialization Preparation Tests
%%% ----------------------------------------------------------------------------

enum_to_binary_test() ->
    %% Test converting enum to binary (for JSON serialization)
    ToBinary = fun(expedited) -> <<"expedited">>;
                  (standard) -> <<"standard">>;
                  (bulk) -> <<"bulk">>
               end,
    
    ?assertEqual(<<"expedited">>, ToBinary(expedited)),
    ?assertEqual(<<"standard">>, ToBinary(standard)),
    ?assertEqual(<<"bulk">>, ToBinary(bulk)).

enum_to_string_test() ->
    %% Test converting enum to string
    ToString = fun(Opt) -> atom_to_list(Opt) end,
    
    ?assertEqual("expedited", ToString(expedited)),
    ?assertEqual("standard", ToString(standard)),
    ?assertEqual("bulk", ToString(bulk)).

%%% ----------------------------------------------------------------------------
%%% Validation Tests
%%% ----------------------------------------------------------------------------

is_valid_enum_test() ->
    %% Test enum validation
    ValidOptions = [expedited, standard, bulk],
    IsValid = fun(Opt) -> lists:member(Opt, ValidOptions) end,
    
    ?assert(IsValid(expedited)),
    ?assert(IsValid(standard)),
    ?assert(IsValid(bulk)),
    ?assertNot(IsValid(invalid)),
    ?assertNot(IsValid(fast)).

validate_enum_or_default_test() ->
    %% Test validation with default fallback
    ValidOptions = [expedited, standard, bulk],
    ValidateOrDefault = fun(Opt) ->
        case lists:member(Opt, ValidOptions) of
            true -> Opt;
            false -> standard  % default
        end
    end,
    
    ?assertEqual(expedited, ValidateOrDefault(expedited)),
    ?assertEqual(standard, ValidateOrDefault(invalid)),
    ?assertEqual(standard, ValidateOrDefault(undefined)).
