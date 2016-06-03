-module(integration_onion_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    init_per_testcase/2,
    end_per_suite/1
]).

-export([
    insert_nodes/1
]).

all() -> [
    insert_nodes
].

init_per_suite(Config) ->
    application:load(master),
    application:load(websocket),
    application:ensure_all_started(websocket),
    test_helpers_int:init_sharded_eredis(),
    Config.

init_per_testcase(_, Config) ->
    test_helpers_int:empty_database(),
    Config.

end_per_suite(Config) ->
    Config.

insert_nodes(_Config) ->
    node_service:node_register("192.168.1.1", 80, <<"key">>),
    node_service:node_register("192.168.1.2", 81, <<"key">>),
    node_service:node_register("192.168.1.3", 82, <<"key">>),
    erlang:display(node_graph_manager:get_graph_updates(0)).