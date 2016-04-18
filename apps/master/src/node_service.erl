%%%-------------------------------------------------------------------
%% @doc master public API
%% @end
%%%-------------------------------------------------------------------

-module(node_service).
-export([node_register/3, node_unregister/3]).

%% @doc Register your node in the graph
%% @end
node_register(IPaddress, Port, PublicKey) when is_integer(Port) -> 
    ok = node_graph_manager:add_node(IPaddress, Port, PublicKey),
    heartbeat_monitor:add_node(IPaddress, Port, PublicKey).

%% @doc Unregister your node in the graph
%% @end
node_unregister(IPaddress, Port, PublicKey) when is_integer(Port) ->
    ok = node_graph_manager:remove_node(IPaddress, Port, PublicKey),
    heartbeat_monitor:remove_node(IPaddress, Port, PublicKey).