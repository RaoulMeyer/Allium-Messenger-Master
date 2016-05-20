var nodes, edges, network;
var url = "ws://localhost:8080/websocket";
var socket;
var counter = 10;
var edgeCounter = 10;
var ProtoBuf = dcodeIO.ProtoBuf;
var builder = ProtoBuf.loadProtoFile("js/hrp.proto");
var Wrapper = builder.build("Wrapper");
var OnionNode = builder.build("Node");
var NodeDeleteRequest = builder.build("NodeDeleteRequest");
var NodeRegisterRequest = builder.build("NodeRegisterRequest");
var UpdateNode = builder.build("UpdateNode");
var GraphUpdateResponse = builder.build("GraphUpdateResponse");
var GraphUpdate = builder.build("GraphUpdate");


$(function () {
    if (typeof dcodeIO === 'undefined' || !dcodeIO.ProtoBuf) {
        throw(new Error("ProtoBuf.js is not present."));
    }

    function initSocket() {
        console.log("Initializing socket");
        socket = new WebSocket(url);
        socket.binaryType = "arraybuffer";

        socket.onopen = socketOpen;
        socket.onclose = socketClose;
        socket.onmessage = socketMessage;
    }

    function socketOpen() {
        console.log("WebSocket connection established");
    }

    function socketClose() {
        console.log("WebSocket connection closed");
    }

    function socketMessage(event) {
        console.log("Received message: " + event.data);
        try {
            var wrapper = Wrapper.decode(event.data);

            switch (wrapper.type) {
                case Wrapper.Type.GRAPHUPDATERESPONSE:
                    console.log("Received GraphUpdateResponse...");
                    var graphUpdateResponse = GraphUpdateResponse.decode(wrapper.data);
                    graphUpdateResponse.graphUpdates.forEach(function (update) {
                        var graphUpdate = GraphUpdate.decode(update);

                        if (graphUpdate.isFullGraph) {
                            nodes.clear();
                        }

                        if(graphUpdate.addedNodes){
                            graphUpdate.addedNodes.forEach(function(node) {
                                addNode(node);
                            });
                        }

                        if(graphUpdate.deletedNodes){
                            graphUpdate.deletedNodes.forEach(function(node) {
                                removeNode(node);
                            });
                        }
                    });
                    break;
            }
        } catch (error) {
            console.log("Error while receiving message: " + error);
        }
    }

    function addNode(node) {
        try {
            nodes.add({
                id: node.id,
                IPaddress: node.IPaddress,
                port: node.port,
                publicKey: node.publicKey,
                label: node.id
            });
            if(!node.edge) return;
            
            node.edge.forEach(function (edge) {
                console.log(JSON.stringify(edge));
                edges.add({
                    id: node.id + edge.targetNodeId,
                    from: node.id,
                    to: edge.targetNodeId,
                    weight: 1,
                    arrows: 'to'
                });
            });
        } catch (error) {
            alert(error);
        }
    }

    function removeNode(node) {
        try {
            nodes.remove({
                id: node.id
            });
        } catch (error) {
            alert(error);
        }
    }

    function drawGraph() {
        nodes = new vis.DataSet();
        /* Voorbeelddata, to remove */
        // nodes.add([
        //     {id: 'Node 1', IPaddress: "127.0.0.1", port: 1337, publicKey: "secret", label: 'Node 1'},
        //     {id: 'Node 2', IPaddress: "127.0.0.2", port: 1337, publicKey: "secret", label: 'Node 2'},
        //     {id: 'Node 31', IPaddress: "127.0.0.3", port: 1337, publicKey: "secret", label: 'Node 31'},
        //     {id: 'Node 3', IPaddress: "127.0.0.3", port: 1337, publicKey: "secret", label: 'Node 3'},
        // ]);

        edges = new vis.DataSet();
        // /* Voorbeelddata, to remove */
        // edges.add([
        //     {id: 'Node 1Node 2', from: 'Node 1', to: 'Node 2', weight_from_to: '12', weight_to_from: undefined, arrows:'to'},
        // ]);

        var container = document.getElementById('network');

        var data = {
            nodes: nodes,
            edges: edges
        };

        var options = {interaction: {hover: true, selectConnectedEdges: false, hoverConnectedEdges: false}};

        network = new vis.Network(container, data, options);

        network.on("selectEdge", function (data) {
                    $("#edgeData1").html("from node " + edges._data[data.edges[0]].from + " to node " + edges._data[data.edges[0]].to);
                    $("#edgeData2").html("from node " + edges._data[data.edges[0]].to + " to node " + edges._data[data.edges[0]].from);

                    $("#weight1").val(edges._data[data.edges[0]].weight_from_to);
                    $("#weight2").val(edges._data[data.edges[0]].weight_to_from);

                    $("#edgeFrom").val(edges._data[data.edges[0]].from);
                    $("#edgeTo").val(edges._data[data.edges[0]].to);

                    var div = document.getElementById("edit-from-edge");
                    div.style.display = 'block';
                    div = document.getElementById("edit-from-node");
                    div.style.display = 'none';
                    var div = document.getElementById("add-edge-from-node");
                    div.style.display = 'none';
                });

        network.on("selectNode", function (data) {

                    $("#nodeId").val(nodes._data[data.nodes[0]].id);

                    var div = document.getElementById("edit-from-node");
                    div.style.display = 'block';  
                    div = document.getElementById("edit-from-edge");
                    div.style.display = 'none';
                    var div = document.getElementById("add-edge-from-node");
                    div.style.display = 'none';
                });
    }

    function clear() {
        nodes.clear();
    }

    $("#finder").on('submit', function(event) {
        event.preventDefault();
        var findNode = $("#find_node");
        var search = findNode.val();

        var result = undefined;
        nodes.forEach(function(node) {
            if (node.id.lastIndexOf(search, 0) === 0) {
                result = node;
            }
        });

        if (result !== undefined) {
            network.focus(result.id);
            network.selectNodes([result.id]);
        }

        findNode.val("");
    });

    drawGraph();
    initSocket();

});

    function addEdge(fromId, toId, weight) {
        weight = parseInt(weight);
        var node = nodes.get(fromId);
        var currentEdges = getEdges(fromId);
        if(checkEdge(fromId, toId)) {
            console.log(toId);
            console.log(weight);
            currentEdges.push({targetNodeId: toId, weight: weight});

            console.log(JSON.stringify(currentEdges));
            toSend = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges};
            console.log(JSON.stringify(toSend));
            newNode = new OnionNode({id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges});
            var message = new UpdateNode({node: newNode});
            socketSend("UPDATENODE", message.encode());
        }
    }

    function createEdgeForm(nodeId) {
        node = nodes.get(nodeId);

        $("#from").val(nodeId);

        var div = document.getElementById("add-edge-from-node");
        div.style.display = 'block';
    }

    function deleteEdge(nodeId1, nodeId2) {
        var node = nodes.get(nodeId1);
        var currentEdges = getEdges(nodeId1);
        currentEdges.forEach(function(edge){
            if(edge.targetNodeId == nodeId2) {
                console.log("trying to remove edge");
                var index = currentEdges.indexOf(edge);
                if (index > -1) {
                    currentEdges.splice(index, 1);
                }
            }
        });
        toSend = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges};
        alert(JSON.stringify(toSend));
        var newNode = new OnionNode({id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges});
        var message = new UpdateNode({node: newNode});
        console.log("predecode message: " + JSON.stringify(message));
        socketSend("UPDATENODE", message.encode());
    }

    function updateEdge(nodeId1, nodeId2, weight) {
        console.log(nodeId1);
        console.log(nodeId2);
        console.log(weight);
        var node = nodes.get(nodeId1);
        var currentEdges = getEdges(nodeId1);
        var targetEdgeIndex = -1;
        currentEdges.forEach(function(edge){
            if(edge.targetNodeId == nodeId2) {
                console.log("trying to remove edge");
                targetEdgeIndex = currentEdges.indexOf(edge);
            }
        });
        if (targetEdgeIndex > -1) {
            currentEdges.splice(targetEdgeIndex, 1);
        }
        currentEdges.push({targetNodeId: nodeId2, weight: weight});
        toSend = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges};
        alert(JSON.stringify(toSend));
    }

    function checkEdge (fromId, toId) {
        if(fromId == toId) {
            return false;
        }

        var node = nodes.get(toId);
        if(!node) {
            return false;
        }

        var edge = edges.get(fromId + toId)
        if(edge) {
            return false;
        }

        var otherEdge = edges.get(toId + fromId)
        if(otherEdge) {
            if(otherEdge.weight_to_from != undefined) {
                return false;
            }
        }
        return true;
    }

    function getEdges(NodeId){
        currentEdges = [];
        edges.forEach(function(edgeToAdd) {
            if(edgeToAdd.from == NodeId && edgeToAdd.weight_from_to != undefined) {
                currentEdges.push({targetNodeId: edgeToAdd.to, weight: edgeToAdd.weight_from_to});
            }
            if(edgeToAdd.to == NodeId && edgeToAdd.weight_to_from != undefined) {
                currentEdges.push({targetNodeId: edgeToAdd.from, weight: edgeToAdd.weight_to_from});
            }
        });
        return currentEdges;
    }

    function socketSend(type, data) {
        if (socket.readyState == WebSocket.OPEN) {
            var message = new Wrapper({type: type, data: data});
            var response = message.encodeDelimited();
            socket.send(response.toArrayBuffer());
        } else {
            console.log("Not connected while sending: " + data);
        }
    }




    // function createUpdateNodeMessage(Id) {
    //     var node = nodes.get(Id);
    //     alert(JSON.stringify(node));
    //     var currentEdges = getEdges(Id);
    //     toSend = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges};

    //     alert(JSON.stringify(toSend));
    //     var newNode = new OnionNode({id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges});
    //     var message = new UpdateNode({node: newNode});
    //     socketSend("UPDATENODE", message.encode());
    // }

