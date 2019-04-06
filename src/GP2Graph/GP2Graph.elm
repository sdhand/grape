module GP2Graph.GP2Graph exposing (Label, Mark(..), MultiContext, MultiGraph, VisualGraph, VisualContext, createEdge, createNode, deleteEdge, nextEdgeId, nextNodeId, nodePosition, setNodePosition)

import Geometry.Vec2 exposing (Vec2)
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import Graph exposing (Graph, Node, NodeContext, NodeId)
import IntDict
import List.Extra


type alias MultiGraph n e =
    Graph n (List e)


type alias MultiContext n e =
    NodeContext n (List e)


type alias VisualGraph =
    MultiGraph ( Label, Ellipse ) Label


type alias VisualContext =
    MultiContext ( Label, Ellipse ) Label


type Mark
    = None
    | Any
    | Grey
    | Dashed
    | Red
    | Blue
    | Green


type alias Label =
    { label : String
    , id : String
    , mark : Mark
    }


nextId : Graph n e -> NodeId
nextId graph =
    Graph.nodeIdRange graph
        |> Maybe.map (Tuple.second >> (+) 1)
        |> Maybe.withDefault 0


tryId : Int -> String -> List String -> String
tryId n prefix ids =
    let
        attempt = (prefix ++ String.fromInt n)
    in
    if List.member attempt ids then
        tryId (n + 1) prefix ids

    else
        attempt


nextNodeId : VisualGraph -> String
nextNodeId graph =
    tryId 0 "n" (Graph.nodes graph |> List.map (.label >> Tuple.first >> .id))


nextEdgeId : VisualGraph -> String
nextEdgeId graph =
    tryId 0 "e" (Graph.edges graph |> List.concatMap (.label >> List.map .id))


createNode : n -> MultiGraph n e -> MultiGraph n e
createNode label graph =
    Graph.insert
        { node = Node (nextId graph) label
        , incoming = IntDict.empty
        , outgoing = IntDict.empty
        }
        graph


createEdge : e -> NodeId -> NodeId -> MultiGraph n e -> MultiGraph n e
createEdge label from to graph =
    Graph.update
        from
        ((::) label |> updateOutgoing to |> Maybe.map)
        graph


deleteEdge : NodeId -> NodeId -> Int -> MultiGraph n e -> MultiGraph n e
deleteEdge from to i graph =
    Graph.update
        from
        (List.Extra.removeAt i |> updateOutgoing to |> Maybe.map)
        graph


updateNode : (n -> n) -> MultiContext n e -> MultiContext n e
updateNode f ({ node } as context) =
    { context | node = { node | label = f node.label } }


updateOutgoing : NodeId -> (List e -> List e) -> MultiContext n e -> MultiContext n e
updateOutgoing to f context =
    { context | outgoing = IntDict.update to (updateMultiEdge f) context.outgoing }


updateMultiEdge : (List e -> List e) -> Maybe (List e) -> Maybe (List e)
updateMultiEdge f edges =
    let
        newEdges =
            Maybe.withDefault [] edges |> f
    in
    if newEdges == [] then
        Nothing

    else
        Just newEdges


nodePosition : NodeId -> VisualGraph -> Maybe Vec2
nodePosition id graph =
    Graph.get id graph |> Maybe.map (.node >> .label >> Tuple.second >> .center)


setNodePosition : NodeId -> Vec2 -> VisualGraph -> VisualGraph
setNodePosition id position graph =
    Graph.update
        id
        (Ellipse.setCenter position |> Tuple.mapSecond |> updateNode |> Maybe.map)
        graph
