module GP2Graph.GP2Graph exposing (Label, Mark(..), MultiContext, MultiGraph, VisualContext, VisualGraph, createEdge, createNode, deleteEdge, tryId, nodePosition, setNodeMajor, setNodePosition, updateEdgeLabel, updateNodeLabel, updateEdgeId, updateNodeId, updateEdgeMark, updateNodeMark, updateEdgeFlag, updateNodeFlag, getEdgeData, getNodeData, setLabel, setId, setMark, setFlag, HostList, HostListItem(..), toGP2, internalId, isValid, setNodeMinor)

import Geometry.Ellipse as Ellipse exposing (Ellipse)
import Geometry.Vec2 exposing (Vec2)
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
    { id : String
    , flag : Bool
    , label : String
    , mark : Mark
    }


type alias HostList =
    List HostListItem


type HostListItem
    = HostString String
    | HostInt Int
    | Empty


markToString : Mark -> String
markToString mark =
    case mark of
        None ->
            "none"

        Any ->
            "any"

        Grey ->
            "grey"

        Dashed ->
            "dashed"

        Red ->
            "red"

        Blue ->
            "blue"

        Green ->
            "green"


nodesToGP2 : Graph.Node ( Label, Ellipse ) -> String -> String
nodesToGP2 node acc =
    let
        (label, pos) =
            node.label
    in
    acc
        ++ "\t("
        ++ label.id
        ++ ( if label.flag then
                "(R)"

             else
                 ""
           )
        ++ ", "
        ++ label.label
        ++ ( if label.mark /= None then
                " # " ++ (markToString label.mark)

             else
                ""
           )
        ++ "<"
        ++ (String.fromFloat pos.center.x)
        ++ ", "
        ++ (String.fromFloat pos.center.y)
        ++ ">"
        ++ ")\n"


edgeToGP2 : VisualContext -> VisualContext -> Label -> String -> String
edgeToGP2 from to label acc =
    acc
        ++ "\t("
        ++ label.id
        ++ ( if label.flag then
                "(B)"

            else
                ""
           )
        ++ ", "
        ++ (Tuple.first from.node.label).id
        ++ ", "
        ++ (Tuple.first to.node.label).id
        ++ ", "
        ++ label.label
        ++ ( if label.mark /= None then
                "#" ++ (markToString label.mark)

             else
                ""
           )
        ++ ")\n"


edgesToGP2 : VisualGraph -> Graph.Edge (List Label) -> String -> String
edgesToGP2 graph { from, to, label } acc =
    acc
        ++ (Maybe.map2 (\f t -> List.foldl (edgeToGP2 f t) "" label) (Graph.get from graph) (Graph.get to graph)
                |> Maybe.withDefault ""
           )


toGP2 : VisualGraph -> String
toGP2 graph =
    "[\n"
        ++(List.foldl nodesToGP2 "" (Graph.nodes graph))
        ++"\t|\n"
        ++(List.foldl (edgesToGP2 graph) "" (Graph.edges graph))
        ++"]"


internalId : String -> VisualGraph -> Maybe NodeId
internalId id graph =
    Graph.nodes graph
        |> List.filter (.label >> Tuple.first >> .id >> (==) id)
        |> List.head
        |> Maybe.map .id


setLabel : String -> Label -> Label
setLabel label meta =
    { meta | label = label }


setId : String -> Label -> Label
setId id meta =
    { meta | id = id }


setMark : Mark -> Label -> Label
setMark mark meta =
    { meta | mark = mark }


setFlag : Bool -> Label -> Label
setFlag flag meta =
    { meta | flag = flag }


isValid : VisualGraph -> Bool
isValid graph =
    True


nextId : Graph n e -> NodeId
nextId graph =
    Graph.nodeIdRange graph
        |> Maybe.map (Tuple.second >> (+) 1)
        |> Maybe.withDefault 0


tryId : Int -> String -> List String -> String
tryId n prefix ids =
    let
        attempt =
            prefix ++ String.fromInt n
    in
    if List.member attempt ids then
        tryId (n + 1) prefix ids

    else
        attempt


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


setNodeMajor : NodeId -> Float -> VisualGraph -> VisualGraph
setNodeMajor id major graph =
    Graph.update
        id
        (Ellipse.setMajor major |> Tuple.mapSecond |> updateNode |> Maybe.map)
        graph


setNodeMinor : NodeId -> Float -> VisualGraph -> VisualGraph
setNodeMinor id minor graph =
    Graph.update
        id
        (Ellipse.setMinor minor |> Tuple.mapSecond |> updateNode |> Maybe.map)
        graph


updateNodeLabel : NodeId -> String -> VisualGraph -> VisualGraph
updateNodeLabel id label graph =
    Graph.update
        id
        (Tuple.mapFirst (setLabel label) |> updateNode |> Maybe.map)
        graph


updateEdgeLabel : NodeId -> NodeId -> Int -> String -> VisualGraph -> VisualGraph
updateEdgeLabel from to id label graph =
    Graph.update
        from
        (List.Extra.updateAt id (setLabel label) |> updateOutgoing to |> Maybe.map)
        graph


updateNodeId : NodeId -> String -> VisualGraph -> VisualGraph
updateNodeId node id graph =
    Graph.update
        node
        (Tuple.mapFirst (setId id) |> updateNode |> Maybe.map)
        graph


updateEdgeId : NodeId -> NodeId -> Int -> String -> VisualGraph -> VisualGraph
updateEdgeId from to edge id graph =
    Graph.update
        from
        (List.Extra.updateAt edge (setId id) |> updateOutgoing to |> Maybe.map)
        graph


updateNodeMark : NodeId -> Mark -> VisualGraph -> VisualGraph
updateNodeMark id mark graph =
    Graph.update
        id
        (Tuple.mapFirst (setMark mark) |> updateNode |> Maybe.map)
        graph


updateEdgeMark : NodeId -> NodeId -> Int -> Mark -> VisualGraph -> VisualGraph
updateEdgeMark from to id mark graph =
    Graph.update
        from
        (List.Extra.updateAt id (setMark mark) |> updateOutgoing to |> Maybe.map)
        graph


updateNodeFlag : NodeId -> Bool -> VisualGraph -> VisualGraph
updateNodeFlag id flag graph =
    Graph.update
        id
        (Tuple.mapFirst (setFlag flag) |> updateNode |> Maybe.map)
        graph


updateEdgeFlag : NodeId -> NodeId -> Int -> Bool -> VisualGraph -> VisualGraph
updateEdgeFlag from to id flag graph =
    Graph.update
        from
        (List.Extra.updateAt id (setFlag flag) |> updateOutgoing to |> Maybe.map)
        graph

getNodeData : NodeId -> VisualGraph -> Maybe Label
getNodeData id graph =
    Graph.get id graph
        |> Maybe.map (.node >> .label >> Tuple.first)


getEdgeData : NodeId -> NodeId -> Int -> VisualGraph -> Maybe Label
getEdgeData from to id graph =
    Graph.get from graph
        |> Maybe.map .outgoing
        |> Maybe.andThen (IntDict.get to)
        |> Maybe.andThen (List.Extra.getAt id)
