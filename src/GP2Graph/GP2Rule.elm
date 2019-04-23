module GP2Graph.GP2Rule exposing (GP2Rule, toGP2, toDot)


import GP2Graph.GP2Graph as GP2Graph
import Graph exposing (Graph)
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import DotLang exposing (Attr(..), Dot(..), EdgeRHS(..), EdgeType(..), ID(..), Stmt(..), AttrStmtType(..))


type alias GP2Rule =
    { vars : String
    , left : GP2Graph.VisualGraph
    , right : GP2Graph.VisualGraph
    , condition : String
    }


toGP2 : GP2Rule -> String
toGP2 { vars, left, right, condition } =
    vars
        ++ "\n"
        ++ (GP2Graph.toGP2 left)
        ++ "\n=>\n"
        ++ (GP2Graph.toGP2 right)
        ++ "\ninterface = \n{\n"
        ++ ((GP2Graph.interface left right)
            |> String.join ", ")
        ++ "\n}"
        ++ (if condition /= "" then "\nwhere " ++ condition else "")


nodeToDot : String -> String -> String -> String -> String -> Ellipse -> Stmt
nodeToDot shape id label colour style { center } =
    NodeStmt
        (DotLang.NodeId (ID id) Nothing)
        [ Attr (ID "pos") (ID (String.fromFloat center.x ++ "," ++ String.fromFloat center.y ++ "!"))
        , Attr (ID "label") (ID label)
        , Attr (ID "fillcolor") (ID colour)
        , Attr (ID "style") (ID style)
        , Attr (ID "shape") (ID shape)
        ]


dotEdge : String -> String -> String -> String -> String -> String -> String -> String -> Stmt
dotEdge shape id label colour direction style from to =
    EdgeStmtNode
        (DotLang.NodeId (ID from) Nothing)
        (EdgeNode (DotLang.NodeId (ID to) Nothing))
        []
        [ Attr (ID "label") (ID label)
        , Attr (ID "dir") (ID direction)
        , Attr (ID "color") (ID colour)
        , Attr (ID "style") (ID style)
        , Attr (ID "id") (ID id)
        , Attr (ID "arrowhead") (ID shape)
        , Attr (ID "arrowtail") (ID shape)
        ]


leftNodeToDot : GP2Graph.VisualGraph -> Graph.Node ( GP2Graph.Label, Ellipse ) -> Stmt
leftNodeToDot right node =
    case GP2Graph.getCorrespondingNode node right of
        Just { label } ->
            nodeToDot
                "ellipse"
                (Tuple.first node.label).id
                ((Tuple.first node.label).label++"/"++(Tuple.first label).label)
                ((GP2Graph.markToColour (Tuple.first node.label).mark |> Tuple.second)++";0.5:"++(GP2Graph.markToColour (Tuple.first label).mark |> Tuple.second))
                ("filled,"++(if (Tuple.first node.label).flag then "bold," else "solid,")++(if (Tuple.first label).flag then "bold" else "solid"))
                (Tuple.second node.label)

        Nothing ->
            nodeToDot
                "rect"
                (Tuple.first node.label).id
                (Tuple.first node.label).label
                (GP2Graph.markToColour (Tuple.first node.label).mark |> Tuple.second)
                ("filled,"++(if (Tuple.first node.label).flag then "bold" else "solid"))
                (Tuple.second node.label)


rightNodeToDot : GP2Graph.VisualGraph -> Graph.Node ( GP2Graph.Label, Ellipse ) -> Maybe Stmt
rightNodeToDot left node =
    case GP2Graph.getCorrespondingNode node left of
        Just _ ->
            Nothing

        Nothing ->
            nodeToDot
                "diamond"
                (Tuple.first node.label).id
                (Tuple.first node.label).label
                (GP2Graph.markToColour (Tuple.first node.label).mark |> Tuple.second)
                ("filled,"++(if (Tuple.first node.label).flag then "bold" else "solid"))
                (Tuple.second node.label)
                |> Just


edgeToDot : String -> String -> GP2Graph.VisualGraph -> String -> String -> GP2Graph.Label -> Stmt
edgeToDot side shape other from to edge =
    case GP2Graph.getCorrespondingEdge edge other of
        Just _ ->
            dotEdge
                shape
                (side++"_"++edge.id)
                edge.label
                (GP2Graph.markToColour edge.mark |> Tuple.first)
                (if edge.flag then "both" else "forward")
                (if edge.mark == GP2Graph.Dashed then "dashed" else "solid")
                from
                to

        Nothing ->
            dotEdge
                shape
                edge.id
                edge.label
                (GP2Graph.markToColour edge.mark |> Tuple.first)
                (if edge.flag then "both" else "forward")
                (if edge.mark == GP2Graph.Dashed then "dashed" else "solid")
                from
                to


edgesToDot : String -> String -> GP2Graph.VisualGraph -> GP2Graph.VisualGraph -> Graph.Edge (List GP2Graph.Label) -> Maybe (List Stmt)
edgesToDot side shape graph other edge =
    let
        from =
            Graph.get edge.from graph
                |> Maybe.map (.node >> .label >> Tuple.first >> .id)

        to =
            Graph.get edge.to graph
                |> Maybe.map (.node >> .label >> Tuple.first >> .id)
    in
    Maybe.map2
        ( \f t -> List.map (edgeToDot side shape other f t) edge.label) from to




toDot : GP2Rule -> String
toDot { vars, left, right, condition } =
    Dot
        DotLang.Digraph
        (Just (ID (vars++"="++condition)))
        (List.map (leftNodeToDot right) (Graph.nodes left) ++ List.filterMap (rightNodeToDot left) (Graph.nodes right)
        ++ (List.concat <| List.filterMap (edgesToDot "left" "obox" left right) (Graph.edges left)) ++ (List.concat <| List.filterMap (edgesToDot "right" "odiamond" right left) (Graph.edges right))
        )
        |> DotLang.toString
