module Editor.Graph exposing (Model, Msg(..), Selection(..), init, update, view)


import AssocList as Dict exposing (Dict)
import Browser.Dom as Dom
import GP2Graph.GP2Graph as GP2Graph exposing (VisualGraph)
import Geometry.Arc as Arc exposing (Arc)
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import Geometry.LineSegment as LineSegment exposing (LineSegment)
import Geometry.Vec2 as Vec2 exposing (Vec2)
import Graph exposing (Edge, Graph, Node, NodeId)
import Graphics.Colour as Colour exposing (Colour)
import Html
import Html.Attributes
import Html.Events
import IntDict exposing (IntDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ports.Editor
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import File.Download as Download
import Task


type Selection
    = NodeSelection NodeId
    | EdgeSelection NodeId NodeId Int
    | NullSelection


type Action
    = MoveNode NodeId Vec2
    | DrawEdge NodeId Vec2
    | NullAction


type Msg
    = Create Vec2
    | CreateEdge NodeId
    | Move Vec2
    | Action Action
    | Select Selection
    | Delete Selection
    | Fit NodeId Float
    | UpdateId Selection String
    | UpdateLabel Selection String
    | UpdateMark Selection GP2Graph.Mark
    | UpdateFlag Selection Bool
    | SaveGP2
    | NullMsg


type alias Model =
    { graph : VisualGraph
    , action : Action
    , selection : Selection
    , id : String
    , host : Bool
    }


type Style
    = StrokeDefault
    | StrokeSelected
    | FillDefault
    | MarkAny
    | MarkGrey
    | MarkRed
    | MarkGreen
    | MarkBlue


arrowSide : Float
arrowSide =
    10


arrowAltitude : Float
arrowAltitude =
    sqrt 3 / 2 * arrowSide


radius : Float
radius =
    25


init : VisualGraph -> String -> Bool -> ( Model, Cmd Msg )
init graph id host =
    let
        model =
            { graph = graph
            , action = NullAction
            , selection = NullSelection
            , id = id
            , host = host
            }

        commands =
            Ports.Editor.editorInit (Encode.string id)
                :: List.map
                    (.id >> fitLabel id)
                    (Graph.nodes graph)
    in
    ( model, Cmd.batch commands )


update : String -> String -> Msg -> Model -> ( Model, Cmd Msg )
update nextNodeId nextEdgeId msg model =
    case msg of
        Create coords ->
            ( { model | graph = createNode nextNodeId coords model.graph }, Cmd.none )

        CreateEdge to ->
            ( shouldCreateEdge nextEdgeId to model, Cmd.none )

        Move coords ->
            ( mouseMoved coords model, Cmd.none )

        Action action ->
            ( { model
                | action = action
                , selection = shouldSelect action model.selection
              }
            , Cmd.none
            )

        Select selection ->
            ( { model | selection = selection }, Cmd.none )

        Delete selection ->
            ( { model
                | graph = delete selection model.graph
                , selection = shouldDeselect selection model.selection
              }
            , Cmd.none
            )

        Fit id width ->
            ( { model
                | graph = GP2Graph.setNodeMajor id (Basics.max radius (width / 2)) model.graph
              }
            , Cmd.none
            )

        UpdateId selection id ->
            ( { model
                | graph = setId selection id model.graph
              }
            , Cmd.none
            )

        UpdateLabel selection label ->
            ( { model
                | graph = setLabel selection label model.graph
              }
            , shouldFit selection model.id
            )

        UpdateMark selection mark ->
            ( { model
                | graph = setMark selection mark model.graph
              }
            , Cmd.none
            )

        UpdateFlag selection flag ->
            ( { model
                | graph = setFlag selection flag model.graph
              }
            , Cmd.none
            )

        SaveGP2 ->
            ( model, Download.string "graph.host" "text/plain" (GP2Graph.toGP2 model.graph))

        NullMsg ->
            ( model, Cmd.none )


fitLabel : String -> NodeId -> Cmd Msg
fitLabel id nodeid =
    String.fromInt nodeid
        |> (++) id
        |> Dom.getElement
        |> Task.attempt
            (Result.map (.element >> .width >> (+) 15 >> Fit nodeid)
                >> Result.withDefault NullMsg
            )


shouldFit : Selection -> String -> Cmd Msg
shouldFit selection id =
    case selection of
        NodeSelection nodeid ->
            fitLabel id nodeid

        _ ->
            Cmd.none


setLabel : Selection -> String -> VisualGraph -> VisualGraph
setLabel selection label graph =
    case selection of
        NodeSelection id ->
            GP2Graph.updateNodeLabel id label graph

        EdgeSelection from to id ->
            GP2Graph.updateEdgeLabel from to id label graph

        NullSelection ->
            graph


setId : Selection -> String -> VisualGraph -> VisualGraph
setId selection id graph =
    case selection of
        NodeSelection node ->
            GP2Graph.updateNodeId node id graph

        EdgeSelection from to edge ->
            GP2Graph.updateEdgeId from to edge id graph

        NullSelection ->
            graph


setMark : Selection -> GP2Graph.Mark -> VisualGraph -> VisualGraph
setMark selection mark graph =
    case selection of
        NodeSelection id ->
            GP2Graph.updateNodeMark id mark graph

        EdgeSelection from to id ->
            GP2Graph.updateEdgeMark from to id mark graph

        NullSelection ->
            graph


setFlag : Selection -> Bool -> VisualGraph -> VisualGraph
setFlag selection flag graph =
    case selection of
        NodeSelection id ->
            GP2Graph.updateNodeFlag id flag graph

        EdgeSelection from to id ->
            GP2Graph.updateEdgeFlag from to id flag graph

        NullSelection ->
            graph


mouseMoved : Vec2 -> Model -> Model
mouseMoved coords model =
    case model.action of
        MoveNode id offset ->
            { model | graph = GP2Graph.setNodePosition id (Vec2.add coords offset) model.graph }

        DrawEdge id _ ->
            { model | action = DrawEdge id coords }

        NullAction ->
            model


shouldCreateEdge : String -> NodeId -> Model -> Model
shouldCreateEdge id to model =
    case model.action of
        DrawEdge from _ ->
            { model | graph = createEdge id from to model.graph }

        _ ->
            model


shouldDeselect : Selection -> Selection -> Selection
shouldDeselect deleted current =
    if deleted == current then
        NullSelection

    else
        current


shouldSelect : Action -> Selection -> Selection
shouldSelect action selection =
    case action of
        MoveNode id _ ->
            NodeSelection id

        _ ->
            selection


newLabel : String -> GP2Graph.Label
newLabel id =
    { label = ""
    , id = id
    , mark = GP2Graph.None
    , flag = False
    }


createNode : String -> Vec2 -> VisualGraph -> VisualGraph
createNode id coords graph =
    GP2Graph.createNode
        ( newLabel id, Ellipse.fromCircle radius coords )
        graph


createEdge : String -> NodeId -> NodeId -> VisualGraph -> VisualGraph
createEdge id from to graph =
    GP2Graph.createEdge
        (newLabel id)
        from
        to
        graph


delete : Selection -> VisualGraph -> VisualGraph
delete selection graph =
    case selection of
        NodeSelection id ->
            Graph.remove id graph

        EdgeSelection from to id ->
            GP2Graph.deleteEdge from to id graph

        NullSelection ->
            graph


view : Model -> Html.Html Msg
view model =
    background
        :: (Dict.map (\k v -> arrow True k (v ++ "-start")) markerIds |> Dict.values)
        ++ (Dict.map (\k v -> arrow False k (v ++ "-end")) markerIds |> Dict.values)
        ++ List.concatMap (makeEdges model) (Graph.edges model.graph)
        ++ List.map (makeNode model) (Graph.nodes model.graph)
        ++ drawingEdge model
        |> svgContainer model


coordsDecoder : Decoder Vec2
coordsDecoder =
    Decode.map2 Vec2
        (Decode.field "detail" (Decode.field "x" Decode.float))
        (Decode.field "detail" (Decode.field "y" Decode.float))


createDecoder : Decoder Msg
createDecoder =
    Decode.map Create coordsDecoder


deleteDecoder : Msg -> Decoder Msg
deleteDecoder msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Delete" then
                    Decode.succeed msg

                else
                    Decode.fail "Non-delete key pressed - doing nothing"
            )


startMoveDecoder : Node ( GP2Graph.Label, Ellipse ) -> Decoder Msg
startMoveDecoder node =
    coordsDecoder
        |> Decode.map
            (Vec2.sub (Tuple.second node.label).center
                >> MoveNode node.id
                >> Action
            )


startEdgeDecoder : NodeId -> Decoder Msg
startEdgeDecoder nodeId =
    coordsDecoder
        |> Decode.map (DrawEdge nodeId >> Action)


moveDecoder : Decoder Msg
moveDecoder =
    Decode.map Move coordsDecoder


svgContainer : Model -> List (Svg Msg) -> Html.Html Msg
svgContainer model contents =
    svg
        [ class "graph-editor"
        , on "svgrightup" (Decode.succeed (Action NullAction))
        , on "svgleftup" (Decode.succeed (Action NullAction))
        , on "svgmousemove" moveDecoder
        , on "mouseleave" (Decode.succeed (Action NullAction))
        , id model.id
        , Html.Attributes.attribute "tabindex" "0"
        , on "keydown" (deleteDecoder (Delete model.selection))
        ]
        contents


background : Svg Msg
background =
    rect
        [ width "100%"
        , height "100%"
        , fill "white"
        , on "svgdblclick" createDecoder
        , onClick (Select NullSelection)
        ]
        []


arrow : Bool -> Style -> String -> Svg Msg
arrow start style name =
    marker
        [ id <| "arrow-" ++ name
        , refX <| String.fromFloat (if start then 0 else arrowAltitude)
        , refY <| String.fromFloat <| 0.5 * arrowSide
        , markerWidth <| String.fromFloat <| 2 * arrowSide
        , markerHeight <| String.fromFloat <| 2 * arrowSide
        , orient "auto"
        ]
        [ polygon
            [ points <|
                String.fromFloat (if start then arrowAltitude else 0)
                    ++ ","
                    ++ String.fromFloat arrowSide
                    ++ " "
                    ++ String.fromFloat (if start then arrowAltitude else 0)
                    ++ ",0 "
                    ++ String.fromFloat (if start then 0 else arrowAltitude)
                    ++ ","
                    ++ (String.fromFloat <| 0.5 * arrowSide)
            , fill (colour style |> Colour.toCss)
            ]
            []
        ]


makeNode : Model -> Node ( GP2Graph.Label, Ellipse ) -> Svg Msg
makeNode model node =
    let
        shape =
            Tuple.second node.label

        style =
            nodeStyle model node

        events =
            [ on "svgleftdown" (startMoveDecoder node)
            , on "svgrightdown" (startEdgeDecoder node.id)
            , on "svgrightup" (Decode.succeed (CreateEdge node.id))
            ]
    in
    g
        []
        [ ellipse (Ellipse.toSvg shape ++ style ++ events) []
        , text_
            [ x (String.fromFloat shape.center.x)
            , y (String.fromFloat (shape.center.y + shape.minor + 15))
            , fontSize "15"
            , textAnchor "middle"
            , pointerEvents "none"
            ]
            [ text (if model.host then "" else (Tuple.first node.label).id) ]
        , text_
            [ x (String.fromFloat shape.center.x)
            , y (String.fromFloat shape.center.y)
            , fontSize "15"
            , textAnchor "middle"
            , pointerEvents "none"
            , dominantBaseline "central"
            , id (model.id ++ String.fromInt node.id)
            ]
            [ text (Tuple.first node.label).label ]
        ]


makeEdges : Model -> Edge (List GP2Graph.Label) -> List (Svg Msg)
makeEdges model edges =
    List.indexedMap
        (makeEdge model edges.from edges.to)
        edges.label


makeEdge : Model -> NodeId -> NodeId -> Int -> GP2Graph.Label -> Svg Msg
makeEdge model from to id edge =
    let
        maybeShape =
            if from == to then
                Maybe.map
                    (reflexivePath model id)
                    (Graph.get from model.graph)

            else
                Maybe.map2
                    (edgePath model id)
                    (Graph.get from model.graph)
                    (Graph.get to model.graph)

        visibleStyle =
            edgeStyle model from to id edge

        hiddenStyle =
            [ strokeWidth "15"
            , visibility "hidden"
            ]

        visibleEvents =
            [ pointerEvents "none"
            ]

        hiddenEvents =
            [ onClick (Select (EdgeSelection from to id))
            , pointerEvents "stroke"
            ]
    in
    case maybeShape of
        Nothing ->
            Svg.text "unknown"

        Just shape ->
            g
                []
                [ Svg.path (Arc.toSvg shape ++ visibleEvents ++ visibleStyle) []
                , Svg.path (Arc.toSvg shape ++ hiddenEvents ++ hiddenStyle) []
                , text_
                    [ fontSize "15"
                    , pointerEvents "none"
                    , x (String.fromFloat (Arc.peak shape).x)
                    , y (String.fromFloat (Arc.peak shape).y)
                    , dy "-10"
                    , textAnchor "middle"
                    , transform
                        ("rotate("
                            ++ String.fromFloat (Vec2.angle shape.start shape.end)
                            ++ ","
                            ++ String.fromFloat (Arc.peak shape).x
                            ++ ","
                            ++ String.fromFloat (Arc.peak shape).y
                            ++ ")"
                        )
                    ]
                    [ text edge.label ]
                ]


drawingEdge : Model -> List (Svg Msg)
drawingEdge { graph, action } =
    case action of
        DrawEdge id coords ->
            Maybe.map
                (edgeToPoint coords)
                (Graph.get id graph)
                |> Maybe.withDefault []

        _ ->
            []


edgeToPoint : Vec2 -> GP2Graph.VisualContext -> List (Svg Msg)
edgeToPoint coords { node } =
    let
        nodeEllipse =
            Tuple.second node.label

        path =
            LineSegment.toSvg
                { start = Ellipse.project coords nodeEllipse
                , end = coords
                }

        lineEnd =
            Vec2.direction nodeEllipse.center coords
                |> Vec2.mul arrowAltitude
                |> Vec2.sub coords

        style =
            [ strokeWidth "1"
            , pointerEvents "none"
            , stroke <| Colour.toCss <| colour StrokeDefault
            , markerEnd
                (Dict.get StrokeDefault markerIds
                    |> Maybe.map (\marker -> "url(#arrow-" ++ marker ++ "-end)")
                    |> Maybe.withDefault "none"
                )
            ]
    in
    if not (Ellipse.contains lineEnd nodeEllipse) then
        [ line (path ++ style) [] ]

    else
        []


edgePath : Model -> Int -> GP2Graph.VisualContext -> GP2Graph.VisualContext -> Arc
edgePath { graph } id from to =
    let
        fromEllipse =
            from.node.label |> Tuple.second

        toEllipse =
            to.node.label |> Tuple.second

        line =
            Ellipse.lineBetween fromEllipse toEllipse

        intersects =
            Graph.nodes graph
                |> List.filter
                    (\node -> node /= from.node && node /= to.node)
                |> List.any
                    (.label >> Tuple.second >> Ellipse.intersects line)

        bidir =
            IntDict.member from.node.id to.outgoing

        bend =
            if intersects || bidir then
                id + 1

            else
                id + 0
    in
    { start = line.start
    , end = line.end
    , major = LineSegment.length line
    , minor = toFloat bend * radius * 10
    , sweep = False
    }


reflexivePath : Model -> Int -> GP2Graph.VisualContext -> Arc
reflexivePath { graph } id { node } =
    let
        start =
            Tuple.second node.label |> Ellipse.upperArc |> Tuple.first

        end =
            Tuple.second node.label |> Ellipse.upperArc |> Tuple.second
    in
    { start = start
    , end = end
    , major = 0.6 * Vec2.distance start end * ((toFloat id * 0.05) + 1)
    , minor = radius * ((toFloat id * 0.6) + 1)
    , sweep = True
    }


colour : Style -> Colour
colour style =
    case style of
        StrokeDefault ->
            { r = 0, g = 0, b = 0, a = 1.0 }

        StrokeSelected ->
            { r = 222, g = 145, b = 22, a = 1.0 }

        FillDefault ->
            { r = 255, g = 255, b = 255, a = 1.0 }

        MarkAny ->
            { r = 235, g = 72, b = 148, a = 1.0 }

        MarkGrey ->
            { r = 184, g = 184, b = 184, a = 1.0 }

        MarkRed ->
            { r = 235, g = 46, b = 66, a = 1.0 }

        MarkGreen ->
            { r = 35, g = 111, b = 98, a = 1.0 }

        MarkBlue ->
            { r = 85, g = 170, b = 235, a = 1.0 }

markerIds : Dict Style String
markerIds =
    Dict.fromList
        [ ( StrokeDefault, "default" )
        , ( StrokeSelected, "selected" )
        , ( MarkAny, "any" )
        , ( MarkRed, "red" )
        , ( MarkGreen, "green" )
        , ( MarkBlue, "blue" )
        ]


markToStyle : Style -> GP2Graph.Mark -> Style
markToStyle default mark =
    case mark of
        GP2Graph.Any ->
            MarkAny

        GP2Graph.Grey ->
            MarkGrey

        GP2Graph.Red ->
            MarkRed

        GP2Graph.Green ->
            MarkGreen

        GP2Graph.Blue ->
            MarkBlue

        _ ->
            default


nodeStyle : Model -> Node ( GP2Graph.Label, Ellipse ) -> List (Attribute Msg)
nodeStyle { selection } node =
    let
        fillColour =
            markToStyle FillDefault (Tuple.first node.label).mark |> colour

        strokeColour =
            if selection == NodeSelection node.id then
                colour StrokeSelected

            else
                colour StrokeDefault
    in
    [ strokeWidth (if (Tuple.first node.label).flag then "4" else "2")
    , fill <| Colour.toCss fillColour
    , stroke <| Colour.toCss strokeColour
    ]


edgeStyle : Model -> NodeId -> NodeId -> Int -> GP2Graph.Label -> List (Attribute Msg)
edgeStyle { selection } from to id edge =
    let
        strokeStyle =
            if selection == EdgeSelection from to id then
                StrokeSelected

            else
                markToStyle StrokeDefault edge.mark

        markerId suffix =
            Dict.get strokeStyle markerIds
                |> Maybe.map (\style -> "url(#arrow-" ++ style ++ suffix ++ ")")
                |> Maybe.withDefault "none"

        markerBi =
            if edge.flag then
                markerId "-start"

            else
                "none"

        dashed =
            if edge.mark == GP2Graph.Dashed then
                "10 5"

            else
                "0"
    in
    [ strokeWidth "1"
    , stroke <| Colour.toCss <| colour strokeStyle
    , fillOpacity "0"
    , markerEnd (markerId "-end")
    , markerStart markerBi
    , strokeDasharray dashed
    ]
