module Editor.Graph exposing (Model, Msg(..), init, update, view)

--TODO: Remove this

import Browser
import Graphics.Colour as Colour exposing (Colour)
import AssocList as Dict exposing (Dict)
import GP2Graph.GP2Graph as GP2Graph exposing (VisualGraph)
import Geometry.Arc as Arc exposing (Arc)
import Geometry.LineSegment as LineSegment exposing (LineSegment)
import Geometry.Vec2 as Vec2 exposing (Vec2)
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import Graph exposing (Graph, NodeId, Node, Edge)
import Html
import Html.Attributes
import Html.Events
import IntDict exposing (IntDict)
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


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
    | CreateEdge NodeId NodeId
    | Move NodeId Vec2
    | Action Action
    | Select Selection
    | Delete Selection
    | NullMsg


type alias Model =
    { graph : VisualGraph
    , action : Action
    , selection : Selection
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


main =
    Browser.sandbox
        { init = init Graph.empty
        , update = update
        , view = view
        }


init : VisualGraph -> Model
init graph =
    { graph = graph, action = NullAction, selection = NullSelection }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Create coords ->
            { model | graph = createNode coords model.graph }

        CreateEdge from to ->
            { model | graph = createEdge from to model.graph }

        Move id coords ->
            { model | graph = GP2Graph.setNodePosition id coords model.graph }

        Action action ->
            { model 
                | action = action
                , selection = shouldSelect action model.selection
            }

        Select selection ->
            { model | selection = selection }

        Delete selection ->
            { model 
                | graph = delete selection model.graph
                , selection = shouldDeselect selection model.selection
            }

        NullMsg ->
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
            

newLabel : (VisualGraph -> String) -> VisualGraph -> GP2Graph.Label
newLabel idGenerator graph =
    { label = ""
    , id = idGenerator graph
    , mark = GP2Graph.None
    }


createNode : Vec2 -> VisualGraph -> VisualGraph
createNode coords graph =
    GP2Graph.createNode 
        ( newLabel GP2Graph.nextEdgeId graph, Ellipse.fromCircle radius coords )
        graph


createEdge : NodeId -> NodeId -> VisualGraph -> VisualGraph
createEdge from to graph =
    GP2Graph.createEdge
        (newLabel GP2Graph.nextNodeId graph)
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
        :: (Dict.map arrow markerIds |> Dict.values)
        ++ List.concatMap (makeEdges model) (Graph.edges model.graph)
        ++ List.map (makeNode model) (Graph.nodes model.graph)
        ++ drawingEdge model
        |> svgContainer model
        |> List.singleton
        |> container model


coordsDecoder : Decoder Vec2
coordsDecoder =
    Decode.map2 Vec2
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


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


preventDecoder : Decoder (Msg, Bool)
preventDecoder =  
    Decode.map (\a -> (a, True)) (Decode.succeed NullMsg)


startMoveDecoder : Node (GP2Graph.Label, Ellipse) -> Decoder Msg
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
        |> Decode.map (\coords -> Action (DrawEdge nodeId coords))


createEdgeDecoder : Model -> NodeId -> Decoder Msg
createEdgeDecoder { action } to =
    case action of
        DrawEdge from _ ->
            Decode.succeed (CreateEdge from to)

        _ ->
            Decode.fail "Creating edge with no start"


moveDecoder : Model -> Decoder Msg
moveDecoder { action } =
    case action of
        MoveNode id coords ->
            coordsDecoder
                |> Decode.map (Vec2.add coords >> Move id)

        DrawEdge id coords ->
            coordsDecoder
                |> Decode.map (DrawEdge id >> Action)

        _ ->
            Decode.fail "Mouse moved with no current action - doing nothing"
            


clickDecoder : Decoder Msg -> Decoder Msg -> Decoder Msg -> Decoder Msg
clickDecoder left right default =
   Decode.field "button" Decode.int
        |> Decode.andThen
            (\button ->
                case button of
                    0 ->
                        left

                    2 ->
                        right

                    _ ->
                        default
            )


container : Model -> List (Html.Html Msg) -> Html.Html Msg
container { selection } contents =
    Html.div
        [ Html.Attributes.style "outline" "none"
        , Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "line-height" "0"
        , Html.Attributes.tabindex 0
        , preventDefaultOn "contextmenu" preventDecoder
        , on "keydown" (deleteDecoder (Delete selection))
        ]
        contents


svgContainer : Model -> List (Svg Msg) -> Html.Html Msg
svgContainer model contents =
    svg
        [ width "1024"
        , height "768"
        , viewBox "0 0 1024 768"
        , onMouseUp (Action NullAction)
        , on "mousemove" (moveDecoder model)
        , on "mouseleave" (Decode.succeed (Action NullAction))
        ]
        contents


background : Svg Msg
background =
    rect
        [ width "1024"
        , height "768"
        , fill "white"
        , stroke "black"
        , on "dblclick" createDecoder
        , onClick (Select NullSelection)
        ]
        []


arrow : Style -> String -> Svg Msg
arrow style name =
    marker
        [ id <| "arrow-" ++ name
        , refX <| String.fromFloat arrowAltitude
        , refY <| String.fromFloat <| 0.5 * arrowSide
        , markerWidth <| String.fromFloat <| 2 * arrowSide
        , markerHeight <| String.fromFloat <| 2 * arrowSide
        , orient "auto"
        ]
        [ polygon
            [ points <|
                "0,"
                    ++ String.fromFloat arrowSide
                    ++ " 0,0 "
                    ++ String.fromFloat arrowAltitude
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
            Tuple.second node.label |> Ellipse.toSvg

        style =
            nodeStyle model node

        events =
            [ on "mousedown" 
                <| clickDecoder 
                    (startMoveDecoder node)
                    (startEdgeDecoder node.id) 
                    (Decode.fail "Non standard click on node - doing nothing")
            , on "mouseup" (createEdgeDecoder model node.id)
            , onClick (Select (NodeSelection node.id))
            ]
    in ellipse (shape ++ style ++ events) []


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
                [ Svg.path (shape ++ visibleEvents ++ visibleStyle) []
                , Svg.path (shape ++ hiddenEvents ++ hiddenStyle) []
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
            , stroke <| Colour.toCss <| colour StrokeDefault
            , markerEnd 
                (Dict.get StrokeDefault markerIds
                    |> Maybe.map (\marker -> "url(#arrow-" ++ marker ++ ")")
                    |> Maybe.withDefault "none"
                )
            ]
    in
    if not (Ellipse.contains lineEnd nodeEllipse) then
        [ line (path ++ style) [] ]

    else
        []


edgePath : Model -> Int -> GP2Graph.VisualContext -> GP2Graph.VisualContext -> List (Attribute msg)
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
    Arc.toSvg
        { start = line.start
        , end = line.end
        , major = LineSegment.length line
        , minor = toFloat bend * radius * 10
        , sweep = False
        }


reflexivePath : Model -> Int -> GP2Graph.VisualContext -> List (Attribute msg)
reflexivePath { graph } id { node } =
    Arc.toSvg
        { start = Tuple.second node.label |> Ellipse.upperArc |> Tuple.first
        , end = Tuple.second node.label |> Ellipse.upperArc |> Tuple.second
        , major = 0.8 * radius * ((toFloat id * 0.1) + 1)
        , minor = radius * ((toFloat id * 0.4) + 1)
        , sweep = True
        }


colour : Style -> Colour
colour style =
    case style of
        StrokeDefault ->
            { r = 0, g = 0, b = 0, a = 1.0 }

        StrokeSelected ->
            { r = 255, g = 0, b = 0, a = 1.0 }

        FillDefault ->
            { r = 255, g = 255, b = 255, a = 1.0 }

        MarkAny ->
            { r = 255, g = 127, b = 255, a = 1.0 }

        MarkGrey ->
            { r = 127, g = 127, b = 127, a = 1.0 }

        MarkRed ->
            { r = 255, g = 0, b = 0, a = 1.0 }

        MarkGreen ->
            { r = 0, g = 255, b = 0, a = 1.0 }

        MarkBlue ->
            {r = 0, g = 0, b = 255, a = 1.0 }


markerIds : Dict Style String
markerIds =
    Dict.fromList 
        [ (StrokeDefault, "default")
        , (StrokeSelected, "selected")
        , (MarkAny, "any")
        , (MarkRed, "red")
        , (MarkGreen, "green")
        , (MarkBlue, "blue")
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
    [ strokeWidth "2"
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

        markerId =
            Dict.get strokeStyle markerIds
                |> Maybe.map (\style -> "url(#arrow-" ++ style ++ ")")
                |> Maybe.withDefault "none"
    in
    [ strokeWidth "1"
    , stroke <| Colour.toCss <| colour strokeStyle
    , fillOpacity "0"
    , markerEnd markerId
    ]
