module Main exposing (Action(..), Coords, Model, Msg(..), clickDecoder, coordsDecoder, getNodePos, main, makeNode, moveNode, nextId, update, view)

import Browser
import Dict exposing (Dict)
import Graph exposing (Graph, Node, NodeContext, NodeId, Edge)
import IntDict
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (..)
import Html
import Html.Attributes
import Html.Events
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type Action
    = MoveNode NodeId Coords
    | DrawEdge NodeId Coords
    | NullAction


type Selection
    = NodeSelection NodeId
    | EdgeSelection NodeId NodeId Int
    | NullSelection


type Msg
    = Create Coords
    | StartMove NodeId Coords
    | StartEdge NodeId Coords
    | CreateEdge NodeId
    | Select Selection
    | Delete Selection
    | StopAction
    | NullMsg
    | Move Coords


main =
    Browser.sandbox { init = { graph = Graph.empty, action = NullAction, selection = NullSelection }, update = update, view = view }


type alias Coords =
    { x : Float, y : Float }


coordsDecoder : Decoder Coords
coordsDecoder =
    Decode.map2 Coords (Decode.field "pageX" Decode.float) (Decode.field "pageY" Decode.float)


deleteDecoder : Decoder ()
deleteDecoder =
    Decode.field "key" Decode.string |> Decode.andThen (\a -> if a == "Delete" then Decode.succeed () else Decode.fail a)


clickDecoder : Msg -> Msg -> Msg -> Decoder Msg
clickDecoder left right default =
    Decode.field "button" Decode.int
        |> Decode.map
            (\a ->
                case a of
                    0 ->
                        left

                    2 ->
                        right

                    _ ->
                        default
            )


type alias Model =
    { graph : Graph ( String, Coords ) (List String)
    , action : Action
    , selection : Selection
    }


nextId : Graph n e -> NodeId
nextId g =
    Graph.nodeIdRange g |> Maybe.map (Tuple.second >> (+) 1) >> Maybe.withDefault 0


getNodePos : NodeId -> Graph ( a, Coords ) e -> Maybe Coords
getNodePos i g =
    Graph.get i g |> Maybe.map (.node >> .label >> Tuple.second)


moveNode : Coords -> Maybe (NodeContext ( a, Coords ) e) -> Maybe (NodeContext ( a, Coords ) e)
moveNode c n =
    Maybe.map (\({ node } as context) -> { context | node = { node | label = Tuple.mapSecond (always c) node.label } }) n


createEdge : NodeId -> Maybe (NodeContext n (List String)) -> Maybe (NodeContext n (List String))
createEdge target n =
    Maybe.map (\context -> { context | outgoing = IntDict.update target (Maybe.map ((::)"" >> Just) >> Maybe.withDefault (Just [""])) context.outgoing }) n


update : Msg -> Model -> Model
update msg model =
    case msg of
        Create coords ->
            { model | graph = Graph.insert { node = Node (nextId model.graph) ( "", coords ), incoming = IntDict.empty, outgoing = IntDict.empty } model.graph }

        StartMove id coords ->
            { model | action = Maybe.map (\n -> MoveNode id { x = n.x - coords.x, y = n.y - coords.y }) (getNodePos id model.graph) |> Maybe.withDefault NullAction, selection = NodeSelection id }

        StartEdge id coords ->
            { model | action = DrawEdge id coords }

        CreateEdge id2 ->
            case model.action of
                NullAction ->
                    model
                MoveNode _ _ ->
                    model
                DrawEdge id1 _ ->
                    { model | graph = Graph.update id1 (createEdge id2) model.graph }

        NullMsg ->
            model

        StopAction ->
            { model | action = NullAction }


        Select selection ->
            { model | selection = selection }


        Delete selection ->
            { model | selection = NullSelection, graph = delete model.selection model.graph }


        Move coords ->
            case model.action of
                NullAction ->
                    model

                MoveNode id offset ->
                    { model | graph = Graph.update id (moveNode { x = coords.x + offset.x, y = coords.y + offset.y }) model.graph }

                DrawEdge id _ ->
                    { model | action = DrawEdge id coords }


deleteEdge : Int -> List e -> Maybe (List e)
deleteEdge i edges =
    let
        removed = (List.take i edges)++(List.drop (i+1) edges)
    in
        if List.length removed == 0 then Nothing else Just removed


delete : Selection -> Graph n (List e) -> Graph n (List e)
delete selection graph =
    case selection of
        NodeSelection id ->
            Graph.remove id graph

        EdgeSelection from to id ->
                Graph.update from (Maybe.map (\context -> { context | outgoing = IntDict.update to (Maybe.andThen (deleteEdge id)) context.outgoing })) graph

        NullSelection ->
            graph


radius : Float
radius = 25

majorAxisScale : Float
majorAxisScale = 1.2


intersectionCE: (Coords, Coords)
intersectionCE =
    let
        i1 = { x = (radius*sqrt (majorAxisScale*(majorAxisScale+2)))/(majorAxisScale+1), y = -radius/(majorAxisScale+1) }
    in
        ({ i1 | x = -i1.x }, i1)

unitDir : Coords -> Coords -> Coords
unitDir a b =
    let
        scale = distance a b
    in
        { x = (b.x-a.x)/scale, y = (b.y-a.y)/scale }

distance : Coords -> Coords -> Float
distance a b = sqrt((b.x-a.x)^2+(b.y-a.y)^2)

angle : Coords -> Coords -> Float
angle a b = 180*atan((b.y-a.y)/(b.x-a.x))/pi

intersects : Coords -> Coords -> Float -> Coords -> Bool
intersects l1 l2 r circ =
    let
        x1 = l1.x+(unitDir l1 l2).x*radius-circ.x
        x2 = l2.x+(unitDir l2 l1).x*radius-circ.x
        y1 = l1.y+(unitDir l1 l2).y*radius-circ.y
        y2 = l2.y+(unitDir l2 l1).y*radius-circ.y
        dx = x2-x1
        dy = y2-y1
        a = dx^2+dy^2
        b = 2*(x1*dx+y1*dy)
        c = x1^2+y1^2-r^2
        disc = b^2-4*a*c
        t1 = (-b+(sqrt disc))/(2*a)
        t2 = (-b-(sqrt disc))/(2*a)
    in
        disc >= 0 && ((0 <= t1 && t1 <= 1) || (0 <= t2 && t2 <= 1))


arrowheadLength : Float
arrowheadLength = 10


makeNode : Selection -> Node ( a, Coords ) -> Svg Msg
makeNode selection { id, label } =
    circle
      [ 
        cx <| String.fromFloat (Tuple.second label).x,
        cy <| String.fromFloat (Tuple.second label).y,
        r <| String.fromFloat radius,
        stroke <| if selection == NodeSelection id then "red" else "black",
        strokeWidth "2",
        fill "white",
        on "mousedown" (coordsDecoder |> Decode.andThen (\a -> clickDecoder (StartMove id a) (StartEdge id a) NullMsg)),
        on "mouseup" (clickDecoder NullMsg (CreateEdge id) NullMsg)
      ] []


edgePath : Float -> Coords -> Coords -> String
edgePath r start end =
    "M "++
        (String.fromFloat start.x)++" "++
        (String.fromFloat start.y)++" "++
    "A "++
        (String.fromFloat <| distance start end)++" "++
        (String.fromFloat r)++" "++
        (String.fromFloat <| angle start end)++" "++
        "0 1 "++
        (String.fromFloat end.x)++" "++
        (String.fromFloat end.y)


reflexivePath : Int -> Coords -> String
reflexivePath count coords =
    "M "++
        (String.fromFloat (coords.x + (Tuple.first intersectionCE |> .x)))++" "++
        (String.fromFloat (coords.y + (Tuple.first intersectionCE |> .y)))++" "++
    "A "++
        (String.fromFloat <| (((toFloat count)*0.1)+1)*radius)++" "++
        (String.fromFloat <| radius*(((toFloat count)*0.4)+1)*majorAxisScale)++" "++
        "0 1 1 "++
        (String.fromFloat (coords.x + (Tuple.second intersectionCE |> .x)))++" "++
        (String.fromFloat (coords.y + (Tuple.second intersectionCE |> .y)))


drawArrow : String -> String -> Svg Msg
drawArrow colour path =
    Svg.path [
        d path,
        stroke colour,
        fillOpacity "0",
        strokeWidth "1",
        markerEnd <| "url(#arrow-"++colour++")",
        pointerEvents "none"
    ] []


clickArrow : String -> Msg -> Svg Msg
clickArrow path msg =
    Svg.path [
        d path,
        visibility "hidden",
        strokeWidth "15",
        pointerEvents "stroke",
        onMouseDown msg
    ] []



makeEdge : Selection -> Graph (a, Coords) (List e) -> Edge (List e) -> List (Svg Msg)
makeEdge selection g { from, to, label } =
    let oneEdge i l =
            case (Graph.get from g, Graph.get to g) of
            (Just node1, Just node2) ->
                let
                    n1 = Tuple.second node1.node.label
                    n2 = Tuple.second node2.node.label
                    ob = Graph.nodes g |> (List.filter (\a -> a.id /= from && a.id /= to) >> List.map (.label >> Tuple.second) >> List.any (intersects n1 n2 radius))
                    bidir = IntDict.member from node2.outgoing
                    unit = unitDir n1 n2
                    colour = if selection == EdgeSelection from to i then "red" else "black"
                    path =
                        if to == from then 
                            reflexivePath i n1
                        else
                            edgePath ((toFloat <| if ob || bidir then i+1 else i)*radius*10) { x = n1.x+unit.x*radius, y = n1.y+unit.y*radius } { x = n2.x-unit.x*radius, y = n2.y-unit.y*radius }
                in
                    [ drawArrow colour path, clickArrow path (Select <| EdgeSelection from to i) ]
            _ ->
                [ Svg.text "unknown", Svg.text "unknown" ]
    in List.indexedMap oneEdge label |> List.concat


-- We include a background rectangle to capture the create events instead of doing this at the top level to avoid clicks on the nodes themselves propagating

arrowhead : String -> Svg Msg
arrowhead colour =
    marker [
        id <| "arrow-"++colour,
        refX (String.fromFloat <| sqrt(3)/2*arrowheadLength),
        refY (String.fromFloat <| 1/2*arrowheadLength),
        markerWidth (String.fromFloat <| 2*arrowheadLength),
        markerHeight (String.fromFloat <| 2*arrowheadLength),
        orient "auto"
    ]
    [
        polygon [
            points <|
                "0,"++(String.fromFloat arrowheadLength)++" "++
                "0,0 "++
                (String.fromFloat <| sqrt(3)/2*arrowheadLength)++","++(String.fromFloat <| 1/2*arrowheadLength),
            fill colour
        ] []
    ]


view : Model -> Html.Html Msg
view model =
    Html.div [
        Html.Attributes.style "outline" "none",
        Html.Attributes.style "display" "inline-block",
        Html.Attributes.style "line-height" "0",
        Html.Attributes.tabindex 0,
        Html.Events.on "keydown" (deleteDecoder |> Decode.andThen (\_ -> Decode.succeed (Delete model.selection)))
    ]
    [
        svg
            [ width "1024"
            , height "768"
            , viewBox "0 0 1024 768"
            , onMouseUp StopAction
            , on "mouseleave" <| Decode.succeed StopAction
            , on "mousemove" (coordsDecoder |> Decode.map Move)
            , preventDefaultOn "contextmenu" <| Decode.map (\a -> ( a, True )) (Decode.succeed NullMsg)
            ]
            (rect [ x "0", y "0", width "1024", height "768", fill "white", stroke "black", on "dblclick" (coordsDecoder |> Decode.map Create), onClick (Select NullSelection) ] [] ::
            arrowhead "red"::
            arrowhead "black"::
            List.concatMap (makeEdge model.selection model.graph) (Graph.edges model.graph) ++ List.map (makeNode model.selection) (Graph.nodes model.graph)++
            (case model.action of
                DrawEdge id coords -> getNodePos id model.graph |> Maybe.map (\a -> if distance a coords > radius + sqrt(3)/2*arrowheadLength then [edgePath 0 {x = a.x+(unitDir a coords).x*radius, y = a.y+(unitDir a coords).y*radius } coords |> drawArrow "black" ] else [] ) |> Maybe.withDefault []
                _ -> []))
    ]
