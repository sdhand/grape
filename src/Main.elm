module Main exposing (main)

import Browser exposing (Document)
import Editor.Graph as GraphEditor
import GP2Graph.GP2Graph as GP2Graph exposing (VisualGraph)
import Graph
import Html exposing (..)


type alias Model =
    { editorModel : GraphEditor.Model }


type Msg
    = EditorMsg GraphEditor.Msg


testGraph : VisualGraph
testGraph =
    Graph.empty
        |> GP2Graph.createNode ( { label = "node 1", id = "n1", mark = GP2Graph.Red }, { major = 25, minor = 25, center = { x = 100, y = 100 } } )
        |> GP2Graph.createNode ( { label = "node 2", id = "n2", mark = GP2Graph.Blue }, { major = 25, minor = 25, center = { x = 500, y = 100 } } )
        |> GP2Graph.createNode ( { label = "node 3", id = "n3", mark = GP2Graph.Green }, { major = 25, minor = 25, center = { x = 900, y = 100 } } )
        |> GP2Graph.createNode ( { label = "node 4", id = "n4", mark = GP2Graph.Any }, { major = 25, minor = 25, center = { x = 100, y = 500 } } )
        |> GP2Graph.createNode ( { label = "node 5", id = "n5", mark = GP2Graph.Grey }, { major = 25, minor = 25, center = { x = 500, y = 500 } } )
        |> GP2Graph.createNode ( { label = "node 6", id = "n6", mark = GP2Graph.None }, { major = 25, minor = 25, center = { x = 900, y = 500 } } )
        |> GP2Graph.createEdge { label = "edge 1", id = "e1", mark = GP2Graph.None } 1 0
        |> GP2Graph.createEdge { label = "edge 2", id = "e2", mark = GP2Graph.Red } 0 3
        |> GP2Graph.createEdge { label = "edge 3", id = "e3", mark = GP2Graph.Blue } 1 2
        |> GP2Graph.createEdge { label = "edge 4", id = "e4", mark = GP2Graph.Green } 2 5
        |> GP2Graph.createEdge { label = "edge 5", id = "e5", mark = GP2Graph.Any } 1 4
        |> GP2Graph.createEdge { label = "edge 6", id = "e6", mark = GP2Graph.Dashed } 4 3
        |> GP2Graph.createEdge { label = "edge 7", id = "e7", mark = GP2Graph.None } 4 5
        |> GP2Graph.createEdge { label = "reflexive", id = "e8", mark = GP2Graph.None } 1 1
        


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( editorModel, editorCmd ) = GraphEditor.init testGraph "test"
    in
    ( { editorModel = editorModel }, Cmd.batch [ Cmd.map EditorMsg editorCmd ] )


view : Model -> Document Msg
view model =
    { title = "sushi"
    , body = GraphEditor.view model.editorModel
        |> Html.map EditorMsg 
        |> List.singleton
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            ( updateEditor model editorMsg, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


updateEditor : Model -> GraphEditor.Msg -> Model
updateEditor model msg =
    { model | editorModel = GraphEditor.update msg model.editorModel }
