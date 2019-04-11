module Main exposing (main)

import Browser exposing (Document)
import Editor.Graph as GraphEditor
import Editor.Element as ElementEditor exposing (Msg(..))
import GP2Graph.GP2Graph as GP2Graph exposing (VisualGraph)
import Graph
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { editorModel : GraphEditor.Model
    , elementModel : ElementEditor.Model
    }


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
        ( editorModel, editorCmd ) =
            GraphEditor.init Graph.empty "host" True

    in
    ( { editorModel = editorModel, elementModel = ElementEditor.init }, Cmd.map EditorMsg editorCmd )


view : Model -> Document Msg
view model =
    { title = "sushi"
    , body =
        [ div
            [ class "host-container" ]
            [ ElementEditor.view model.editorModel model.elementModel
            , GraphEditor.view model.editorModel
                |> Html.map EditorMsg
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nextNodeId =
            GP2Graph.tryId 0 "" (Graph.nodes model.editorModel.graph |> List.map (.label >> Tuple.first >> .id))

        nextEdgeId =
            GP2Graph.tryId 0 "" (Graph.edges model.editorModel.graph |> List.concatMap (.label >> List.map .id))
    in
    case msg of
        EditorMsg editorMsg ->
            let
                (newModel, cmd) =
                    updateEditor nextNodeId nextEdgeId editorMsg model
            in
            if newModel.editorModel.selection /= model.editorModel.selection then
                (updateElement (ElementEditor.Select newModel.editorModel.selection) newModel, cmd)

            else
                (newModel, cmd)

        ElementMsg elementMsg ->
            (updateElement elementMsg model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


updateEditor : String -> String -> GraphEditor.Msg -> Model -> ( Model, Cmd Msg )
updateEditor nextNodeId nextEdgeId msg model =
    let
        ( newEditor, cmd ) =
            GraphEditor.update nextNodeId nextEdgeId msg model.editorModel
    in
    ( { model | editorModel = newEditor }, Cmd.map EditorMsg cmd )


updateElement : ElementEditor.ElementMsg -> Model -> Model
updateElement msg model =
    { model | elementModel = ElementEditor.update model.editorModel msg model.elementModel }
