port module Ports.Editor exposing (editorInit, layoutGraph, layoutDone)

import Json.Encode exposing (Value)

port editorInit : Value -> Cmd msg

port layoutGraph : Value -> Cmd msg

port layoutDone : (Value -> msg) -> Sub msg
