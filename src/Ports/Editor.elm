port module Ports.Editor exposing (editorInit)

import Json.Encode exposing (Value)

port editorInit : Value -> Cmd msg
