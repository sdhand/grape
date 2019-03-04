module Geometry.Arc exposing (Arc, toSvg)


import Svg
import Svg.Attributes exposing (..)
import Geometry.Vec2 as Vec2 exposing (Vec2)


type alias Arc =
    { start : Vec2
    , end : Vec2
    , major : Float
    , minor : Float
    , sweep : Bool
    }


toSvg : Arc -> List (Svg.Attribute msg)
toSvg { start, end, major, minor, sweep } =
    [ d ("M "
        ++ String.fromFloat start.x
        ++ " "
        ++ String.fromFloat start.y
        ++ " A "
        ++ String.fromFloat major
        ++ " "
        ++ String.fromFloat minor
        ++ " "
        ++ (String.fromFloat <| Vec2.angle start end)
        ++ " "
        ++ (if sweep then "1" else "0")
        ++ " 1 "
        ++ String.fromFloat end.x
        ++ " "
        ++ String.fromFloat end.y)
    ] 
