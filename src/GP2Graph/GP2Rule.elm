module GP2Graph.GP2Rule exposing (GP2Rule, toGP2)


import GP2Graph.GP2Graph as GP2Graph


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
        ++ (if condition /= "" then "\n}\nwhere " ++ condition else "")
