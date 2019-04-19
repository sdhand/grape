module GP2Graph.GP2Parser exposing (parse)


import Parser exposing (..)
import DotLang exposing (Dot(..), Stmt(..), ID(..), NodeId(..), AttrStmtType(..), EdgeRHS(..), EdgeType(..), Attr(..))
import GP2Graph.HostListParser exposing (hostListParser)


parse : String -> Result (List DeadEnd) Dot
parse graph =
    run gp2GraphParser graph


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')


lexeme : Parser a -> Parser a
lexeme parser =
    parser |. whitespace


negFloat : Parser Float
negFloat =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= float
        , float
        ]


gp2GraphParser : Parser Dot
gp2GraphParser =
    succeed (Dot Digraph Nothing)
        |. whitespace
        |. (symbol >> lexeme) "["
        |. oneOf [ succeed () |. position |. (symbol >> lexeme) "|" , succeed () ]
        |= stmtList
        |. (symbol >> lexeme) "]"


position : Parser String
position =
    succeed (\a b -> a++","++b)
        |. (symbol >> lexeme) "<"
        |= (succeed String.fromFloat |= lexeme negFloat)
        |. (symbol >> lexeme) ","
        |= (succeed String.fromFloat |= lexeme negFloat)
        |. (symbol >> lexeme) ">"


stmtList : Parser (List Stmt)
stmtList =
    (loop [] (item node) |. (symbol >> lexeme) "|") |> andThen (\graph -> loop graph (item edge))


item : Parser Stmt -> (List Stmt) -> Parser (Step (List Stmt) (List Stmt))
item parser list =
    oneOf [ succeed (\parsed -> Loop (parsed::list)) |= parser, succeed (Done list) ]


nodeId : Parser NodeId
nodeId =
    succeed (\id -> NodeId (ID id) Nothing)
        |= (lexeme int |> map String.fromInt)


edgeId : Parser Attr
edgeId =
    succeed (Attr (ID "id"))
        |= (succeed ID |= map String.fromInt int)


parseList : List (Parser a) -> Parser (List a)
parseList list =
    case list of
        (x::xs) ->
            andThen (\a -> map ((::) a) (parseList xs)) x

        _ ->
            succeed []


hostList : Parser String
hostList =
    succeed String.slice
        |= getOffset
        |. hostListParser
        |= getOffset
        |= getSource
        |> andThen (\s -> if s == "empty" then succeed "" else succeed s)


root : Parser (Maybe Attr)
root =
    succeed (Attr (ID "style") >> Just)
        |= (succeed ID |= oneOf [ succeed "bold,filled" |. (symbol >> lexeme) "(R)", succeed "filled" ])
        |. (symbol >> lexeme) ","


nodeColour : Parser (Maybe Attr)
nodeColour =
    oneOf
        [ succeed (Attr (ID "fillcolor") >> Just)
            |= (succeed ID
                |. (symbol >> lexeme) "#"
                |= oneOf
                    [ succeed "white" |. (symbol >> lexeme) "none"
                    , succeed "grey" |. (symbol >> lexeme) "grey"
                    , succeed "red" |. (symbol >> lexeme) "red"
                    , succeed "green" |. (symbol >> lexeme) "green"
                    , succeed "blue" |. (symbol >> lexeme) "blue"
                    ]
            )
        , succeed Nothing
        ]


edgeColour : Parser (Maybe Attr)
edgeColour =
    oneOf
        [ succeed (Attr (ID "color") >> Just)
            |= (succeed ID
                |. (symbol >> lexeme) "#"
                |= oneOf
                    [ succeed "black" |. (symbol >> lexeme) "none"
                    , succeed "red" |. (symbol >> lexeme) "red"
                    , succeed "green" |. (symbol >> lexeme) "green"
                    , succeed "blue" |. (symbol >> lexeme) "blue"
                    ]
            )
        , succeed identity |. (symbol >> lexeme) "#" |. (symbol >> lexeme) "dashed" |= succeed (Just (Attr (ID "style") (ID "dashed")))
        , succeed Nothing
        ]



pos : Parser (Maybe Attr)
pos =
    oneOf
        [ succeed (Attr (ID "pos") >> Just)
            |= (succeed ID |= position)
        , succeed Nothing
        ]


label : Parser (Maybe Attr)
label =
    succeed (Attr (ID "label") >> Just)
        |= (succeed ID |= hostList)


node : Parser Stmt
node =
    succeed NodeStmt
        |. (symbol >> lexeme) "("
        |= nodeId
        |= ((parseList [ root, label, nodeColour, pos ]) |> map (List.filterMap identity))
        |. (symbol >> lexeme) ")"


edge : Parser Stmt
edge =
    succeed (\id from to attrs -> EdgeStmtNode from (EdgeNode to) [] (id::attrs))
        |. (symbol >> lexeme) "("
        |= edgeId
        |. (symbol >> lexeme) ","
        |= nodeId
        |. (symbol >> lexeme) ","
        |= nodeId
        |. (symbol >> lexeme) ","
        |= (parseList [ label, edgeColour ] |> map (List.filterMap identity))
        |. (symbol >> lexeme) ")"
