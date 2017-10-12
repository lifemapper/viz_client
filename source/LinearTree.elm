module LinearTree exposing (..)

-- import Html
-- import ExampleTree

import DecodeTree exposing (Tree(..), TreeData)
import Svg exposing (..)
import Svg.Attributes exposing (..)


treeDepth : Tree -> Int
treeDepth tree =
    case tree of
        Leaf _ ->
            1

        Node _ left right ->
            1 + Basics.max (treeDepth left) (treeDepth right)


drawTree : Int -> Tree -> ( Float, List (Svg msg) )
drawTree depth tree =
    case tree of
        Leaf data ->
            ( 1
            , [ line [ x1 "0", x2 (toString depth), y1 "0.5", y2 "0.5", strokeWidth "0.1", stroke "black" ] []
              , text_ [ x (toString (depth + 1)), y "0.75", fontSize "0.8" ]
                    [ text data.name ]
              ]
            )

        Node data left right ->
            let
                ( leftHeight, leftNodes ) =
                    drawTree (depth - 1) left

                ( rightHeight, rightNodes ) =
                    drawTree (depth - 1) right

                thisHeight =
                    leftHeight + rightHeight

                width =
                    (toFloat depth) / 8 |> toString
            in
                ( thisHeight
                , [ line
                        [ x1 "0"
                        , x2 width
                        , y1 <| toString (thisHeight / 2.0)
                        , y2 <| toString (thisHeight / 2.0)
                        , strokeWidth "0.1"
                        , stroke "black"
                        ]
                        []
                  , line
                        [ x1 width
                        , x2 width
                        , y1 (toString (leftHeight / 2))
                        , y2 (toString (leftHeight + rightHeight / 2))
                        , strokeWidth "0.1"
                        , stroke "black"
                        ]
                        []
                  , g [ transform <| "translate(" ++ width ++ ",0)" ] leftNodes
                  , g [ transform <| "translate(" ++ width ++ "," ++ (toString leftHeight) ++ ")" ] rightNodes
                  ]
                )
