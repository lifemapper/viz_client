{-
   Copyright (C) 2018, University of Kansas Center for Research

   Lifemapper Project, lifemapper [at] ku [dot] edu,
   Biodiversity Institute,
   1345 Jayhawk Boulevard, Lawrence, Kansas, 66045, USA

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.
-}


module LinearTreeView exposing (drawTree, computeColor)

import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import DecodeTree exposing (Tree(..), TreeData)


scaleLength : Float -> Float -> Float
scaleLength totalLength thisLength =
    30 * thisLength / totalLength


logistic : Float -> Float
logistic x =
    1.0 / (1.0 + e ^ (-3.0 * x))


computeColor : Float -> Float -> String
computeColor opacity value =
    let
        v =
            logistic value * 256 |> round

        r =
            256 - v |> toString

        g =
            v |> toString
    in
        "rgba(" ++ r ++ "," ++ g ++ ",0," ++ (toString opacity) ++ ")"


type alias TreeConfig msg =
    { computeColor : Float -> Int -> String
    , showBranchLengths : Bool
    , treeDepth : Int
    , totalLength : Float
    , selectedNode : Maybe Int
    , selectNode : Int -> msg
    }


drawTree : TreeConfig msg -> String -> Tree -> ( Float, List ( Int, String, String ), List (Svg msg) )
drawTree config parentColor tree =
    case tree of
        Leaf data ->
            let
                length =
                    if config.showBranchLengths then
                        data.length |> Maybe.map (scaleLength config.totalLength) |> Maybe.withDefault 0
                    else
                        30 / (toFloat config.treeDepth)
            in
                ( 1
                , [ ( data.cladeId, parentColor, "#ccc" ) ]
                , [ rect
                        [ x "0"
                        , width (toString length)
                        , y "0.45"
                        , height "0.05"
                        , fill ("url(#grad-" ++ (toString data.cladeId) ++ ")")
                        ]
                        []
                  , text_ [ x (toString (length + 0.5)), y "0.75", fontSize "0.8", stroke "none", fill "#ccc" ]
                        [ text data.name ]
                  ]
                )

        Node data left right ->
            let
                color =
                    config.computeColor 1.0 data.cladeId

                ( leftHeight, leftGrads, leftNodes ) =
                    drawTree config color left

                ( rightHeight, rightGrads, rightNodes ) =
                    drawTree config color right

                thisHeight =
                    leftHeight + rightHeight

                length =
                    if config.showBranchLengths then
                        data.length |> Maybe.map (scaleLength config.totalLength) |> Maybe.withDefault 0
                    else
                        30 / (toFloat config.treeDepth)

                thisGrad =
                    ( data.cladeId, parentColor, color )

                boxes =
                    if config.selectedNode == Just data.cladeId then
                        [ rect
                            [ x <| toString length
                            , y "0"
                            , width "100"
                            , height <| toString leftHeight
                            , fill "#00f"
                            , fillOpacity "0.5"
                            ]
                            []
                        , rect
                            [ x <| toString length
                            , y <| toString leftHeight
                            , width "100"
                            , height <| toString rightHeight
                            , fill "#f00"
                            , fillOpacity "0.5"
                            ]
                            []
                        ]
                    else
                        []
            in
                ( thisHeight
                , thisGrad :: (leftGrads ++ rightGrads)
                , boxes
                    ++ [ g [ transform <| "translate(" ++ (toString length) ++ ",0)" ] leftNodes
                       , g [ transform <| "translate(" ++ (toString length) ++ "," ++ (toString leftHeight) ++ ")" ] rightNodes
                       , rect
                            [ x "0"
                            , width (toString length)
                            , height "0.05"
                            , y <| toString (thisHeight / 2.0 - 0.05)
                            , strokeWidth "0.01"
                            , fill ("url(#grad-" ++ (toString data.cladeId) ++ ")")
                            ]
                            []
                       , line
                            [ x1 (toString length)
                            , x2 (toString length)
                            , y1 (toString (leftHeight / 2))
                            , y2 (toString (leftHeight + rightHeight / 2))
                            , strokeWidth "0.05"
                            , stroke color
                            ]
                            []
                       , circle
                            [ cx (toString length)
                            , cy <| toString (thisHeight / 2.0)
                            , r "0.3"
                            , fill color
                            , Html.Events.onClick <| config.selectNode data.cladeId
                            ]
                            []
                       ]
                )
