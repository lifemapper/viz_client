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


module LinearTreeView exposing (view)

import Html
import Html.Attributes
import Html.Events
import Dict
import List.Extra as List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import TreeZipper exposing (TreeZipper, Position(..), moveToward, getTree, getData, getPosition)
import DecodeTree exposing (Tree(..), TreeData)
import McpaModel exposing (..)


view : Model -> Html.Html Msg
view ({ root, zipper, mcpaData, selectedVariable } as model) =
    let
        treeDepth_ =
            treeDepth root

        treeBreadth_ =
            treeBreadth root

        treeLength_ =
            treeLength root

        ( treeHeight, grads, treeSvg ) =
            drawTree model treeLength_ "#ccc" root

        gradDefs =
            grads
                |> List.map
                    (\( cladeId, startColor, endColor ) ->
                        linearGradient
                            [ id <| "grad-" ++ (toString cladeId)
                            , x1 "0%"
                            , y1 "0%"
                            , x2 "100%"
                            , y2 "0%"
                            ]
                            [ stop [ offset "0%", stopColor startColor ] []
                            , stop [ offset "100%", stopColor endColor ] []
                            ]
                    )
                |> defs []

        mapClade =
            getData zipper |> .cladeId

        clickBox =
            rect
                [ x "0"
                , y "0"
                , width "100"
                , height "100"
                , fill "grey"
                , fillOpacity "0.01"
                , Html.Events.onClick JumpUp
                ]
                []

        select =
            String.toInt
                >> Result.toMaybe
                >> Maybe.andThen (\i -> List.getAt i model.mcpaVariables)
                >> Maybe.withDefault ""
                >> SelectVariable

        variableSelector =
            Html.select [ Html.Events.onInput select ]
                (model.mcpaVariables
                    |> List.indexedMap
                        (\i v ->
                            Html.option
                                [ Html.Attributes.selected (v == selectedVariable)
                                , Html.Attributes.value (toString i)
                                ]
                                [ Html.text v ]
                        )
                )

        border v =
            if v == selectedVariable then
                Html.Attributes.style [ ( "border", "1px solid black" ) ]
            else
                Html.Attributes.style [ ( "border", "none" ) ]
    in
        Html.div []
            [ Html.p []
                [ variableSelector ]
            , svg
                [ width "800"
                , height "800"
                , viewBox ("0 0 100 100")
                , Html.Attributes.style [ ( "background", "#000" ), ( "font-family", "sans-serif" ) ]
                  -- , Html.Events.onClick JumpUp
                ]
                -- (clickBox :: treeSvg)
                [ gradDefs, g [ transform "translate(5,5)" ] treeSvg ]
            , Html.ul [ Html.Attributes.style [ ( "display", "inline-block" ), ( "list-style", "none" ) ] ]
                (model.mcpaVariables
                    |> List.map
                        (\v ->
                            Html.li [ Html.Events.onClick (SelectVariable v), border v ] [ Html.text v ]
                        )
                )
            , Html.div
                [ Html.Attributes.class "leaflet-map"
                , Html.Attributes.attribute "data-map-column" (mapClade |> toString)
                , Html.Attributes.style [ ( "width", "800px" ), ( "height", "800px" ) ]
                ]
                []
            ]


treeLength : Tree -> Float
treeLength tree =
    case tree of
        Leaf { length } ->
            length |> Maybe.withDefault 0

        Node { length } left right ->
            let
                thisLength =
                    length |> Maybe.withDefault 0
            in
                thisLength + Basics.max (treeLength left) (treeLength right)


treeDepth : Tree -> Int
treeDepth tree =
    case tree of
        Leaf _ ->
            1

        Node _ left right ->
            1 + Basics.max (treeDepth left) (treeDepth right)


treeBreadth : Tree -> Int
treeBreadth tree =
    case tree of
        Leaf _ ->
            1

        Node _ left right ->
            (treeBreadth left) + (treeBreadth right)


scaleLength : Float -> Float -> Float
scaleLength totalLength thisLength =
    80 * thisLength / totalLength


computeColor : Model -> Int -> String
computeColor model cladeId =
    Dict.get ( cladeId, "P-Values", model.selectedVariable ) model.mcpaData
        |> Maybe.map ((*) 255 >> round >> (\green -> "rgb(" ++ (toString <| 255 - green) ++ "," ++ (toString green) ++ ",0)"))
        |> Maybe.withDefault "#ccc"


drawTree : Model -> Float -> String -> Tree -> ( Float, List ( Int, String, String ), List (Svg Msg) )
drawTree model totalLength parentColor tree =
    case tree of
        Leaf data ->
            let
                length =
                    data.length |> Maybe.map (scaleLength totalLength) |> Maybe.withDefault 10
            in
                ( 1
                , [ ( data.cladeId, parentColor, "#ccc" ) ]
                , [ rect
                        [ x "0"
                        , width (toString length)
                        , y "0.45"
                        , height "0.1"
                        , fill ("url(#grad-" ++ (toString data.cladeId) ++ ")")
                        ]
                        []
                  , text_ [ x (toString (length + 2)), y "0.75", fontSize "0.8", stroke "none", fill "#ccc" ]
                        [ text data.name ]
                  ]
                )

        Node data left right ->
            let
                color =
                    computeColor model data.cladeId

                ( leftHeight, leftGrads, leftNodes ) =
                    drawTree model totalLength color left

                ( rightHeight, rightGrads, rightNodes ) =
                    drawTree model totalLength color right

                thisHeight =
                    leftHeight + rightHeight

                length =
                    data.length |> Maybe.map (scaleLength totalLength) |> Maybe.withDefault 10

                thisGrad =
                    ( data.cladeId, parentColor, color )

                boxes =
                    if tree == getTree model.zipper then
                        [ rect
                            [ x <| toString length
                            , y "0"
                            , width "100"
                            , height <| toString leftHeight
                            , fill "blue"
                            , fillOpacity "0.2"
                            , Html.Events.onClick JumpLeft
                            ]
                            []
                        , rect
                            [ x <| toString length
                            , y <| toString leftHeight
                            , width "100"
                            , height <| toString rightHeight
                            , fill "red"
                            , fillOpacity "0.2"
                            , Html.Events.onClick JumpRight
                            ]
                            []
                        ]
                    else
                        []
            in
                ( thisHeight
                , thisGrad :: (leftGrads ++ rightGrads)
                , [ rect
                        [ x "0"
                        , width (toString length)
                        , height "0.1"
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
                        , strokeWidth "0.1"
                        , stroke color
                        ]
                        []
                  , circle
                        [ cx (toString length)
                        , cy <| toString (thisHeight / 2.0)
                        , r "0.3"
                        , fill color
                        ]
                        []
                  , g [ transform <| "translate(" ++ (toString length) ++ ",0)" ] leftNodes
                  , g [ transform <| "translate(" ++ (toString length) ++ "," ++ (toString leftHeight) ++ ")" ] rightNodes
                  ]
                  -- ++ boxes
                )
