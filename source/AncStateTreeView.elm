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


module AncStateTreeView exposing (view)

import Html
import Html.Attributes
import Html.Events
import Dict
import List.Extra as List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Formatting as F exposing ((<>))
import DecodeTree exposing (Tree(..), TreeData)
import McpaModel exposing (..)


view : Model -> Html.Html Msg
view ({ treeInfo, zipper, ancState, selectedVariable } as model) =
    let
        ( treeHeight, grads, treeSvg ) =
            drawTree model treeInfo.length "#ccc" treeInfo.root

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

        select =
            String.toInt
                >> Result.toMaybe
                >> Maybe.andThen (\i -> List.getAt i model.ancStateVars)
                >> Maybe.withDefault ""
                >> SelectVariable

        variableSelector =
            Html.div [ Html.Attributes.style [ ( "margin-bottom", "8px" ) ] ]
                [ Html.select [ Html.Events.onInput select ]
                    (model.ancStateVars
                        |> List.indexedMap
                            (\i v ->
                                Html.option
                                    [ Html.Attributes.selected (v == selectedVariable)
                                    , Html.Attributes.value (toString i)
                                    ]
                                    [ Html.text v ]
                            )
                    )
                ]

        showBranchLengths =
            Html.div []
                [ Html.label []
                    [ Html.input
                        [ Html.Attributes.type_ "checkbox"
                        , Html.Attributes.checked model.showBranchLengths
                        , Html.Attributes.readonly True
                        , Html.Events.onClick ToggleShowLengths
                        ]
                        []
                    , Html.text "Show branch lengths"
                    ]
                ]

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
    in
        Html.div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "justify-content", "space-between" )
                , ( "font-family", "sans-serif" )
                ]
            ]
            [ Html.div
                [ Html.Attributes.style [ ( "height", "100vh" ), ( "display", "flex" ), ( "flex-direction", "column" ) ] ]
                [ Html.div
                    [ Html.Attributes.style
                        [ ( "display", "flex" )
                        , ( "justify-content", "space-between" )
                        , ( "flex-shrink", "0" )
                        ]
                    ]
                    [ variableSelector, showBranchLengths ]
                , Html.div [ Html.Attributes.style [ ( "margin-bottom", "20px" ), ( "overflow-y", "scroll" ) ] ]
                    [ svg
                        [ width "800"
                        , height (20 * treeHeight |> toString)
                        , viewBox ("0 0 40 " ++ (toString treeHeight))
                        , Html.Attributes.style [ ( "background", "#000" ), ( "font-family", "sans-serif" ) ]
                          -- , Html.Events.onClick JumpUp
                        ]
                        -- (clickBox :: treeSvg)
                        (gradDefs :: treeSvg)
                    ]
                ]
            , Html.table []
                ([ Html.tr [] [ Html.th [ Html.Attributes.colspan 2 ] [ Html.text "Ancestral data for Selected Node" ] ]
                 , Html.tr [] [ Html.th [] [ Html.text "Value" ], Html.th [] [ Html.text "Variable" ] ]
                 ]
                    ++ (model.ancStateVars |> List.map (drawVariable model))
                )
            , Html.div
                [ Html.Attributes.class "leaflet-map"
                , Html.Attributes.attribute "data-map-column"
                    (model.selectedNode |> Maybe.map toString |> Maybe.withDefault "")
                , Html.Attributes.style [ ( "width", "800px" ), ( "height", "800px" ) ]
                ]
                []
            ]


barGraph : Float -> Html.Html Msg
barGraph value =
    let
        width =
            (1.0 - e ^ (-1.0 * abs value) |> (*) 100 |> toString) ++ "%"

        background =
            computeColor_ 1.0 value
    in
        Html.div
            [ Html.Attributes.style
                [ ( "width", width )
                , ( "height", "100%" )
                , ( "position", "absolute" )
                , ( "top", "0" )
                , ( "background-color", background )
                , ( "z-index", "-1" )
                ]
            ]
            []


variableFormatter : Float -> String
variableFormatter value =
    F.print (F.roundTo 3) value


drawVariable : Model -> String -> Html.Html Msg
drawVariable model var =
    let
        value =
            model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, var ) model.ancState)

        fontWeight =
            ( "font-weight", "normal" )

        bar =
            value |> Maybe.map (List.singleton << barGraph) |> Maybe.withDefault []

        values =
            value
                |> Maybe.map variableFormatter
                |> Maybe.withDefault ""
    in
        Html.tr []
            [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ), ( "padding-right", "12px" ) ] ]
                [ Html.text values ]
            , Html.td [ Html.Attributes.style [ ( "position", "relative" ), fontWeight ] ] (bar ++ [ Html.text var ])
            ]


scaleLength : Float -> Float -> Float
scaleLength totalLength thisLength =
    30 * thisLength / totalLength


logistic : Float -> Float
logistic x =
    1.0 / (1.0 + e ^ (-3.0 * x))


computeColor_ : Float -> Float -> String
computeColor_ opacity value =
    let
        v =
            logistic value * 256 |> round

        r =
            256 - v |> toString

        g =
            v |> toString
    in
        "rgba(" ++ r ++ "," ++ g ++ ",0," ++ (toString opacity) ++ ")"


computeColor : Float -> Model -> Int -> String
computeColor opacity model cladeId =
    Dict.get ( cladeId, model.selectedVariable ) model.ancState
        |> Maybe.map (computeColor_ 1.0)
        |> Maybe.withDefault "#ccc"


drawTree : Model -> Float -> String -> Tree -> ( Float, List ( Int, String, String ), List (Svg Msg) )
drawTree model totalLength parentColor tree =
    case tree of
        Leaf data ->
            let
                length =
                    if model.showBranchLengths then
                        data.length |> Maybe.map (scaleLength totalLength) |> Maybe.withDefault 0
                    else
                        30.0 / (toFloat model.treeInfo.depth)
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
                    computeColor 1.0 model data.cladeId

                ( leftHeight, leftGrads, leftNodes ) =
                    drawTree model totalLength color left

                ( rightHeight, rightGrads, rightNodes ) =
                    drawTree model totalLength color right

                thisHeight =
                    leftHeight + rightHeight

                length =
                    if model.showBranchLengths then
                        data.length |> Maybe.map (scaleLength totalLength) |> Maybe.withDefault 0
                    else
                        30.0 / (toFloat model.treeInfo.depth)

                thisGrad =
                    ( data.cladeId, parentColor, color )

                boxes =
                    if model.selectedNode == Just data.cladeId then
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
                            , Html.Events.onClick <| SelectNode data.cladeId
                            ]
                            []
                       ]
                )
