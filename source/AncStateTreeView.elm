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
import McpaModel exposing (..)
import LinearTreeView exposing (computeColor, drawTree, gradientDefinitions)


view : Model -> Html.Html Msg
view ({ treeInfo, zipper, ancState, selectedVariable } as model) =
    let
        computeColor_ opacity cladeId =
            Dict.get ( cladeId, model.selectedVariable ) model.ancState
                |> Maybe.map (computeColor 1.0)
                |> Maybe.withDefault "#ccc"

        ( treeHeight, grads, treeSvg ) =
            drawTree
                { computeColor = computeColor_
                , showBranchLengths = model.showBranchLengths
                , treeDepth = treeInfo.depth
                , totalLength = treeInfo.length
                , selectedNode = model.selectedNode
                , selectNode = SelectNode
                }
                "#ccc"
                treeInfo.root

        gradDefs =
            gradientDefinitions grads

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
            computeColor 1.0 value
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
