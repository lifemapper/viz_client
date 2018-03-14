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


module MultiSpeciesView exposing (view)

import Html
import Html.Attributes
import Html.Events
import List.Extra as List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import McpaModel exposing (..)
import LinearTreeView exposing (computeColor, drawTree, gradientDefinitions)


barGraph : ( Float, Float ) -> Html.Html Msg
barGraph ( observedValue, pValue ) =
    let
        width =
            (1.0 - e ^ (-1.0 * abs observedValue) |> (*) 100 |> toString) ++ "%"

        opacity =
            1.0 - (pValue / 1.2)

        background =
            computeColor opacity observedValue
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


drawVariable : Bool -> (( Float, Float ) -> String) -> String -> ( Maybe Float, Maybe Float, Maybe Float ) -> Html.Html Msg
drawVariable showBarGraph formatter var ( observed, pValue, significant ) =
    let
        fontWeight =
            if significant |> Maybe.map ((<) 0.5) |> Maybe.withDefault False then
                ( "font-weight", "bold" )
            else
                ( "font-weight", "normal" )

        bar =
            if showBarGraph then
                Maybe.map2 (,) observed pValue |> Maybe.map (List.singleton << barGraph) |> Maybe.withDefault []
            else
                []

        values =
            Maybe.map2 (,) observed pValue
                |> Maybe.map formatter
                |> Maybe.withDefault ""
    in
        Html.tr []
            [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ), ( "padding-right", "12px" ) ] ]
                [ Html.text values ]
            , Html.td [ Html.Attributes.style [ ( "position", "relative" ), fontWeight ] ] (bar ++ [ Html.text var ])
            ]


view :
    Model
    -> Html.Html Msg
    -> Bool
    -> (( Float, Float ) -> String)
    -> List String
    -> (Int -> Maybe Float)
    -> (String -> ( Maybe Float, Maybe Float, Maybe Float ))
    -> Html.Html Msg
view { selectedVariable, showBranchLengths, treeInfo, selectedNode } tableHead showBarGraph variableFormatter vars selectData dataForVar =
    let
        computeColor_ opacity cladeId =
            selectData cladeId
                |> Maybe.map (computeColor opacity)
                |> Maybe.withDefault "#ccc"

        ( treeHeight, grads, treeSvg ) =
            drawTree
                { computeColor = computeColor_
                , showBranchLengths = showBranchLengths
                , treeDepth = treeInfo.depth
                , totalLength = treeInfo.length
                , selectedNode = selectedNode
                , selectNode = SelectNode
                }
                "#ccc"
                treeInfo.root

        gradDefs =
            gradientDefinitions grads

        select =
            String.toInt
                >> Result.toMaybe
                >> Maybe.andThen (\i -> List.getAt i vars)
                >> Maybe.withDefault ""
                >> SelectVariable

        variableSelector =
            Html.div [ Html.Attributes.style [ ( "margin-bottom", "8px" ) ] ]
                [ Html.select [ Html.Events.onInput select ]
                    (vars
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

        toggleBranchLengths =
            Html.div []
                [ Html.label []
                    [ Html.input
                        [ Html.Attributes.type_ "checkbox"
                        , Html.Attributes.checked showBranchLengths
                        , Html.Attributes.readonly True
                        , Html.Events.onClick ToggleShowLengths
                        ]
                        []
                    , Html.text "Show branch lengths"
                    ]
                ]
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
                    [ variableSelector, toggleBranchLengths ]
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
                (tableHead
                    :: (vars
                            |> List.map
                                (\var ->
                                    dataForVar var
                                        |> drawVariable showBarGraph variableFormatter var
                                )
                       )
                )
            , Html.div
                [ Html.Attributes.class "leaflet-map"
                , Html.Attributes.attribute "data-map-column"
                    (selectedNode |> Maybe.map toString |> Maybe.withDefault "")
                , Html.Attributes.style [ ( "width", "800px" ), ( "height", "800px" ) ]
                ]
                []
            ]
