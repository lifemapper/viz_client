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


module McpaTreeView exposing (viewTree)

import Html
import Html.Attributes
import Html.Events
import List.Extra as List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import McpaModel exposing (..)
import LinearTreeView exposing (computeColor, drawTree, gradientDefinitions)


viewTree : Model data -> Bool -> (Int -> Maybe Float) -> Html.Html Msg
viewTree model redBlue selectData =
    let
        variables =
            model.variables
                |> List.partition (\v -> v == "Env - Adjusted R-squared" || v == "BG - Adjusted R-squared")
                |> (\( adjustedRSquareds, rest ) -> adjustedRSquareds ++ rest)

        computeColor_ opacity cladeId =
            selectData cladeId
                |> Maybe.map (computeColor opacity)
                |> Maybe.withDefault "#ccc"

        ( treeHeight, grads, treeSvg ) =
            drawTree
                { computeColor = computeColor_
                , showBranchLengths = model.showBranchLengths
                , treeDepth = model.treeInfo.depth
                , totalLength = model.treeInfo.length
                , flaggedNodes = model.flaggedNodes
                , selectedNode = model.selectedNode
                , selectNode = SelectNode
                , redBlue = redBlue
                }
                "#ccc"
                model.treeInfo.root

        gradDefs =
            gradientDefinitions grads

        select =
            String.toInt
                >> Result.toMaybe
                >> Maybe.andThen (\i -> List.getAt i variables)
                >> Maybe.withDefault ""
                >> SelectVariable

        variableSelector =
            Html.div [ Html.Attributes.style [ ( "margin-bottom", "8px" ) ] ]
                [ Html.span [] [ Html.text "Node color: " ]
                , Html.select [ Html.Events.onInput select, Html.Attributes.style [ ( "max-width", "355px" ) ] ]
                    (variables
                        |> List.indexedMap
                            (\i v ->
                                Html.option
                                    [ Html.Attributes.selected (v == model.selectedVariable)
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
                        , Html.Attributes.checked model.showBranchLengths
                        , Html.Attributes.readonly True
                        , Html.Events.onClick ToggleShowLengths
                        ]
                        []
                    , Html.text "Show branch lengths"
                    ]
                ]
    in
        Html.div
            [ Html.Attributes.style [ ( "display", "flex" ), ( "flex-direction", "column" ) ] ]
            [ Html.h3 [ Html.Attributes.style [ ( "text-align", "center" ), ( "text-decoration", "underline" ) ] ]
                [ Html.text "Phylogenetic Tree" ]
            , Html.div
                [ Html.Attributes.style
                    [ ( "display", "flex" )
                    , ( "justify-content", "space-between" )
                    , ( "flex-shrink", "0" )
                    ]
                ]
                [ variableSelector, toggleBranchLengths ]
            , Html.div [ Html.Attributes.style [ ( "margin-bottom", "20px" ), ( "overflow-y", "auto" ) ] ]
                [ svg
                    [ width "560"
                    , height (14 * treeHeight |> toString)
                    , viewBox ("0 0 40 " ++ (toString treeHeight))
                    , Html.Attributes.style [ ( "background", "#000" ), ( "font-family", "sans-serif" ) ]
                      -- , Html.Events.onClick JumpUp
                    ]
                    -- (clickBox :: treeSvg)
                    (gradDefs :: treeSvg)
                ]
            ]
