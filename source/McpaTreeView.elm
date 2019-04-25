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
import Html.Attributes as A
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
            Html.div [ A.style [ ( "margin-bottom", "8px" ) ] ]
                [ Html.span [] [ Html.text "Predictor: " ]
                , Html.select [ Html.Events.onInput select, A.style [ ( "max-width", "355px" ) ] ]
                    (variables
                        |> List.indexedMap
                            (\i v ->
                                Html.option
                                    [ A.selected (v == model.selectedVariable)
                                    , A.value (toString i)
                                    ]
                                    [ Html.text v ]
                            )
                    )
                ]

        toggleBranchLengths =
            Html.div []
                [ Html.label []
                    [ Html.input
                        [ A.type_ "checkbox"
                        , A.checked model.showBranchLengths
                        , A.readonly True
                        , Html.Events.onClick ToggleShowLengths
                        ]
                        []
                    , Html.text "Show branch lengths"
                    ]
                ]

        ( color0, color1 ) =
            ( computeColor 1.0 0.0, computeColor 1.0 1.0 )

        legend =
            Html.div
                [ A.style
                    [ ( "width", "558px" )
                    , ( "background", "linear-gradient(to right, " ++ color0 ++ ", " ++ color1 ++ ")" )
                    , ( "display", "flex" )
                    , ( "flex-direction", "row" )
                    , ( "justify-content", "space-between" )
                    , ( "margin", "5px 0" )
                    , ( "border", "solid 2px" )
                    ]
                ]
                [ Html.p [ A.style [ ( "margin", "3px" ) ] ] [ Html.text "0.0" ]
                , Html.p [ A.style [ ( "margin", "3px" ) ] ] [ Html.text "Semiparital Correlation b/w Node and Selected Predictor" ]
                , Html.p [ A.style [ ( "margin", "3px" ) ] ] [ Html.text "1.0" ]
                ]
    in
        Html.div
            [ A.style [ ( "display", "flex" ), ( "flex-direction", "column" ) ] ]
            [ Html.h3 [ A.style [ ( "text-align", "center" ), ( "text-decoration", "underline" ) ] ]
                [ Html.text "Phylogenetic Tree" ]
            , Html.div
                [ A.style
                    [ ( "display", "flex" )
                    , ( "justify-content", "space-between" )
                    , ( "flex-shrink", "0" )
                    ]
                ]
                [ variableSelector, toggleBranchLengths ]
            , legend
            , Html.div [ A.style [ ( "margin-bottom", "20px" ), ( "overflow-y", "auto" ) ] ]
                [ svg
                    [ width "560"
                    , height (14 * treeHeight |> toString)
                    , viewBox ("0 0 40 " ++ (toString treeHeight))
                    , A.style [ ( "background", "#000" ), ( "font-family", "sans-serif" ) ]
                      -- , Html.Events.onClick JumpUp
                    ]
                    -- (clickBox :: treeSvg)
                    (gradDefs :: treeSvg)
                ]
            , Html.p [ A.style [ ( "width", "560px" ) ] ]
                [ Html.text <|
                    "Node color indicates correlation between sister clades and "
                        ++ "the selected predictor.  Selecting a node highlights aggregated "
                        ++ "presence of species of one clade in blue and the other in red.  "
                        ++ "Sites where species of both sides are present are purple."
                ]
            ]
