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


module McpaGraphView exposing (viewGraph)

import Html
import Html.Attributes
import List.Extra as List
import McpaModel exposing (..)
import LinearTreeView exposing (computeColor)


barGraph : ( Float, Float ) -> Html.Html Msg
barGraph ( observedValue, pValue ) =
    let
        height =
            (100 * abs observedValue |> toString) ++ "%"

        -- (1.0 - e ^ (-1.0 * abs observedValue) |> (*) 100 |> toString) ++ "%"
        opacity =
            1.0 - (pValue / 1.2)

        background =
            computeColor opacity observedValue
    in
        Html.div
            [ Html.Attributes.style
                [ ( "width", "100%" )
                , ( "height", height )
                , ( "position", "absolute" )
                , ( "bottom", "0" )
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
        Html.td
            [ Html.Attributes.style [ ( "position", "relative" ), fontWeight ]
            , Html.Attributes.title (var ++ "\n" ++ values)
            ]
            bar


viewGraph :
    Model data
    -> Bool
    -> (( Float, Float ) -> String)
    -> List String
    -> (String -> ( Maybe Float, Maybe Float, Maybe Float ))
    -> Html.Html Msg
viewGraph { selectedNode } showBarGraph variableFormatter vars dataForVar =
    let
        ( envVars, bgVars ) =
            case List.findIndex ((==) "ENV - Adjusted R-squared") vars of
                Just i ->
                    List.splitAt (i + 1) vars

                Nothing ->
                    ( vars, [] )

        ( envVarTableRows, bgVarTableRows ) =
            case selectedNode of
                Just _ ->
                    ( envVars
                        |> List.map (\var -> dataForVar var |> drawVariable showBarGraph variableFormatter var)
                        |> Html.tr
                            [ Html.Attributes.style
                                [ ( "height", "400px" )
                                , ( "border-bottom", "1px solid" )
                                , ( "border-right", "1px solid" )
                                ]
                            ]
                    , bgVars
                        |> List.reverse
                        |> List.map (\var -> dataForVar var |> drawVariable showBarGraph variableFormatter var)
                        |> Html.tr
                            [ Html.Attributes.style
                                [ ( "height", "400px" )
                                , ( "border-bottom", "1px solid" )
                                , ( "border-left", "1px solid" )
                                ]
                            ]
                    )

                Nothing ->
                    ( Html.tr []
                        [ Html.td [ Html.Attributes.colspan 2, Html.Attributes.style [ ( "text-align", "center" ) ] ]
                            [ Html.text "No node selected." ]
                        ]
                    , Html.tr []
                        [ Html.td [ Html.Attributes.colspan 2, Html.Attributes.style [ ( "text-align", "center" ) ] ]
                            [ Html.text "No node selected." ]
                        ]
                    )
    in
        Html.div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "flex-grow", "1" )
                ]
            ]
            [ Html.div
                [ Html.Attributes.style [ ( "flex-shrink", "0" ), ( "margin", "0 12px" ) ] ]
                [ Html.h3 [ Html.Attributes.style [ ( "text-align", "center" ), ( "text-decoration", "underline" ) ] ]
                    [ Html.text "Subtree Left (blue) vs. Right (red) of selected node" ]
                , Html.div
                    [ Html.Attributes.class "leaflet-map"
                    , Html.Attributes.attribute "data-map-column"
                        (selectedNode |> Maybe.map toString |> Maybe.withDefault "")
                    , Html.Attributes.style
                        [ ( "max-width", "900px" )
                        , ( "height", "500px" )
                        , ( "margin-left", "auto" )
                        , ( "margin-right", "auto" )
                        ]
                    ]
                    []
                ]
            , Html.h3 [ Html.Attributes.style [ ( "text-align", "center" ), ( "text-decoration", "underline" ) ] ]
                [ Html.text "Semipartial Correlations b/w Clade and Predictors" ]
            , Html.div [ Html.Attributes.style [ ( "width", "100%" ) ] ]
                [ Html.div
                    [ Html.Attributes.style
                        [ ( "display", "flex" )
                        , ( "justify-content", "center" )
                        , ( "margin-top", "10px" )
                        , ( "margin-left", "auto" )
                        , ( "margin-right", "auto" )
                        , ( "max-width", "900px" )
                        ]
                    ]
                    [ Html.table
                        [ Html.Attributes.style
                            [ ( "width"
                              , 100
                                    * toFloat (List.length envVars)
                                    / toFloat (List.length vars)
                                    |> toString
                                    |> flip (++) "%"
                              )
                            ]
                        ]
                        [ envVarTableRows
                        , Html.tr []
                            [ Html.th [ Html.Attributes.colspan <| List.length envVars ]
                                [ Html.text "Environmental Variables" ]
                            ]
                        ]
                    , Html.table
                        [ Html.Attributes.style
                            [ ( "width"
                              , 100
                                    * toFloat (List.length bgVars)
                                    / toFloat (List.length vars)
                                    |> toString
                                    |> flip (++) "%"
                              )
                            ]
                        ]
                        [ bgVarTableRows
                        , Html.tr []
                            [ Html.th
                                [ Html.Attributes.colspan <| List.length bgVars
                                ]
                                [ Html.text "Biogeographic Hypotheses" ]
                            ]
                        ]
                    ]
                ]
            ]
