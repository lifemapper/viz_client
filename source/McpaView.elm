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


module McpaView exposing (view)

import Html
import Html.Attributes
import Dict
import Formatting as F exposing ((<>))
import McpaModel exposing (..)
import LinearTreeView exposing (drawTree, computeColor, gradientDefinitions)
import MultiSpeciesView


view : Model -> Html.Html Msg
view model =
    let
        selectData cladeId =
            Dict.get ( cladeId, "Observed", model.selectedVariable ) model.mcpaData

        tableHead =
            Html.thead []
                [ Html.tr [] [ Html.th [ Html.Attributes.colspan 2 ] [ Html.text "MCPA data for Selected Node" ] ]
                , Html.tr [] [ Html.th [] [ Html.text "Observed (p-value)" ], Html.th [] [ Html.text "Variable" ] ]
                ]
    in
        MultiSpeciesView.view model tableHead drawVariable model.mcpaVariables selectData


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


variableFormatter : ( Float, Float ) -> String
variableFormatter ( observed, pValue ) =
    F.print (F.roundTo 3 <> F.s " (" <> F.roundTo 3 <> F.s ") ") observed pValue


drawVariable : Model -> String -> Html.Html Msg
drawVariable model var =
    let
        significant =
            model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, "BH Significant", var ) model.mcpaData)

        observed =
            model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, "Observed", var ) model.mcpaData)

        pValue =
            model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, "P-Values", var ) model.mcpaData)

        fontWeight =
            if significant |> Maybe.map ((<) 0.5) |> Maybe.withDefault False then
                ( "font-weight", "bold" )
            else
                ( "font-weight", "normal" )

        bar =
            Maybe.map2 (,) observed pValue |> Maybe.map (List.singleton << barGraph) |> Maybe.withDefault []

        values =
            Maybe.map2 (,) observed pValue
                |> Maybe.map variableFormatter
                |> Maybe.withDefault ""
    in
        Html.tr []
            [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ), ( "padding-right", "12px" ) ] ]
                [ Html.text values ]
            , Html.td [ Html.Attributes.style [ ( "position", "relative" ), fontWeight ] ] (bar ++ [ Html.text var ])
            ]
