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
import Dict
import Formatting as F exposing ((<>))
import McpaModel exposing (..)
import LinearTreeView exposing (computeColor, drawTree, gradientDefinitions)
import MultiSpeciesView


view : Model -> Html.Html Msg
view model =
    let
        selectData cladeId =
            Dict.get ( cladeId, model.selectedVariable ) model.ancState

        tableHead =
            Html.thead []
                [ Html.tr [] [ Html.th [ Html.Attributes.colspan 2 ] [ Html.text "Ancestral data for Selected Node" ] ]
                , Html.tr [] [ Html.th [] [ Html.text "Value" ], Html.th [] [ Html.text "Variable" ] ]
                ]
    in
        MultiSpeciesView.view model tableHead drawVariable model.ancStateVars selectData


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
