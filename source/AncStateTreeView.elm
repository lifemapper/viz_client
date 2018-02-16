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
import MultiSpeciesView


view : Model -> Html.Html Msg
view model =
    let
        ( min, max ) =
            Dict.get model.selectedVariable model.ancState.ranges
                |> Maybe.withDefault ( 0, 0 )

        scaleData value =
            2 * (value - min) / (max - min) - 1

        selectData cladeId =
            Dict.get ( cladeId, model.selectedVariable ) model.ancState.values
                |> Maybe.map scaleData

        dataForVar var =
            ( model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, var ) model.ancState.values)
            , Just 0.0
            , Just 0.0
            )

        tableHead =
            Html.thead []
                [ Html.tr []
                    [ Html.th
                        [ Html.Attributes.colspan 2
                        , Html.Attributes.style [ ( "text-decoration", "underline" ) ]
                        ]
                        [ Html.text "Ancestral data for Selected Node" ]
                    ]
                , Html.tr [] [ Html.th [] [ Html.text "Value" ], Html.th [] [ Html.text "Variable" ] ]
                ]
    in
        MultiSpeciesView.view model tableHead variableFormatter model.ancStateVars selectData dataForVar


variableFormatter : ( Float, Float ) -> String
variableFormatter ( value, _ ) =
    F.print (F.roundTo 3) value
