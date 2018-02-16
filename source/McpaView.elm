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
import MultiSpeciesView


view : Model -> Html.Html Msg
view model =
    let
        selectData cladeId =
            Dict.get ( cladeId, "Observed", model.selectedVariable ) model.mcpaData

        dataForVar var =
            ( model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, "Observed", var ) model.mcpaData)
            , model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, "P-Values", var ) model.mcpaData)
            , model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, "BH Significant", var ) model.mcpaData)
            )

        tableHead =
            Html.thead []
                [ Html.tr []
                    [ Html.th
                        [ Html.Attributes.colspan 2
                        , Html.Attributes.style [ ( "text-decoration", "underline" ) ]
                        ]
                        [ Html.text "MCPA data for Selected Node" ]
                    ]
                , Html.tr [] [ Html.th [] [ Html.text "Observed (p-value)" ], Html.th [] [ Html.text "Variable" ] ]
                ]
    in
        MultiSpeciesView.view model tableHead True variableFormatter model.mcpaVariables selectData dataForVar


variableFormatter : ( Float, Float ) -> String
variableFormatter ( observed, pValue ) =
    F.print (F.roundTo 3 <> F.s " (" <> F.roundTo 3 <> F.s ") ") observed pValue
