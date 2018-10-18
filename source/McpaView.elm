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
import ParseMcpa exposing (McpaData)
import McpaTreeView exposing (viewTree)
import McpaGraphView exposing (viewGraph)


view : Model McpaData -> Html.Html Msg
view model =
    let
        selectData cladeId =
            Dict.get ( cladeId, "Observed", model.selectedVariable ) model.data

        dataForVar var =
            ( model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, "Observed", var ) model.data)
            , model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, "P-Values", var ) model.data)
            , model.selectedNode |> Maybe.andThen (\cladeId -> Dict.get ( cladeId, "BH Significant", var ) model.data)
            )
    in
        Html.div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                  -- , ( "justify-content", "space-between" )
                , ( "font-family", "sans-serif" )
                , ( "height", "100vh" )
                ]
            ]
            [ viewTree model True selectData
            , viewGraph model.selectedNode False variableFormatter model.variables dataForVar
            ]


variableFormatter : ( Float, Float ) -> String
variableFormatter ( observed, pValue ) =
    F.print (F.roundTo 3) observed
