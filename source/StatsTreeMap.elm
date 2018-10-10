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


module StatsTreeMap exposing (..)

import Html
import Html.Attributes
import Dict
import McpaModel exposing (..)
import ParseMcpa exposing (McpaData, parseMcpa)
import McpaTreeView exposing (viewTree)
import StatsMain


parseData : String -> ( List String, McpaData )
parseData data =
    case parseMcpa data of
        Ok result ->
            result

        Err err ->
            Debug.crash ("failed to decode MCPA matrix: " ++ err)


view : Model McpaData -> Html.Html Msg
view model =
    let
        selectData cladeId =
            Dict.get ( cladeId, "Observed", model.selectedVariable ) model.data
    in
        Html.div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                  -- , ( "justify-content", "space-between" )
                , ( "font-family", "sans-serif" )
                , ( "height", "100vh" )
                ]
            ]
            [ viewTree model selectData
            , Html.div
                [ Html.Attributes.style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    , ( "flex-grow", "1" )
                    ]
                ]
                [ Html.div
                    [ Html.Attributes.style [ ( "flex-shrink", "0" ), ( "margin", "0 12px" ) ] ]
                    [ Html.h3 [ Html.Attributes.style [ ( "text-align", "center" ), ( "text-decoration", "underline" ) ] ]
                        [ Html.text "Mappy McMapface" ]
                    , Html.div
                        [ Html.Attributes.class "leaflet-map"
                        , Html.Attributes.attribute "data-map-sites" ""
                            -- (model.selectedNode |> Maybe.map toString |> Maybe.withDefault "")
                        , Html.Attributes.style
                            [ ( "max-width", "900px" )
                            , ( "height", "500px" )
                            , ( "margin-left", "auto" )
                            , ( "margin-right", "auto" )
                            ]
                        ]
                        []
                    ]
                ]
            ]


main : Program Flags (Model McpaData) Msg
main =
    Html.programWithFlags
        { init = init parseData
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
