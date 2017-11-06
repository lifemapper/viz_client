{-
Copyright (C) 2017, University of Kansas Center for Research

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
module Leaflet exposing (view, WMSInfo)

import Html exposing (Html)
import Material.Options as Options
import Material.Elevation as Elevation
import Json.Encode exposing (..)


type alias WMSInfo =
    { mapName : String
    , layers : List String
    , endPoint : String
    }


serialize : List WMSInfo -> String
serialize =
    List.map
        (\info ->
            object
                [ ( "mapName", string info.mapName )
                , ( "endPoint", string info.endPoint )
                , ( "layers", info.layers |> List.map string |> list )
                ]
        )
        >> list
        >> encode 0


view : List WMSInfo -> Html msg
view wmsInfo =
    Options.div
        [ Options.cs "leaflet-map"
        , Options.data "leaflet" (serialize wmsInfo)
        , Options.css "height" "510px"
        , Options.css "margin" "10px"
        , Elevation.e4
        ]
        []
