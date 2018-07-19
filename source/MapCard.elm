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


module MapCard exposing (Model, update, Msg(SetMap), view, init)

import Maybe.Extra as Maybe
import List.Extra as List
import Material
import Material.Options as Options
import Material.Card as Card
import Material.Menu as Menu
import Material.Elevation as Elevation
import Material.Icon as Icon
import Html exposing (Html)
import Helpers exposing (Index)
import Leaflet exposing (WMSInfo, BoundingBox)


type alias Model =
    { mapInfo : Maybe WMSInfo
    , bb : Maybe BoundingBox
    , mapLayer : Int
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | SetLayer Int
    | SetMap (Maybe BoundingBox) (Maybe WMSInfo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SetMap bb mapInfo ->
            ( { model | bb = bb, mapInfo = mapInfo, mapLayer = 0 }, Cmd.none )

        SetLayer layer ->
            ( { model | mapLayer = layer }, Cmd.none )


selectLayers : Int -> WMSInfo -> WMSInfo
selectLayers i mapInfo =
    { mapInfo | layers = "bmng" :: (mapInfo.layers |> List.getAt i |> Maybe.toList) }


view : Index -> String -> Model -> Html Msg
view index title model =
    let
        layerNames =
            model.mapInfo |> Maybe.map .layers |> Maybe.withDefault []

        checkmark x =
            if x then
                Icon.view "check" [ Options.css "width" "40px" ]
            else
                Options.span [ Options.css "width" "40px" ] []

        menuItem layer layerName =
            Menu.item [ Menu.onSelect (SetLayer layer) ]
                [ checkmark (layer == model.mapLayer)
                , Html.text layerName
                ]

        leafletDiv =
            model.mapInfo |> Maybe.map (selectLayers model.mapLayer) |> Maybe.toList |> Leaflet.view model.bb
    in
        Card.view
            [ Elevation.e2
            , Options.css "width" "800px"
            , Options.css "height" "591px"
            , Options.css "margin" "20px"
            ]
            [ Card.title [ Card.border ]
                [ Card.head [] [ Html.text title ]
                ]
            , Card.menu [ Options.cs "map-layers-menu" ]
                [ Menu.render Mdl
                    (-1 :: index)
                    model.mdl
                    [ Menu.bottomRight ]
                    (List.indexedMap menuItem layerNames)
                ]
            , Card.text [ Options.css "padding" "0", Options.css "width" "100%" ] [ leafletDiv ]
            ]


init : Maybe BoundingBox -> Maybe WMSInfo -> Model
init bb wmsInfo =
    { mapInfo = wmsInfo
    , bb = bb
    , mapLayer = 0
    , mdl = Material.model
    }
