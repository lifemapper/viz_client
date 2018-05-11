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


module MapCardMultiple exposing (NamedMap, Model, update, Msg(SetAvailable), view, init)

import Maybe.Extra as Maybe
import List.Extra as List
import Material
import Material.Options as Options
import Material.Card as Card
import Material.Elevation as Elevation
import Material.List as L
import Html exposing (Html)
import Helpers exposing (Index)
import Leaflet exposing (WMSInfo, BoundingBox)


type alias NamedMap =
    { name : String
    , wmsInfo : WMSInfo
    }


type alias Model =
    { available : List NamedMap
    , showing : List NamedMap
    , bb : Maybe BoundingBox
    , expanded : Bool
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | SetShowing NamedMap
    | SetNotShowing NamedMap
    | SetAvailable (List NamedMap)
    | SetExpanded Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SetAvailable available ->
            ( { model | available = available, showing = available }, Cmd.none )

        SetShowing show ->
            ( { model | showing = show :: model.showing }, Cmd.none )

        SetNotShowing notShow ->
            ( { model | showing = List.remove notShow model.showing }, Cmd.none )

        SetExpanded expanded ->
            ( { model | expanded = expanded }, Cmd.none )


selectLayers : Int -> WMSInfo -> WMSInfo
selectLayers i mapInfo =
    { mapInfo | layers = mapInfo.layers |> List.getAt i |> Maybe.toList }


layerLi : Model -> Index -> Int -> NamedMap -> Html Msg
layerLi model index i map =
    let
        ( iconName, onClick ) =
            if List.member map model.showing then
                ( "check_box", SetNotShowing map )
            else
                ( "check_box_outline_blank", SetShowing map )

        icon =
            L.icon iconName [ Options.onClick onClick ]
    in
        L.li [] [ L.content [] [ icon, Html.text map.name ] ]


layersList : Index -> Model -> Html Msg
layersList index model =
    L.ul [] <| List.indexedMap (layerLi model index) <| List.reverse model.available


view : Index -> String -> Model -> Html Msg
view index title model =
    let
        leafletDiv =
            model.available
                |> List.filter (\m -> List.member m model.showing)
                -- doesn't this just equal model.showing?
                |>
                    List.indexedMap (\i m -> m.wmsInfo |> selectLayers 0)
                |> Leaflet.view model.bb
    in
        Card.view
            [ Elevation.e2
            , Options.css "width" "100%"
            ]
            [ Card.title [ Options.onClick <| SetExpanded (not model.expanded) ]
                [ Card.head [] [ Html.text title ] ]
            , Card.text [ Options.css "display" "none" |> Options.when (not model.expanded) ]
                [ layersList index model ]
            , Card.text [ Options.css "padding" "0", Options.css "width" "100%" ] [ leafletDiv ]
            ]


init : Maybe BoundingBox -> List NamedMap -> Model
init bb available =
    { available = available
    , showing = available
    , bb = bb
    , expanded = False
    , mdl = Material.model
    }
