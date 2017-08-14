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
