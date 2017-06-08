port module Leaflet exposing (..)


port setLeafletMap : LeafletMap -> Cmd msg

port clearLeafletMap : String -> Cmd msg

type alias LeafletMap =
    { containerId : String
    , mapName : String
    , layers : List String
    , endPoint : String
    }
