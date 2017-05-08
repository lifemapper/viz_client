port module Leaflet exposing (..)

type alias Map = Int

port addMap : String -> Cmd msg

port mapAdded : (Map -> msg) -> Sub msg
