port module Leaflet exposing (Model, Msg(..), update, init)

type alias View =
    { lat : Float, lon : Float, zoom : Int }


port drawMap : (String, View) -> Cmd msg


port destroyMap : String -> Cmd msg


port setMap : (String, View) -> Cmd msg

id : String
id = "leaflet-map"

type Model
    = NoMap
    | Map View


init : Model
init = NoMap


type Msg
    = Destroy
    | Draw View


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NoMap ->
            case msg of
                Draw view ->
                    ( Map view, drawMap (id, view) )

                Destroy ->
                    ( model, Cmd.none )

        Map current ->
            case msg of
                Draw view ->
                    ( Map view, setMap (id, view) )

                Destroy ->
                    ( NoMap, destroyMap id )

