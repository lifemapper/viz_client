port module Leaflet exposing (Model, Msg(..), update, init)


port updateMapState : Model -> Cmd msg


type alias Model =
    { mapName : String
    , layers : List String
    , endPoint : String
    }


init : Model
init =
    { mapName = "", layers = [], endPoint = "" }


type Msg
    = SetMap String String (List String)


updateState : Msg -> Model -> Model
updateState msg model =
    case msg of
        SetMap endPoint mapName layers ->
            ({ model | endPoint = endPoint, mapName = mapName, layers = layers })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        state =
            updateState msg model
    in
        ( state, updateMapState state )
