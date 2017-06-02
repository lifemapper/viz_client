module ScenariosView exposing (..)

import Material
import Material.Scheme
import Material.Options as Options
import Html exposing (Html)
import Helpers exposing (Index)


type alias Model =
    { mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model


view : Index -> Model -> Html Msg
view index model =
    Options.div
        [ Options.id "leaflet-map"
        , Options.css "width" "800px"
        , Options.css "height" "600px"
          -- , Options.css "display" "none" |> Options.when (model.selectedTab /= Scenario)
        ]
        []


init : Model
init =
    { mdl = Material.model
    }


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Material.init Mdl )
        , view = view [] >> Material.Scheme.top
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
