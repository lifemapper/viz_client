module OccurrenceSets exposing (..)

import Decoder exposing (OccurrenceSet(..), OccurrenceSetRecord)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes as A
import Debug


-- import Material

import Leaflet


-- Model


type alias Model =
    { occurrenceSets :
        List OccurrenceSetRecord
        -- , mdl : Material.Model
    , map : Maybe Leaflet.Map
    }


model : Model
model =
    { occurrenceSets =
        []
        -- , mdl = Material.model
    , map = Nothing
    }



-- Action, Update


type Msg
    = OpenMap
    | MapAdded Leaflet.Map



-- = Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Debug.log (toString msg)
        (case msg of
            OpenMap ->
                ( model, Leaflet.addMap "leaflet-map" )

            MapAdded map ->
                ( { model | map = Just map }, Cmd.none )
        )



-- View


view : Model -> Html Msg
view model =
    div []
        [ div
            [ A.id "leaflet-map"
            , A.style
                [ ( "width", "800px" )
                , ( "height", "600px" )
                ]
            ]
            []
        , button [ onClick OpenMap, A.disabled (model.map /= Nothing) ] [ text "Open Map" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Leaflet.mapAdded MapAdded
