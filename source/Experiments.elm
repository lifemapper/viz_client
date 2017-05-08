module Experiments exposing (..)

import Decoder exposing (ExperimentRecord, ExperimentModel(..))
import Html exposing (Html, text)
import Material
import Material.Table
    exposing
        ( table
        , tr
        , th
        , td
        , thead
        , tbody
        , ascending
        , descending
        )


-- Model


type alias Model =
    { experiments : List ExperimentRecord
    , mdl : Material.Model
    }


model : Model
model =
    { experiments =
        [ { user = Just "Ben"
          , statusModTime = Just "2017-05-01"
          , serviceType = Just "Best"
          , projections = Nothing
          , moduleType = Nothing
          , model =
                Just <|
                    ExperimentModel
                        { user = Just "Ben"
                        , statusModTime = Nothing
                        , status = Nothing
                        , serviceType = Nothing
                        , scenarioCode = Nothing
                        , ruleset = Nothing
                        , qualityControl = Nothing
                        , priority = Nothing
                        , pointsName = Nothing
                        , occurrenceSet = Nothing
                        , name = Just "Kraken"
                        , moduleType = Nothing
                        , modTime = Nothing
                        , metadataUrl = Nothing
                        , mapName = Nothing
                        , mapFilename = Nothing
                        , makeflowFilename = Nothing
                        , layers = Nothing
                        , id = Just 23
                        , epsgcode = Nothing
                        , createTime = Nothing
                        , bbox = Nothing
                        , algorithmCode = Nothing
                        }
          , modTime = Just "2017-05-01"
          , metadataUrl = Nothing
          , id = Just 10
          , epsgcode = Nothing
          , createTime = Nothing
          , bbox = Nothing
          , algorithm = Nothing
          }
        , { user = Just "Juju"
          , statusModTime = Just "2017-05-02"
          , serviceType = Just "Best"
          , projections = Nothing
          , moduleType = Nothing
          , model =
                Just <|
                    ExperimentModel
                        { user = Just "Juju"
                        , statusModTime = Nothing
                        , status = Nothing
                        , serviceType = Nothing
                        , scenarioCode = Nothing
                        , ruleset = Nothing
                        , qualityControl = Nothing
                        , priority = Nothing
                        , pointsName = Nothing
                        , occurrenceSet = Nothing
                        , name = Just "Kaiju"
                        , moduleType = Nothing
                        , modTime = Nothing
                        , metadataUrl = Nothing
                        , mapName = Nothing
                        , mapFilename = Nothing
                        , makeflowFilename = Nothing
                        , layers = Nothing
                        , id = Just 23
                        , epsgcode = Nothing
                        , createTime = Nothing
                        , bbox = Nothing
                        , algorithmCode = Nothing
                        }
          , modTime = Just "2017-05-02"
          , metadataUrl = Nothing
          , id = Just 10
          , epsgcode = Nothing
          , createTime = Nothing
          , bbox = Nothing
          , algorithm = Nothing
          }
        ]
    , mdl = Material.model
    }



-- Action, Update


type Msg
    = Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    table []
        [ thead []
            [ tr []
                [ th [ descending ] [ text "Date" ]
                , th [ ascending ] [ text "Name" ]
                ]
            ]
        , tbody [] (List.map renderRow model.experiments)
        ]


renderRow : ExperimentRecord -> Html Msg
renderRow record =
    tr []
        [ td [] [ text <| Maybe.withDefault "" record.modTime ]
        , td []
            [ text <|
                Maybe.withDefault "" <|
                    Maybe.andThen (\(ExperimentModel r) -> r.name)
                        record.model
            ]
        ]


type alias Mdl =
    Material.Model


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Material.init Mdl )
        , view = view
        , subscriptions = Material.subscriptions Mdl
        , update = update
        }
