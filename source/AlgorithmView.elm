module AlgorithmViews exposing (..)

import List
import Decoder
    exposing
        ( Algorithm(..)
        , AlgorithmParameters(..)
        , AlgorithmParametersItem(..)
        )
import Html exposing (..)
import Material
import Material.Helpers exposing (lift)
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Material.Grid as Grid
import Material.Scheme
import AlgorithmParametersView


type alias Index =
    List Int


type alias Model =
    { code : String
    , parameters : AlgorithmParametersView.Model
    , mdl : Material.Model
    }


initFromDecoder : Algorithm -> Model
initFromDecoder (Algorithm { code, parameters }) =
    { code = code
    , parameters = AlgorithmParametersView.initFromDecoder parameters
    , mdl = Material.model
    }


init : Model
init =
    { code = "Example"
    , parameters = AlgorithmParametersView.init
    , mdl = Material.model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | UpdateCode String
    | AlgorithmParametersMsg AlgorithmParametersView.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Message" msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        UpdateCode code ->
            ( { model | code = code }, Cmd.none )

        AlgorithmParametersMsg a ->
            lift .parameters (\m x -> { m | parameters = x }) AlgorithmParametersMsg AlgorithmParametersView.update a model


table : List (Html m) -> Grid.Cell m
table contents =
    Grid.cell
        []
        [ Options.div
            [ css "display" "inline-flex"
            , css "flex-direction" "column"
            , css "width" "auto"
            ]
            contents
        ]


view : Index -> Model -> Html Msg
view index model =
    Grid.grid []
        [ table
            [ Textfield.render Mdl
                (0 :: index)
                model.mdl
                [ Textfield.label "Code"
                , Textfield.value model.code
                , Options.onInput UpdateCode
                , Textfield.floatingLabel
                ]
                []
            , h4 [] [ text "Parameters" ]
            , Html.map AlgorithmParametersMsg <| AlgorithmParametersView.view (1 :: index) <| model.parameters
            ]
        ]


main =
    Html.program
        { init = ( init, Material.init Mdl )
        , view = view [] >> Material.Scheme.top
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Material.subscriptions Mdl model
                    ]
        }
