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
    { algorithm : Algorithm
    , algorithmParametersViewMdl : Material.Model
    , mdl : Material.Model
    }


algorithmParametersModel : Model -> AlgorithmParametersView.Model
algorithmParametersModel model =
    let
        (Algorithm algorithm) =
            model.algorithm
    in
        { parameters = algorithm.parameters, mdl = model.algorithmParametersViewMdl }


init : Model
init =
    { algorithm =
        Algorithm
            { code = "Example"
            , parameters =
                AlgorithmParameters
                    [ AlgorithmParametersItem { name = "Foo", value = "Bar" }
                    , AlgorithmParametersItem { name = "Boo", value = "Baz" }
                    ]
            }
    , algorithmParametersViewMdl = Material.model
    , mdl = Material.model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | UpdateCode String
    | AlgorithmParametersMsg AlgorithmParametersView.Msg


updateAlgorithmParameters : Model -> AlgorithmParametersView.Model -> Model
updateAlgorithmParameters model submodel =
    let
        (Algorithm algorithm) =
            model.algorithm
    in
        { model
            | algorithm = (Algorithm { algorithm | parameters = submodel.parameters })
            , algorithmParametersViewMdl = submodel.mdl
        }


updateCode : Model -> String -> Model
updateCode model code =
    let
        (Algorithm algorithm) =
            model.algorithm
    in
        { model | algorithm = Algorithm { algorithm | code = code } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Message" msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        UpdateCode s ->
            ( updateCode model s, Cmd.none )

        AlgorithmParametersMsg m ->
            lift algorithmParametersModel updateAlgorithmParameters AlgorithmParametersMsg AlgorithmParametersView.update m model


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
    let
        (Algorithm algorithm) =
            model.algorithm
    in
        Grid.grid []
            [ table
                [ Textfield.render Mdl
                    (0 :: index)
                    model.mdl
                    [ Textfield.label "Code"
                    , Textfield.value algorithm.code
                    , Options.onInput UpdateCode
                    , Textfield.floatingLabel
                    ]
                    []
                , h4 [] [ text "Parameters" ]
                , Html.map AlgorithmParametersMsg <| AlgorithmParametersView.view (1 :: index) <| algorithmParametersModel model
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
