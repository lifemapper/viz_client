module AlgorithmParameters exposing (..)

import Material
import Material.Textfield as Textfield
import Material.Scheme
import Html exposing (..)
import Decoder
    exposing
        ( Algorithm(..)
        , AlgorithmParameters(..)
        , AlgorithmParametersItem(..)
        )


type alias Index =
    List Int


type alias Model =
    { parameters : AlgorithmParameters
    , mdl : Material.Model
    }


init : Model
init =
    { parameters =
        AlgorithmParameters
            [ AlgorithmParametersItem { name = "Foo", value = "Bar" }
            , AlgorithmParametersItem { name = "Boo", value = "Baz" }
            ]
    , mdl = Material.model
    }


type Msg
    = Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model


parametersView : Index -> Model -> Html Msg
parametersView index model =
    let
        (AlgorithmParameters algorithmParameters) =
            model.parameters

        parameterView idx (AlgorithmParametersItem i) =
            p []
                [ Textfield.render Mdl
                    (0 :: idx :: index)
                    model.mdl
                    [ Textfield.label "Name", Textfield.floatingLabel, Textfield.value i.name ]
                    []
                , Textfield.render Mdl
                    (1 :: idx :: index)
                    model.mdl
                    [ Textfield.label "Value", Textfield.floatingLabel, Textfield.value i.value ]
                    []
                ]
    in
        div [] <| List.indexedMap parameterView algorithmParameters


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Material.init Mdl )
        , view = parametersView [ 0 ] >> Material.Scheme.top
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Material.subscriptions Mdl model
                    ]
        }
