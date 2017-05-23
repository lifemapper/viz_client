module ParameterView exposing (..)

import Html exposing (Html)
import Material
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options
import Material.Tooltip as Tooltip
import Material.List exposing (ul, li)
import Material.Scheme
import Decoder exposing (AlgorithmParametersItem(..))
import AlgorithmDefinition as D


type alias Index =
    List Int


type ValidationResult
    = Valid
    | Invalid String


type alias Model =
    { definition : D.Parameter
    , value : String
    , validationResult : ValidationResult
    , mdl : Material.Model
    }


init : Model
init =
    { definition = exampleParameter
    , value = ""
    , validationResult = Valid
    , mdl = Material.model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | Update String


validate : D.Parameter -> String -> ValidationResult
validate definition value =
    case definition.dataType of
        D.IntegerParam ->
            case String.toInt value of
                Ok v ->
                    Valid

                Err err ->
                    Invalid err

        D.FloatParam ->
            case String.toFloat value of
                Ok v ->
                    Valid

                Err err ->
                    Invalid err


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Update value ->
            ( { model | value = value, validationResult = validate model.definition value }, Cmd.none )


renderValidation model =
    case model.validationResult of
        Valid ->
            Options.nop

        Invalid error ->
            Textfield.error error


view : Index -> Model -> Html Msg
view idx model =
    li [ Options.css "padding" "4px" ]
        [ Textfield.render Mdl
            (0 :: idx)
            model.mdl
            [ Textfield.label model.definition.displayName
            , Textfield.value model.value
            , Options.onInput Update
            , Textfield.floatingLabel
            , renderValidation model
            , Tooltip.attach Mdl (1 :: idx)
            ]
            []
        , Tooltip.render Mdl
            (1 :: idx)
            model.mdl
            []
            [ Html.text model.definition.doc ]
        ]


exView : Model -> Html Msg
exView model =
    ul [ Options.css "padding" "0" ]
        [ view [] model ]


exampleAlgorithm : D.Algorithm
exampleAlgorithm =
    case List.head D.algorithms of
        Nothing ->
            Debug.crash "No example algorithm def"

        Just a ->
            a


exampleParameter : D.Parameter
exampleParameter =
    case List.head exampleAlgorithm.parameters of
        Nothing ->
            Debug.crash "Example algorithm has no params"

        Just p ->
            p


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Material.init Mdl )
        , view = exView >> Material.Scheme.top
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Material.subscriptions Mdl model
                    ]
        }
