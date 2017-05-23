module ParameterView exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Material
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Tooltip as Tooltip
import Material.Toggles as Toggles
import Material.Scheme
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


init : D.Parameter -> Model
init def =
    { definition = def
    , value = Maybe.withDefault "" def.default
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


renderValidation :
    { a | validationResult : ValidationResult }
    -> Textfield.Property m
renderValidation model =
    case model.validationResult of
        Valid ->
            Options.nop

        Invalid error ->
            Textfield.error error


view : Index -> Model -> Html Msg
view idx model =
    case model.definition.options of
        [] ->
            viewField idx model

        _ ->
            viewOptions idx model


viewField : Index -> Model -> Html Msg
viewField idx model =
    Html.li [ Attributes.style [ ( "padding", "4px" ) ] ]
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


viewOptions : Index -> Model -> Html Msg
viewOptions idx model =
    Html.li [ Attributes.style [ ( "padding", "4px" ) ] ]
        [ Html.p [] [ Html.text model.definition.displayName ]
        , Html.ul [ Attributes.style [ ( "margin", "0" ), ( "padding", "0" ), ("list-style", "none") ] ]
            (List.indexedMap (optionView idx model) model.definition.options)
        ]


optionView : Index -> Model -> Int -> D.ParameterOption -> Html Msg
optionView idx model i option =
    Html.li []
        [ Toggles.radio Mdl
            (i :: idx)
            model.mdl
            [ Toggles.group (toString idx)
            , Toggles.value (toString option.value == model.value)
            , Options.onToggle (Update <| toString option.value)
            ]
            [ Html.text option.name ]
        ]


exView : Model -> Html Msg
exView model =
    Html.ul [ Attributes.style [ ( "padding", "0" ) ] ]
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
    case List.head (List.drop 3 exampleAlgorithm.parameters) of
        Nothing ->
            Debug.crash "Example algorithm has no params"

        Just p ->
            p


main : Program Never Model Msg
main =
    Html.program
        { init = ( init exampleParameter, Material.init Mdl )
        , view = exView >> Material.Scheme.top
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Material.subscriptions Mdl model
                    ]
        }
