module ParameterView exposing (..)

import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Material
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Tooltip as Tooltip
import Material.Toggles as Toggles
import Material.Scheme
import AlgorithmDefinition as D
import Helpers exposing (Index)


type alias Model =
    { definition : D.Parameter
    , value : String
    , validationResult : Result String String
    , mdl : Material.Model
    }


init : D.Parameter -> Model
init def =
    let
        initValue =
            Maybe.withDefault "" def.default
    in
        { definition = def
        , value = initValue
        , validationResult = validate def initValue
        , mdl = Material.model
        }


type Msg
    = Mdl (Material.Msg Msg)
    | Update String


boundErrMsg : Order -> comparable -> String
boundErrMsg reject bound =
    (case reject of
        LT ->
            "Value must not be less than "

        GT ->
            "Value must not be greater than "

        EQ ->
            "Value must not equal "
    )
        ++ (toString bound)
        ++ "."


checkBound : Maybe comparable -> Order -> comparable -> Result String comparable
checkBound bound reject value =
    Maybe.withDefault (Ok value) <|
        Maybe.map
            (\b ->
                if compare value b /= reject then
                    Ok value
                else
                    Err (boundErrMsg reject b)
            )
            bound


checkBounds :
    (String -> Result String comparable)
    -> Maybe String
    -> Maybe String
    -> String
    -> Result String String
checkBounds parse min max value =
    let
        maybeParse =
            (parse >> Result.toMaybe) |> Maybe.andThen
    in
        parse value
            |> Result.andThen (checkBound (maybeParse min) LT)
            |> Result.andThen (checkBound (maybeParse max) GT)
            |> Result.map (always value)


validate : D.Parameter -> String -> Result String String
validate definition value =
    case definition.dataType of
        D.IntegerParam ->
            checkBounds String.toInt definition.min definition.max value

        D.FloatParam ->
            checkBounds String.toFloat definition.min definition.max value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Update value ->
            ( { model | value = value, validationResult = validate model.definition value }, Cmd.none )


renderValidation : Model -> Textfield.Property m
renderValidation model =
    case model.validationResult of
        Ok _ ->
            Options.nop

        Err error ->
            Textfield.error error


view : Index -> Model -> Html Msg
view idx model =
    case model.definition.options of
        [] ->
            viewField idx model

        options ->
            case List.filter (\{ name } -> name /= "Yes" && name /= "No") options of
                [] ->
                    viewSwitch idx model

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
        , Html.ul [ Attributes.style [ ( "margin", "0" ), ( "padding", "0" ), ( "list-style", "none" ) ] ]
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


viewSwitch : Index -> Model -> Html Msg
viewSwitch idx model =
    let
        options =
            model.definition.options
                |> List.map (\{ name, value } -> ( name, value ))
                |> Dict.fromList

        yes =
            options |> Dict.get "Yes" |> Maybe.map toString |> Maybe.withDefault ""

        no =
            options |> Dict.get "No" |> Maybe.map toString |> Maybe.withDefault ""

        toggle =
            if model.value == yes then
                Update no
            else
                Update yes
    in
        Html.li [ Attributes.style [ ( "padding", "4px" ) ] ]
            [ Toggles.switch Mdl
                idx
                model.mdl
                [ Options.onToggle toggle, Toggles.value (model.value == yes) ]
                [ Html.text model.definition.displayName ]
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
