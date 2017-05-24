module AlgorithmView exposing (..)

import Array
import Decoder
    exposing
        ( Algorithm(..)
        , AlgorithmParameters(..)
        , AlgorithmParametersItem(..)
        )
import Html exposing (Html)
import Html.Attributes as Attributes
import Material
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Helpers exposing (lift)
import Material.Options as Options
import Material.Card as Card
import Material.Scheme
import ParameterView
import AlgorithmDefinition as D


type alias Index =
    List Int


type alias Model =
    { definition : D.Algorithm
    , parameters : Array.Array ParameterView.Model
    , truncateDesc : Bool
    , expandParams : Bool
    , mdl : Material.Model
    }



-- initFromDecoder : Algorithm -> Model
-- initFromDecoder (Algorithm { code, parameters }) =
--     { code = code
--     , parameters = AlgorithmParametersView.initFromDecoder parameters
--     , mdl = Material.model
--     }
-- toAlgorithm : Model -> Algorithm
-- toAlgorithm { code, parameters } =
--     Algorithm
--         { code = code
--         , parameters = AlgorithmParametersView.toAlgorithmParameters parameters
--         }


init : D.Algorithm -> Model
init def =
    { definition = def
    , parameters = Array.fromList <| List.map ParameterView.init def.parameters
    , truncateDesc = True
    , expandParams = False
    , mdl = Material.model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | ParameterMsg Int ParameterView.Msg
    | ToggleDesc
    | ToggleParams


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDesc ->
            ( { model | truncateDesc = not model.truncateDesc }, Cmd.none )

        ToggleParams ->
            ( { model | expandParams = not model.expandParams }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        ParameterMsg i a ->
            lift
                (\m ->
                    case Array.get i m.parameters of
                        Just m ->
                            m

                        Nothing ->
                            Debug.crash "what?"
                )
                (\m x -> { m | parameters = Array.set i x m.parameters })
                (ParameterMsg i)
                ParameterView.update
                a
                model


descriptionView : Model -> Html Msg
descriptionView model =
    let
        desc =
            model.definition.description
    in
        Options.styled Html.p
            [ Options.onClick ToggleDesc ]
            [ if model.truncateDesc && String.length desc > 100 then
                Html.text <| (String.left 100 desc) ++ "..."
              else
                Html.text desc
            ]


parameterView : Index -> Int -> ParameterView.Model -> Html Msg
parameterView index i model =
    Html.map (ParameterMsg i) <| ParameterView.view (i :: index) <| model


parametersView : Index -> Model -> List (Html Msg)
parametersView index model =
    if model.expandParams then
        [ List.indexedMap (parameterView index) (Array.toList model.parameters)
            |> Html.ul [ Attributes.style [ ( "padding", "0" ), ( "list-style", "none" ) ] ]
        ]
    else
        []


view : Index -> Model -> Html Msg
view index model =
    Card.view [  Elevation.e2 ]
        [ Card.title [] [ Card.head [] [ Html.text model.definition.name ] ]
        , Card.text []
            (descriptionView model :: parametersView index model)
        , Card.actions [ Card.border ]
            [ Button.render Mdl
                (-1 :: index)
                model.mdl
                [ Options.onClick ToggleParams
                ]
                [ Html.text
                    (if model.expandParams then
                        "Hide Parameters"
                     else
                        "Show Parameters"
                    )
                ]
            ]
        ]


exampleAlgorithm : D.Algorithm
exampleAlgorithm =
    case List.head (List.drop 4 D.algorithms) of
        Nothing ->
            Debug.crash "No example algorithm def"

        Just a ->
            a


main =
    Html.program
        { init = ( init exampleAlgorithm, Material.init Mdl )
        , view = view [] >> Material.Scheme.top
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Material.subscriptions Mdl model
                    ]
        }
