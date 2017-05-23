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
    , mdl = Material.model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | ParameterMsg Int ParameterView.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


parameterView : Index -> Int -> ParameterView.Model -> Html Msg
parameterView index i model =
    Html.map (ParameterMsg i) <| ParameterView.view (i :: index) <| model


view : Index -> Model -> Html Msg
view index model =
    Card.view [ Options.css "width" "400px" ]
        [ Card.title [] [ Card.head [] [ Html.text model.definition.name ] ]
        , Card.text []
            [ Html.ul [ Attributes.style [ ( "padding", "0" ), ( "list-style", "none" ) ] ] <|
                List.indexedMap (parameterView index) (Array.toList model.parameters)
            ]
        ]


exampleAlgorithm : D.Algorithm
exampleAlgorithm =
    case List.head (List.drop 3 D.algorithms) of
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
