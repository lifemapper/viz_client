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
import Material.Icon as Icon
import Material.Helpers exposing (lift)
import Material.Options as Options
import Material.Card as Card
import Material.Scheme
import ParameterView
import AlgorithmDefinition as D
import Helpers exposing (Index, unsafeGet)


type alias Model =
    { definition : D.Algorithm
    , parameters : Array.Array ParameterView.Model
    , truncateDesc : Bool
    , mouseIn : Bool
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
    , mouseIn = False
    , mdl = Material.model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | ParameterMsg Int ParameterView.Msg
    | ToggleDesc
    | MouseIn Bool
    | Remove


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDesc ->
            ( { model | truncateDesc = not model.truncateDesc }, Cmd.none )

        MouseIn mouseIn ->
            ( { model | mouseIn = mouseIn }, Cmd.none )

        Remove ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        ParameterMsg i msg_ ->
            lift
                (.parameters >> unsafeGet i)
                (\m x -> { m | parameters = Array.set i x m.parameters })
                (ParameterMsg i)
                ParameterView.update
                msg_
                model


setRaised : Bool -> Model -> Model
setRaised raised model =
    { model | mouseIn = raised }


raised : Model -> Bool
raised model =
    model.mouseIn || (model.parameters |> Array.filter (\p -> p.focused) |> Array.isEmpty |> not)


descriptionView : Model -> Html Msg
descriptionView model =
    let
        desc =
            model.definition.description
    in
        Options.styled Html.p
            [ Options.onClick ToggleDesc ]
            [ if ((not <| raised model) || model.truncateDesc) && String.length desc > 100 then
                Html.text <| (String.left 100 desc) ++ "..."
              else
                Html.text desc
            ]


parameterView : Index -> Int -> ParameterView.Model -> Html Msg
parameterView index i model =
    Html.map (ParameterMsg i) <| ParameterView.view (i :: index) <| model


parametersView : Index -> Model -> List (Html Msg)
parametersView index model =
    if raised model then
        [ List.indexedMap (parameterView index) (Array.toList model.parameters)
            |> Html.ul [ Attributes.style [ ( "padding", "0" ), ( "list-style", "none" ) ] ]
        ]
    else
        []


view : Index -> Model -> Html Msg
view index model =
    Card.view
        [ Elevation.e8 |> Options.when (raised model)
        , Elevation.e2 |> Options.when (not <| raised model)
        , Options.onMouseEnter (MouseIn True)
        , Options.onMouseLeave (MouseIn False)
        , Options.css "width" "100%"
        ]
        [ Card.title [] [ Card.head [] [ Html.text model.definition.name ] ]
        , Card.menu []
            [ Button.render Mdl
                (-1 :: index)
                model.mdl
                [ Button.icon, Options.onClick Remove ]
                [ Icon.i "delete" ]
            ]
        , Card.text []
            (descriptionView model :: parametersView index model)
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
