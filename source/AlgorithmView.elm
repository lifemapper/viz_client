module AlgorithmView exposing (..)

import Decoder
import Html exposing (Html)
import Html.Attributes as Attributes
import Material
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options
import Material.Card as Card
import Material.Scheme
import ParameterView
import AlgorithmDefinition as D
import Helpers exposing (Index)
import List.Extra exposing (setAt, getAt)


type alias Model =
    { definition : D.Algorithm
    , parameters : List ParameterView.Model
    , truncateDesc : Bool
    , mouseIn : Bool
    , mdl : Material.Model
    }


toApi : Model -> Decoder.Algorithm
toApi { definition, parameters } =
    Decoder.Algorithm
        { code = definition.code
        , parameters = parameters |> List.map ParameterView.toApi
        }


fromApi : Decoder.Algorithm -> Model
fromApi (Decoder.Algorithm { code, parameters }) =
    let
        definition =
            case D.getAlgorithmByCode code of
                Nothing ->
                    Debug.crash ("unknown algorithm code: " ++ code)

                Just def ->
                    def

        params =
            definition.parameters
                |> List.map (ParameterView.initFromValues parameters)
    in
        { definition = definition
        , parameters = params
        , truncateDesc = True
        , mouseIn = False
        , mdl = Material.model
        }


init : D.Algorithm -> Model
init def =
    { definition = def
    , parameters = List.map ParameterView.init def.parameters
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
            getAt i model.parameters
                |> Maybe.map (ParameterView.update msg_)
                |> Maybe.andThen
                    (\( param, cmd ) ->
                        setAt i param model.parameters
                            |> Maybe.map (\params -> ( { model | parameters = params }, Cmd.map (ParameterMsg i) cmd ))
                    )
                |> Maybe.withDefault ( model, Cmd.none )


setRaised : Bool -> Model -> Model
setRaised raised model =
    { model | mouseIn = raised }


raised : Model -> Bool
raised model =
    model.mouseIn || (model.parameters |> List.filter (\p -> p.focused) |> List.isEmpty |> not)


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
        [ List.indexedMap (parameterView index) model.parameters
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
        [ Card.title [ Options.css "padding-right" "48px" ] [ Card.head [] [ Html.text model.definition.name ] ]
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


main : Program Never Model Msg
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
