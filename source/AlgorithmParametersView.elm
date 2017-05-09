module AlgorithmParametersView exposing (..)

import Html exposing (Html)
import Material
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options
import Material.List exposing (ul, li)
import Material.Scheme
import Decoder
    exposing
        ( Algorithm(..)
        , AlgorithmParameters(..)
        , AlgorithmParametersItem(..)
        )


type alias Index =
    List Int


type alias ParameterItem =
    ( String, String )


type alias Model =
    { parameters : List ParameterItem
    , mdl : Material.Model
    }


initFromDecoder : AlgorithmParameters -> Model
initFromDecoder (AlgorithmParameters params) =
    { parameters = List.map (\(AlgorithmParametersItem { name, value }) -> ( name, value )) params
    , mdl = Material.model
    }


init : Model
init =
    { parameters = []
    , mdl = Material.model
    }


type NameOrValue
    = Name
    | Value


type Msg
    = Mdl (Material.Msg Msg)
    | Upd8 Int NameOrValue String
    | AddParam NameOrValue String
    | Clear Int


updateParameter : List ParameterItem -> Int -> NameOrValue -> String -> List ParameterItem
updateParameter ps i k s =
    let
        perhapsUpdate j ( name, value ) =
            if j /= i then
                ( name, value )
            else
                (case k of
                    Name ->
                        ( s, value )

                    Value ->
                        ( name, s )
                )
    in
        List.indexedMap perhapsUpdate ps


addParameter : List ParameterItem -> NameOrValue -> String -> List ParameterItem
addParameter ps k s =
    let
        appended =
            List.append ps [ ( "", "" ) ]
    in
        updateParameter appended (List.length ps) k s


remParameter : List ParameterItem -> Int -> List ParameterItem
remParameter ps i =
    (List.take i ps) ++ (List.drop (i + 1) ps)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Upd8 idx k s ->
            ( { model | parameters = updateParameter model.parameters idx k s }, Cmd.none )

        AddParam k s ->
            ( { model | parameters = addParameter model.parameters k s }, Cmd.none )

        Clear idx ->
            ( { model | parameters = remParameter model.parameters idx }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


view : Index -> Model -> Html Msg
view index model =
    let
        parameterView idx ( name, value ) =
            li []
                [ Button.render Mdl
                    (2 :: idx :: index)
                    model.mdl
                    [ Button.minifab
                    , Options.onClick (Clear idx)
                    , Options.css "width" "64px"
                    ]
                    [ Icon.i "clear" ]
                , Textfield.render Mdl
                    (0 :: idx :: index)
                    model.mdl
                    [ Textfield.label "Name"
                    , Textfield.value name
                    , Options.onInput (Upd8 idx Name)
                    , if idx == 0 then
                        Textfield.floatingLabel
                      else
                        Options.nop
                    , Options.css "margin-right" "8px"
                    ]
                    []
                , Textfield.render Mdl
                    (1 :: idx :: index)
                    model.mdl
                    [ Textfield.label "Value"
                    , Textfield.value value
                    , Options.onInput (Upd8 idx Value)
                    , if idx == 0 then
                        Textfield.floatingLabel
                      else
                        Options.nop
                    ]
                    []
                ]
    in
        ul []
            (List.indexedMap parameterView model.parameters
                ++ [ li []
                        [ Options.span [ Options.css "width" "64px", Options.css "display" "inline-block" ] []
                        , Textfield.render Mdl
                            (0 :: (List.length model.parameters) :: index)
                            model.mdl
                            [ Textfield.label "New Parameter Name"
                            , Options.onInput (AddParam Name)
                            , Options.css "margin-right" "8px"
                            ]
                            []
                        , Textfield.render Mdl
                            (1 :: (List.length model.parameters) :: index)
                            model.mdl
                            [ Textfield.label "New Parameter Value"
                            , Options.onInput (AddParam Value)
                            ]
                            []
                        ]
                   ]
            )


main : Program Never Model Msg
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
