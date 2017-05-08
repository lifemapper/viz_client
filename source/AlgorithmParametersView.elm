module AlgorithmParameters exposing (..)

import Material
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Icon as Icon
import Material.Scheme
import Material.Options as Options
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


type NameOrValue
    = Name
    | Value


type Msg
    = Mdl (Material.Msg Msg)
    | Upd8 Int NameOrValue String
    | AddParam NameOrValue String
    | Clear Int


updateParameter : AlgorithmParameters -> Int -> NameOrValue -> String -> AlgorithmParameters
updateParameter (AlgorithmParameters ps) i k s =
    let
        perhapsUpdate j (AlgorithmParametersItem item) =
            if j /= i then
                AlgorithmParametersItem item
            else
                AlgorithmParametersItem
                    (case k of
                        Name ->
                            ({ item | name = s })

                        Value ->
                            ({ item | value = s })
                    )
    in
        AlgorithmParameters (List.indexedMap perhapsUpdate ps)


addParameter : AlgorithmParameters -> NameOrValue -> String -> AlgorithmParameters
addParameter (AlgorithmParameters ps) k s =
    let
        appended =
            List.append ps [ AlgorithmParametersItem { name = "", value = "" } ]
    in
        updateParameter (AlgorithmParameters appended) (List.length ps) k s


remParameter : AlgorithmParameters -> Int -> AlgorithmParameters
remParameter (AlgorithmParameters ps) i =
    AlgorithmParameters ((List.take i ps) ++ (List.drop (i + 1) ps))


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
        (AlgorithmParameters algorithmParameters) =
            model.parameters

        parameterView idx (AlgorithmParametersItem i) =
            p []
                [ Textfield.render Mdl
                    (0 :: idx :: index)
                    model.mdl
                    [ Textfield.label "Name"
                    , Textfield.value i.name
                    , Options.onInput (Upd8 idx Name)
                    , Textfield.floatingLabel
                    ]
                    []
                , Textfield.render Mdl
                    (1 :: idx :: index)
                    model.mdl
                    [ Textfield.label "Value"
                    , Textfield.value i.value
                    , Textfield.floatingLabel
                    , Options.onInput (Upd8 idx Value)
                    ]
                    []
                , Button.render Mdl
                    (2 :: idx :: index)
                    model.mdl
                    [ Button.minifab
                    , Options.onClick (Clear idx)
                    ]
                    [ Icon.i "clear" ]
                ]
    in
        div []
            (List.indexedMap parameterView algorithmParameters
                ++ [ p []
                        [ Textfield.render Mdl
                            (0 :: -1 :: index)
                            model.mdl
                            [ Textfield.label "New Parameter Name"
                            , Options.onInput (AddParam Name)
                            ]
                            []
                        , Textfield.render Mdl
                            (1 :: -1 :: index)
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
        , view = view [ 0 ] >> Material.Scheme.top
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Material.subscriptions Mdl model
                    ]
        }
