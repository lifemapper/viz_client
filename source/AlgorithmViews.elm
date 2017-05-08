module AlgorithmViews exposing (..)

import List
import Decoder
    exposing
        ( Algorithm(..)
        , AlgorithmParameters(..)
        , AlgorithmParametersItem(..)
        )
import Html exposing (..)
import Material
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Material.Grid as Grid
import Material.Scheme
import AlgorithmParameters


type alias Index =
    List Int


type alias Model =
    { algorithm : Algorithm
    , mdl : Material.Model
    }


algorithmParametersModel : Model -> AlgorithmParameters.Model
algorithmParametersModel model =
    let
        (Algorithm algorithm) =
            model.algorithm
    in
        { parameters = algorithm.parameters, mdl = model.mdl }


init : Model
init =
    { algorithm =
        Algorithm
            { code = "Example"
            , parameters =
                AlgorithmParameters
                    [ AlgorithmParametersItem { name = "Foo", value = "Bar" }
                    , AlgorithmParametersItem { name = "Boo", value = "Baz" }
                    ]
            }
    , mdl = Material.model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | AlgorithmParametersMsg AlgorithmParameters.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        AlgorithmParametersMsg msg_ ->
            let (mdl, msg__) =
                    AlgorithmParameters.update msg_ <| algorithmParametersModel model
            in
                (M


table : List (Html m) -> Grid.Cell m
table contents =
    Grid.cell
        []
        [ Options.div
            [ css "display" "inline-flex"
            , css "flex-direction" "column"
            , css "width" "auto"
            ]
            contents
        ]



-- parametersView : Model -> Index -> Algorithm -> List (Html Msg)
-- parametersView model index (Algorithm { parameters }) =
--     let
--         (AlgorithmParameters ps) =
--             parameters
--     in
--         List.indexedMap (\i -> parameterView model (i :: index)) ps
-- parameterView : Model -> Index -> AlgorithmParametersItem -> Html Msg
-- parameterView model index (AlgorithmParametersItem i) =
--     p []
--         [ Textfield.render Mdl
--             (0 :: index)
--             model.mdl
--             [ Textfield.label "Name", Textfield.floatingLabel, Textfield.value i.name ]
--             []
--         , Textfield.render Mdl
--             (1 :: index)
--             model.mdl
--             [ Textfield.label "Value", Textfield.floatingLabel, Textfield.value i.value ]
--             []
--         ]


asForm : Model -> Grid.Cell Msg
asForm model =
    let
        code (Algorithm r) =
            r.code
    in
        table
            [ Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Textfield.label "Code", Textfield.value <| code model.algorithm ]
                []
            , h4 [] [ text "Parameters" ]
            , Html.map AlgorithmParametersMsg <| AlgorithmParameters.parametersView [ 1 ] <| algorithmParametersModel model
            ]


view : Model -> Html Msg
view model =
    Grid.grid [] [ asForm model ]
        |> Material.Scheme.top


main =
    Html.program
        { init = ( init, Material.init Mdl )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Material.subscriptions Mdl model
                    ]
        }
