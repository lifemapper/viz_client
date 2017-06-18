module AlgorithmsView exposing (..)

import Html exposing (Html)
import Material
import Material.Scheme
import Material.Grid as Grid exposing (Cell, Device(..), grid, cell)
import Material.Helpers exposing (lift)
import AlgorithmView
import AddAlgorithmView
import AlgorithmDefinition as D
import Helpers exposing (Index)
import List.Extra exposing (removeAt, setAt, getAt)
import Decoder


type alias Model =
    { algorithms : List AlgorithmView.Model
    , adder : AddAlgorithmView.Model
    , mdl : Material.Model
    }


toApi : Model -> Decoder.ProjectionPOSTAlgorithms
toApi =
    .algorithms
        >> List.map AlgorithmView.toApi
        >> Decoder.ProjectionPOSTAlgorithms


type Msg
    = Mdl (Material.Msg Msg)
    | AlgorithmMsg Int AlgorithmView.Msg
    | AddAlgorithmMsg AddAlgorithmView.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        AlgorithmMsg i msg_ ->
            case msg_ of
                AlgorithmView.Remove ->
                    ( { model | algorithms = removeAt i model.algorithms }, Cmd.none )

                _ ->
                    getAt i model.algorithms
                        |> Maybe.map (AlgorithmView.update msg_)
                        |> Maybe.andThen
                            (\( alg_, cmd ) ->
                                setAt i alg_ model.algorithms
                                    |> Maybe.map (\algs -> ( { model | algorithms = algs }, Cmd.map (AlgorithmMsg i) cmd ))
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

        AddAlgorithmMsg msg_ ->
            case msg_ of
                AddAlgorithmView.Add def ->
                    ( addAlgorithm def model, Cmd.none )

                _ ->
                    lift
                        .adder
                        (\m x -> { m | adder = x })
                        AddAlgorithmMsg
                        AddAlgorithmView.update
                        msg_
                        model


addAlgorithm : D.Algorithm -> Model -> Model
addAlgorithm def model =
    { model
        | algorithms = model.algorithms ++ [ AlgorithmView.init def |> AlgorithmView.setRaised True ]
        , adder = AddAlgorithmView.setRaised False model.adder
    }


cardSize : Int
cardSize =
    4


viewAlgorithm : Index -> Int -> AlgorithmView.Model -> Cell Msg
viewAlgorithm index i model =
    cell [ Grid.size All cardSize ] [ Html.map (AlgorithmMsg i) <| AlgorithmView.view (i :: index) <| model ]


view : Index -> Model -> Html Msg
view index model =
    let
        alreadyAdded =
            model.algorithms
                |> List.map .definition
    in
        grid [] <|
            List.append (List.indexedMap (viewAlgorithm index) model.algorithms)
                [ cell [ Grid.size All cardSize ] [ Html.map AddAlgorithmMsg <| AddAlgorithmView.view alreadyAdded model.adder ] ]


complete : Model -> Bool
complete model =
    (List.length model.algorithms) > 0


init : Model
init =
    { algorithms = []
    , adder = AddAlgorithmView.init
    , mdl = Material.model
    }


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
