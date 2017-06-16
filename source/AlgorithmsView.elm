module AlgorithmsView exposing (..)

import Array
import Html exposing (Html)
import Material
import Material.Scheme
import Material.Grid as Grid exposing (Cell, Device(..), grid, cell)
import Material.Helpers exposing (lift)
import AlgorithmView
import AddAlgorithmView
import AlgorithmDefinition as D
import Helpers exposing (Index, unsafeGet, removeElem)
import Decoder


type alias Model =
    { algorithms : Array.Array AlgorithmView.Model
    , adder : AddAlgorithmView.Model
    , mdl : Material.Model
    }


toApi : Model -> Decoder.ProjectionPOSTAlgorithms
toApi =
    .algorithms
        >> Array.toList
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
                    ( { model | algorithms = removeElem i model.algorithms }, Cmd.none )

                _ ->
                    lift
                        (.algorithms >> unsafeGet i)
                        (\m x -> { m | algorithms = Array.set i x m.algorithms })
                        (AlgorithmMsg i)
                        AlgorithmView.update
                        msg_
                        model

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
        | algorithms = Array.push (AlgorithmView.init def |> AlgorithmView.setRaised True) model.algorithms
        , adder = AddAlgorithmView.setRaised False model.adder
    }


viewAlgorithm : Index -> Int -> AlgorithmView.Model -> Cell Msg
viewAlgorithm index i model =
    cell [ Grid.size All 2 ] [ Html.map (AlgorithmMsg i) <| AlgorithmView.view (i :: index) <| model ]


view : Index -> Model -> Html Msg
view index model =
    let
        alreadyAdded =
            model.algorithms
                |> Array.toList
                |> List.map .definition
    in
        grid [] <|
            List.append (List.indexedMap (viewAlgorithm index) (Array.toList model.algorithms))
                [ cell [ Grid.size All 2 ] [ Html.map AddAlgorithmMsg <| AddAlgorithmView.view alreadyAdded model.adder ] ]


complete : Model -> Bool
complete model =
    (Array.length model.algorithms) > 0


init : Model
init =
    { algorithms = Array.empty
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
