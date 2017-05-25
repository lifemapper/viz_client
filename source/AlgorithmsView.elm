module AlgorithmsView exposing (..)

import Array
import Html exposing (Html)
import Material
import Material.Scheme
import Material.Grid as Grid exposing (Cell, Device(..), grid, cell)
import Material.Helpers exposing (lift)
import AlgorithmView
import AlgorithmDefinition as D
import Helpers exposing (Index, unsafeGet)


type alias Model =
    { algorithms : Array.Array AlgorithmView.Model
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | AlgorithmMsg Int AlgorithmView.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        AlgorithmMsg i msg_ ->
            lift
                (.algorithms >> unsafeGet i)
                (\m x -> { m | algorithms = Array.set i x m.algorithms })
                (AlgorithmMsg i)
                AlgorithmView.update
                msg_
                model


viewAlgorithm : Index -> Int -> AlgorithmView.Model -> Cell Msg
viewAlgorithm index i model =
    cell [ Grid.size All 2 ] [ Html.map (AlgorithmMsg i) <| AlgorithmView.view (i :: index) <| model ]


view : Index -> Model -> Html Msg
view index model =
    grid [] <|
        List.indexedMap (viewAlgorithm index) (Array.toList model.algorithms)


init : Model
init =
    { algorithms = Array.fromList <| List.map AlgorithmView.init D.algorithms
    , mdl = Material.model
    }


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
