module ProjectionPostView exposing (..)

import Debug exposing (..)
import Array exposing (Array)
import Html exposing (..)
import Material
import Material.Helpers exposing (lift, pure)
import Material.Scheme
import Material.Button as Button
import Material.Options as Options
import Material.Icon as Icon
import AlgorithmView
import Decoder
    exposing
        ( ProjectionPOST(..)
        , ProjectionPOSTAlgorithms(..)
        )


type alias Index =
    List Int



-- type OccurrenceID
--     = OccurrenceID Int
-- type ScenarioRef
--     = ScenarioID Int
--     | ScenarioCode String


type alias Model =
    { algorithms :
        Array AlgorithmView.Model
        -- , occurrenceSets : Array OccurrenceID
        -- , modelScenario : ScenarioRef
        -- , projectionScenarios : Array ScenarioRef
    , mdl : Material.Model
    }


init : Model
init =
    { algorithms = Array.empty
    , mdl = Material.model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | AddAlgorithm
      -- | RemoveAlgorithm Int
    | AlgorithmViewMsg Int AlgorithmView.Msg


unsafeGet : Int -> Array a -> a
unsafeGet i xs =
    case Array.get i xs of
        Nothing ->
            Debug.crash "Badness"

        Just x ->
            x


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddAlgorithm ->
            pure { model | algorithms = Array.push AlgorithmView.init model.algorithms }

        AlgorithmViewMsg i a ->
            lift (.algorithms >> (unsafeGet i))
                (\m x -> { m | algorithms = Array.set i x m.algorithms })
                (AlgorithmViewMsg i)
                AlgorithmView.update
                a
                model

        Mdl msg_ ->
            Material.update Mdl msg_ model


viewAlgorithm : Index -> Int -> AlgorithmView.Model -> Html Msg
viewAlgorithm index i algorithm =
    Html.map (AlgorithmViewMsg i) <| AlgorithmView.view (i :: index) algorithm


view : Index -> Model -> Html Msg
view index model =
    div []
        [ div [] <|
            Array.toList <|
                Array.indexedMap (viewAlgorithm (0 :: index)) model.algorithms
        , div []
            [ Button.render Mdl
                (1 :: index)
                model.mdl
                [ Options.onClick (AddAlgorithm) ]
                [ Icon.i "add" ]
            ]
        ]


main =
    Html.program
        { init = ( init, Material.init Mdl )
        , view = view [] >> Material.Scheme.top
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
