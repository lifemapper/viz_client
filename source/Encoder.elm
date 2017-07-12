module Encoder exposing (..)

import Decoder exposing (..)
import Json.Encode exposing (..)


encodeBoomPOST : BoomPOST -> Value
encodeBoomPOST (BoomPOST { projectionScenarios, modelScenario, occurrenceSets, algorithms }) =
    [ ( "projectionScenarios", encodeBoomPOSTProjectionScenarios projectionScenarios )
    , ( "modelScenario", encodeBoomPOSTModelScenario modelScenario )
    , ( "occurrenceSets", encodeBoomPOSTOccurrenceSets occurrenceSets )
    , ( "algorithms", encodeBoomPOSTAlgorithms algorithms )
    ]
        |> object


encodeBoomPOSTProjectionScenarios : BoomPOSTProjectionScenarios -> Value
encodeBoomPOSTProjectionScenarios (BoomPOSTProjectionScenarios ss) =
    list <| List.map encodeBoomPOSTProjectionScenariosItem ss


encodeBoomPOSTProjectionScenariosItem : BoomPOSTProjectionScenariosItem -> Value
encodeBoomPOSTProjectionScenariosItem (BoomPOSTProjectionScenariosItem { scenarioCode }) =
    object [ ( "scenarioCode", scenarioCode |> Maybe.map string |> Maybe.withDefault null ) ]


encodeBoomPOSTModelScenario : BoomPOSTModelScenario -> Value
encodeBoomPOSTModelScenario (BoomPOSTModelScenario { scenarioCode }) =
    object [ ( "scenarioCode", scenarioCode |> Maybe.map string |> Maybe.withDefault null ) ]


encodeBoomPOSTOccurrenceSets : BoomPOSTOccurrenceSets -> Value
encodeBoomPOSTOccurrenceSets (BoomPOSTOccurrenceSets os) =
    list <| List.map encodeBoomPOSTOccurrenceSetsItem os


encodeBoomPOSTOccurrenceSetsItem : BoomPOSTOccurrenceSetsItem -> Value
encodeBoomPOSTOccurrenceSetsItem (BoomPOSTOccurrenceSetsItem { occurrenceSetId }) =
    object [ ( "occurrenceSetId", occurrenceSetId |> Maybe.map int |> Maybe.withDefault null ) ]


encodeBoomPOSTAlgorithms : BoomPOSTAlgorithms -> Value
encodeBoomPOSTAlgorithms (BoomPOSTAlgorithms algs) =
    list <| List.map encodeAlgorithm algs


encodeAlgorithm : Algorithm -> Value
encodeAlgorithm (Algorithm { code, parameters }) =
    object [ ( "code", string code ), ( "parameters", encodeAlgorithmParameters parameters ) ]


encodeAlgorithmParameters : AlgorithmParameters -> Value
encodeAlgorithmParameters =
    List.map (\( name, value ) -> ( name, String.toFloat value |> Result.toMaybe |> Maybe.map float |> Maybe.withDefault null )) >> object
