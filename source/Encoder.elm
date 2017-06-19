module Encoder exposing (..)

import Decoder exposing (..)
import Json.Encode exposing (..)


encodeProjectionPOST : ProjectionPOST -> Value
encodeProjectionPOST (ProjectionPOST { projectionScenarios, modelScenario, occurrenceSets, algorithms }) =
    [ ( "projectionScenarios", encodeProjectionPOSTProjectionScenarios projectionScenarios )
    , ( "modelScenario", encodeProjectionPOSTModelScenario modelScenario )
    , ( "occurrenceSets", encodeProjectionPOSTOccurrenceSets occurrenceSets )
    , ( "algorithms", encodeProjectionPOSTAlgorithms algorithms )
    ]
        |> object


encodeProjectionPOSTProjectionScenarios : ProjectionPOSTProjectionScenarios -> Value
encodeProjectionPOSTProjectionScenarios (ProjectionPOSTProjectionScenarios ss) =
    list <| List.map encodeProjectionPOSTProjectionScenariosItem ss


encodeProjectionPOSTProjectionScenariosItem : ProjectionPOSTProjectionScenariosItem -> Value
encodeProjectionPOSTProjectionScenariosItem (ProjectionPOSTProjectionScenariosItem { scenarioId }) =
    object [ ( "scenarioId", scenarioId |> Maybe.map int |> Maybe.withDefault null ) ]


encodeProjectionPOSTModelScenario : ProjectionPOSTModelScenario -> Value
encodeProjectionPOSTModelScenario (ProjectionPOSTModelScenario { scenarioId }) =
    object [ ( "scenarioId", scenarioId |> Maybe.map int |> Maybe.withDefault null ) ]


encodeProjectionPOSTOccurrenceSets : ProjectionPOSTOccurrenceSets -> Value
encodeProjectionPOSTOccurrenceSets (ProjectionPOSTOccurrenceSets os) =
    list <| List.map encodeProjectionPOSTOccurrenceSetsItem os


encodeProjectionPOSTOccurrenceSetsItem : ProjectionPOSTOccurrenceSetsItem -> Value
encodeProjectionPOSTOccurrenceSetsItem (ProjectionPOSTOccurrenceSetsItem { occurrenceSetId }) =
    object [ ( "occurrenceSetId", occurrenceSetId |> Maybe.map int |> Maybe.withDefault null ) ]


encodeProjectionPOSTAlgorithms : ProjectionPOSTAlgorithms -> Value
encodeProjectionPOSTAlgorithms (ProjectionPOSTAlgorithms algs) =
    list <| List.map encodeAlgorithm algs


encodeAlgorithm : Algorithm -> Value
encodeAlgorithm (Algorithm { code, parameters }) =
    object [ ( "code", string code ), ( "parameters", encodeAlgorithmParameters parameters ) ]


encodeAlgorithmParameters : AlgorithmParameters -> Value
encodeAlgorithmParameters =
    List.map (\( name, value ) -> ( name, string value )) >> object
