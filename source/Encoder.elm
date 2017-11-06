{-
Copyright (C) 2017, University of Kansas Center for Research

Lifemapper Project, lifemapper [at] ku [dot] edu,
Biodiversity Institute,
1345 Jayhawk Boulevard, Lawrence, Kansas, 66045, USA

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.
-}
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
