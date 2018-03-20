{-
   Copyright (C) 2018, University of Kansas Center for Research

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
encodeBoomPOST (BoomPOST { tree, sdm, scenario_package, pam_stats, occurrence, mcpa, global_pam }) =
    [ ( "occurrence", Maybe.map encodeBoomOccurrenceSet occurrence |> Maybe.withDefault null )
    , ( "scenario_package", Maybe.map encodeBoomScenarioPackage scenario_package |> Maybe.withDefault null )
    , ( "sdm", Maybe.map encodeBoomSDMs sdm |> Maybe.withDefault null )
      -- , ( "tree",  )
    ]
        |> object


encodeBoomOccurrenceSet : BoomOccurrenceSet -> Value
encodeBoomOccurrenceSet (BoomOccurrenceSet { point_count_min, points_filename, occurrence_ids }) =
    [ ( "point_count_min", point_count_min |> Maybe.map int |> Maybe.withDefault null )
    , ( "points_file_name", points_filename |> Maybe.map string |> Maybe.withDefault null )
    , ( "occurrence_ids", occurrence_ids |> Maybe.map encodeBoomOccurrenceSetOccurrence_ids |> Maybe.withDefault null )
    ]
        |> object


encodeBoomOccurrenceSetOccurrence_ids : BoomOccurrenceSetOccurrence_ids -> Value
encodeBoomOccurrenceSetOccurrence_ids (BoomOccurrenceSetOccurrence_ids ids) =
    ids |> List.map int |> list


encodeBoomScenarioPackage : BoomScenarioPackage -> Value
encodeBoomScenarioPackage (BoomScenarioPackage { projection_scenario, model_scenario, scenario_package_filename }) =
    [ ( "projection_scenario", projection_scenario |> Maybe.map encodeScenarioPackageProjection |> Maybe.withDefault null )
    , ( "model_scenario", model_scenario |> Maybe.map encodeScenarioPackageModel |> Maybe.withDefault null )
    , ( "scenario_package_filename", scenario_package_filename |> Maybe.map string |> Maybe.withDefault null )
    ]
        |> object


encodeScenarioPackageProjection : BoomScenarioPackageProjection_scenario -> Value
encodeScenarioPackageProjection (BoomScenarioPackageProjection_scenario items) =
    items |> List.map encodeScenarioItem |> list


encodeScenarioPackageModel : BoomScenarioPackageModel_scenario -> Value
encodeScenarioPackageModel (BoomScenarioPackageModel_scenario { scenario_code }) =
    scenario_code |> Maybe.map string |> Maybe.withDefault null


encodeScenarioItem : BoomScenarioPackageProjection_scenarioItem -> Value
encodeScenarioItem (BoomScenarioPackageProjection_scenarioItem { scenario_code }) =
    scenario_code |> Maybe.map string |> Maybe.withDefault null


encodeBoomSDMs : BoomSDMs -> Value
encodeBoomSDMs (BoomSDMs { hull_region_intersect_mask, algorithm }) =
    [ ( "hull_region_intersect_mask", hull_region_intersect_mask |> Maybe.map encodeBoomSDMsHull |> Maybe.withDefault null )
    , ( "algorithm", algorithm |> encodeBoomSDMsAlgorithm )
    ]
        |> object


encodeBoomSDMsHull : BoomSDMsHull_region_intersect_mask -> Value
encodeBoomSDMsHull _ =
    null


encodeBoomSDMsAlgorithm : BoomSDMsAlgorithm -> Value
encodeBoomSDMsAlgorithm (BoomSDMsAlgorithm algs) =
    list <| List.map encodeAlgorithm algs


encodeAlgorithm : Algorithm -> Value
encodeAlgorithm (Algorithm { code, parameters }) =
    object [ ( "code", string code ), ( "parameters", encodeAlgorithmParameters parameters ) ]


encodeAlgorithmParameters : AlgorithmParameters -> Value
encodeAlgorithmParameters =
    List.map (\( name, value ) -> ( name, String.toFloat value |> Result.toMaybe |> Maybe.map float |> Maybe.withDefault null )) >> object
