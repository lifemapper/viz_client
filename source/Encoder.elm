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
import Maybe.Extra as Maybe


encodeGbifPost : GbifPost -> Value
encodeGbifPost (GbifPost names) =
    names |> List.map string |> list


encodeOccurrenceMetadata : OccurrenceMetadata -> Value
encodeOccurrenceMetadata (OccurrenceMetadata { role, field }) =
    [ role |> Maybe.map (encodeOccurrenceMetadataRole >> (,) "role")
    , field |> Maybe.map (encodeOccurrenceMetadataField >> (,) "field")
    ]
        |> List.concatMap Maybe.toList
        |> object


encodeOccurrenceMetadataRole : OccurrenceMetadataRole -> Value
encodeOccurrenceMetadataRole (OccurrenceMetadataRole { uniqueId, taxaName, longitude, latitude, groupBy, geopoint }) =
    [ uniqueId |> Maybe.map (string >> (,) "uniqueId")
    , taxaName |> Maybe.map (string >> (,) "taxaName")
    , longitude |> Maybe.map (string >> (,) "longitude")
    , latitude |> Maybe.map (string >> (,) "latitude")
    , geopoint |> Maybe.map (string >> (,) "geopoint")
    , groupBy |> string |> (,) "groupBy" |> Just
    ]
        |> List.concatMap Maybe.toList
        |> object


encodeOccurrenceMetadataField : OccurrenceMetadataField -> Value
encodeOccurrenceMetadataField (OccurrenceMetadataField fields) =
    fields |> List.map encodeOccurrenceMetadataFieldItem |> list


encodeOccurrenceMetadataFieldItem : OccurrenceMetadataFieldItem -> Value
encodeOccurrenceMetadataFieldItem (OccurrenceMetadataFieldItem { key, shortName, fieldType }) =
    [ key |> Maybe.map (string >> (,) "key")
    , shortName |> Maybe.map (string >> (,) "shortName")
    , fieldType |> Maybe.map (encodeOccurrenceMetadataFieldItemFieldType >> (,) "fieldType")
    ]
        |> List.concatMap Maybe.toList
        |> object


encodeOccurrenceMetadataFieldItemFieldType : OccurrenceMetadataFieldItemFieldType -> Value
encodeOccurrenceMetadataFieldItemFieldType fieldType =
    case fieldType of
        String ->
            string "string"

        Integer ->
            string "integer"

        Real ->
            string "real"


encodeBoomPOST : BoomPOST -> Value
encodeBoomPOST (BoomPOST { tree, sdm, scenario_package, pam_stats, occurrence, mcpa, global_pam, archive_name }) =
    [ occurrence |> Maybe.map (encodeBoomOccurrenceSet >> (,) "occurrence")
    , scenario_package |> Maybe.map (encodeBoomScenarioPackage >> (,) "scenario_package")
    , sdm |> Maybe.map (encodeBoomSDMs >> (,) "sdm")
    , tree |> Maybe.map (encodeBoomTree >> (,) "tree")
    , mcpa |> Maybe.map (encodeBoomMcpa >> (,) "mcpa")
    , pam_stats |> Maybe.map (encodeBoomPAMStats >> (,) "pam_stats")
    , archive_name |> Maybe.map (string >> (,) "archive_name")
    ]
        |> List.concatMap Maybe.toList
        |> object


encodeBoomMcpa : BoomMCPA -> Value
encodeBoomMcpa (BoomMCPA { hypotheses_package_name }) =
    object [ ( "hypotheses_package_name", string hypotheses_package_name ) ]


encodeBoomPAMStats : BoomPAMStats -> Value
encodeBoomPAMStats (BoomPAMStats { compute_pam_stats }) =
    object [ ( "compute_pam_stats", int compute_pam_stats ) ]


encodeBoomTree : BoomPOSTTree -> Value
encodeBoomTree (BoomPOSTTree { tree_file_name }) =
    tree_file_name
        |> Maybe.map (string >> (,) "tree_file_name")
        |> Maybe.toList
        |> object


encodeBoomOccurrenceSet : BoomOccurrenceSet -> Value
encodeBoomOccurrenceSet (BoomOccurrenceSet { point_count_min, points_filename, occurrence_ids, taxon_ids, taxon_names }) =
    [ point_count_min |> Maybe.map (int >> (,) "point_count_min")
    , points_filename |> Maybe.map (string >> (,) "points_filename")
    , occurrence_ids |> Maybe.map (encodeBoomOccurrenceSetOccurrence_ids >> (,) "occurrence_ids")
    , taxon_ids |> Maybe.map (encodeBoomOccurrenceSetTaxon_ids >> (,) "taxon_ids")
    , taxon_names |> Maybe.map (encodeBoomOccurrenceSetTaxon_names >> (,) "taxon_names")
    ]
        |> List.concatMap Maybe.toList
        |> object


encodeBoomOccurrenceSetTaxon_ids : BoomOccurrenceSetTaxon_ids -> Value
encodeBoomOccurrenceSetTaxon_ids (BoomOccurrenceSetTaxon_ids ids) =
    ids |> List.map int |> list


encodeBoomOccurrenceSetTaxon_names : BoomOccurrenceSetTaxon_names -> Value
encodeBoomOccurrenceSetTaxon_names (BoomOccurrenceSetTaxon_names names) =
    names |> List.map string |> list


encodeBoomOccurrenceSetOccurrence_ids : BoomOccurrenceSetOccurrence_ids -> Value
encodeBoomOccurrenceSetOccurrence_ids (BoomOccurrenceSetOccurrence_ids ids) =
    ids |> List.map int |> list


encodeBoomScenarioPackage : BoomScenarioPackage -> Value
encodeBoomScenarioPackage (BoomScenarioPackage { projection_scenario, model_scenario, scenario_package_filename }) =
    [ projection_scenario |> Maybe.map (encodeScenarioPackageProjection >> (,) "projection_scenario")
    , model_scenario |> Maybe.map (encodeScenarioPackageModel >> (,) "model_scenario")
    , scenario_package_filename |> Maybe.map (string >> (,) "scenario_package_filename")
    ]
        |> List.concatMap Maybe.toList
        |> object


encodeScenarioPackageProjection : BoomScenarioPackageProjection_scenario -> Value
encodeScenarioPackageProjection (BoomScenarioPackageProjection_scenario items) =
    items |> List.map encodeScenarioItem |> list


encodeScenarioPackageModel : BoomScenarioPackageModel_scenario -> Value
encodeScenarioPackageModel (BoomScenarioPackageModel_scenario { scenario_code }) =
    object [ ( "scenario_code", scenario_code |> Maybe.map string |> Maybe.withDefault null ) ]


encodeScenarioItem : BoomScenarioPackageProjection_scenarioItem -> Value
encodeScenarioItem (BoomScenarioPackageProjection_scenarioItem { scenario_code }) =
    object [ ( "scenario_code", scenario_code |> Maybe.map string |> Maybe.withDefault null ) ]


encodeBoomSDMs : BoomSDMs -> Value
encodeBoomSDMs (BoomSDMs { hull_region_intersect_mask, algorithm }) =
    [ Just ( "algorithm", algorithm |> encodeBoomSDMsAlgorithm )
    , hull_region_intersect_mask |> Maybe.map (encodeBoomSDMsHull >> (,) "hull_region_intersect_mask")
    ]
        |> List.concatMap Maybe.toList
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
