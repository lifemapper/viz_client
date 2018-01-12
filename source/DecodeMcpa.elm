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


module DecodeMcpa exposing (McpaData, decodeMcpa)

import Dict exposing (Dict)
import List.Extra as List
import Result.Extra as Result
import Json.Decode as Decode exposing (Decoder)
import McpaTypes exposing (..)


type alias VarName =
    String


type alias VarType =
    String


type alias McpaData =
    Dict ( Int, VarType, VarName ) Float


type alias RawData =
    { headers : Dict String (List String)
    , data : List (List (List Float))
    }


decodeMcpa : Decoder McpaData
decodeMcpa =
    decodeRawData |> Decode.andThen rawDataToMcpa


decodeRawData : Decoder RawData
decodeRawData =
    Decode.map2 RawData
        (Decode.field "headers" decodeHeaders)
        (Decode.field "data" decodeData)


decodeHeaders : Decoder (Dict String (List String))
decodeHeaders =
    Decode.dict (Decode.list Decode.string)


decodeData : Decoder (List (List (List Float)))
decodeData =
    Decode.list (Decode.list (Decode.list Decode.float))


rawDataToMcpa : RawData -> Decoder McpaData
rawDataToMcpa { headers, data } =
    let
        cladeIds =
            headers
                |> Dict.get "0"
                |> Result.fromMaybe "missing headers[0] in mcpa json"
                |> Result.andThen (List.map String.toInt >> Result.combine)

        variables =
            headers |> Dict.get "1" |> Result.fromMaybe "missing headers[1] in mcpa json"

        valueTypes =
            headers |> Dict.get "2" |> Result.fromMaybe "missing headers[2] in mcpa json"
    in
        case Result.map4 groupMcpaData cladeIds variables valueTypes (Ok data) of
            Ok mcpaData ->
                Decode.succeed mcpaData

            Err err ->
                Decode.fail err


groupMcpaData : List Int -> List VarName -> List VarType -> List (List (List Float)) -> McpaData
groupMcpaData cladeIds variables valueTypes data =
    List.zip cladeIds data |> List.foldl (groupMcpaDataVars variables valueTypes) Dict.empty


groupMcpaDataVars : List VarName -> List VarType -> ( Int, List (List Float) ) -> McpaData -> McpaData
groupMcpaDataVars variables valueTypes ( cladeId, data ) dict =
    List.zip variables data |> List.foldl (groupMcpaDataTypes cladeId valueTypes) dict


groupMcpaDataTypes : Int -> List VarType -> ( VarName, List Float ) -> McpaData -> McpaData
groupMcpaDataTypes cladeId valueTypes ( varName, data ) dict =
    List.zip valueTypes data |> List.foldl (groupMcpaDataBase cladeId varName) dict


groupMcpaDataBase : Int -> VarName -> ( VarType, Float ) -> McpaData -> McpaData
groupMcpaDataBase cladeId varName ( varType, value ) dict =
    dict |> Dict.insert ( cladeId, varType, varName ) value
