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


module ParseMcpa exposing (parseMcpa, McpaData)

import Csv
import Dict
import Regex exposing (..)
import Result.Extra as Result
import Maybe.Extra as Maybe


nodeIdRegex : Regex
nodeIdRegex =
    regex "Node_(\\d+)" |> caseInsensitive


nan : Float
nan =
    (0 / 0)


type alias McpaData =
    Dict.Dict ( Int, String, String ) Float


parseCsv : Csv.Csv -> Result String ( List String, McpaData )
parseCsv { headers, records } =
    let
        variables =
            headers |> List.drop 2

        data =
            List.foldl (\record -> Result.andThen (parseRecord variables record)) (Ok Dict.empty) records
    in
        data |> Result.map ((,) variables)


parseMcpa : String -> Result String ( List String, McpaData )
parseMcpa =
    Csv.parse >> Result.mapError toString >> Result.andThen parseCsv


parseRecord : List String -> List String -> McpaData -> Result String McpaData
parseRecord variables record result =
    case record of
        cladeIdStr :: valueType :: valueStrs ->
            let
                cladeId =
                    case Debug.log "matches" <| find (AtMost 1) nodeIdRegex cladeIdStr of
                        [] ->
                            String.toInt cladeIdStr

                        { submatches } :: _ ->
                            submatches
                                |> List.head
                                |> Maybe.join
                                |> Maybe.map String.toInt
                                |> Maybe.withDefault
                                    (Err "missing node_id")

                valueToFloat s =
                    case s of
                        "nan" ->
                            Ok nan

                        _ ->
                            String.toFloat s

                values =
                    valueStrs |> List.map valueToFloat |> Result.combine

                makeDict cladeId values =
                    List.map2 (,) variables values
                        |> (result |> List.foldl (\( var, val ) -> Dict.insert ( cladeId, valueType, var ) val))
            in
                Result.map2 makeDict cladeId values

        _ ->
            Err "bad csv"
