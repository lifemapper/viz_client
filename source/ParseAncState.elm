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


module ParseAncState exposing (parseAncState, AncStateData)

import Csv
import Dict
import Result.Extra as Result


nan : Float
nan =
    (0 / 0)


type alias AncStateData =
    { values : Dict.Dict ( Int, String ) Float
    , ranges : Dict.Dict String ( Float, Float )
    }


parseCsv : Csv.Csv -> Result String ( List String, AncStateData )
parseCsv { headers, records } =
    let
        variables =
            headers |> List.drop 1

        empty =
            AncStateData Dict.empty Dict.empty

        data =
            List.foldl (\record -> Result.andThen (parseRecord variables record)) (Ok empty) records
    in
        data |> Result.map ((,) variables)


parseAncState : String -> Result String ( List String, AncStateData )
parseAncState =
    Csv.parse >> Result.mapError toString >> Result.andThen parseCsv


parseRecord : List String -> List String -> AncStateData -> Result String AncStateData
parseRecord variables record result =
    case record of
        cladeIdStr :: valueStrs ->
            let
                cladeId =
                    String.toInt cladeIdStr

                valueToFloat s =
                    case s of
                        "nan" ->
                            Ok nan

                        _ ->
                            String.toFloat s

                values =
                    valueStrs |> List.map valueToFloat |> Result.combine

                insert cladeId values =
                    List.map2 (,) variables values
                        |> insertValues cladeId result
            in
                Result.map2 insert cladeId values

        _ ->
            Err "bad csv"


insertValues : Int -> AncStateData -> List ( String, Float ) -> AncStateData
insertValues cladeId { values, ranges } pairs =
    let
        values_ =
            List.foldl (\( var, val ) -> Dict.insert ( cladeId, var ) val) values pairs

        ranges_ =
            List.foldl updateRanges ranges pairs
    in
        { values = values_
        , ranges = ranges_
        }


updateRanges : ( String, Float ) -> Dict.Dict String ( Float, Float ) -> Dict.Dict String ( Float, Float )
updateRanges ( var, val ) ranges =
    let
        ( min_, max_ ) =
            Dict.get var ranges
                |> Maybe.withDefault ( val, val )
    in
        Dict.insert var ( min val min_, max val max_ ) ranges
