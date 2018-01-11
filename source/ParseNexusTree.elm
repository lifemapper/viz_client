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


module ParseNexusTree exposing (parseNexusTree)

import DecodeTree as Binary
import Newick
import Newick2Binary exposing (newick2Binary)
import TaxLabels exposing (taxLabels)
import Regex exposing (..)
import Combine exposing (parse)
import List.Extra as List
import Maybe.Extra as Maybe


taxLabelsRegex : Regex
taxLabelsRegex =
    regex "TAXLABELS[^;]*;"


treeRegex : Regex
treeRegex =
    regex "TREE 1 = ([^;]*;)"


tryParse : Combine.Parser () r -> String -> Result String r
tryParse parser input =
    case Combine.parse parser input of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (input ++ " " ++ String.join " or " errors)


parseNexusTree : String -> Result String Binary.Tree
parseNexusTree nexus =
    let
        taxa =
            nexus
                |> find (AtMost 1) taxLabelsRegex
                |> List.getAt 0
                |> Maybe.map .match
                |> Result.fromMaybe "Couldn't find TAXLABELS in Nexus"
                |> Result.andThen (tryParse taxLabels)

        tree =
            nexus
                |> find (AtMost 1) treeRegex
                |> List.getAt 0
                |> Maybe.map .submatches
                |> Maybe.andThen (List.getAt 0)
                |> Maybe.join
                |> Result.fromMaybe "Couldn't find TREE 1 in Nexus"
                |> Result.andThen (tryParse Newick.tree)
    in
        Result.map2 newick2Binary taxa tree
