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


module Newick2Binary exposing (newick2Binary)

import Dict
import DecodeTree as Binary
import List.Extra as List
import Maybe.Extra as Maybe
import Newick exposing (Tree(..), SubTree(..))
import TaxLabels exposing (TaxLabel)
import Regex exposing (..)


type alias Names2Squids =
    Dict.Dict String (Maybe String)


newick2Binary : List TaxLabel -> Newick.Tree -> Binary.Tree
newick2Binary taxlabels tree =
    let
        names2squids =
            taxlabels
                |> List.map (\{ name, squid } -> ( name, squid ))
                |> Dict.fromList
    in
        case tree of
            SubTree st ->
                subTree2Binary names2squids (Just 0) st

            Branch ( st, length ) ->
                subTree2Binary names2squids length st


subTree2Binary : Names2Squids -> Newick.Length -> Newick.SubTree -> Binary.Tree
subTree2Binary names2squids length st =
    case st of
        Leaf name ->
            Binary.Leaf
                { cladeId = name2Id name |> Maybe.withDefault -1
                , length = length
                , name = name
                , squid = Dict.get name names2squids |> Maybe.join
                }

        Branches [ ( left, ll ), ( right, lr ) ] name ->
            Binary.Node
                { cladeId = name2Id name |> Maybe.withDefault -1
                , length = length
                , name = name
                , squid = Dict.get name names2squids |> Maybe.join
                }
                (subTree2Binary names2squids ll left)
                (subTree2Binary names2squids lr right)

        _ ->
            Debug.crash "Found non binary subtree structure" st


nodeIdRegex : Regex
nodeIdRegex =
    regex "Node_(\\d+)"


name2Id : String -> Maybe Int
name2Id name =
    name
        |> find (AtMost 1) nodeIdRegex
        |> List.getAt 0
        |> Maybe.map .submatches
        |> Maybe.andThen (List.getAt 0)
        |> Maybe.join
        |> Maybe.andThen (String.toInt >> Result.toMaybe)



-- binary2Newick : Binary.Tree -> Newick.Tree
-- binary2Newick tree =
--     case tree of
--         Binary.Leaf data ->
--             case data.length of
--                 Nothing ->
--                     SubTree (Leaf data.name)
--                 Just 0 ->
--                     SubTree (Leaf data.name)
--                 length ->
--                     Branch ( Leaf data.name, length )
--         Binary.Node data left right ->
--             case data.length of
--                 Nothing ->
--                     SubTree (node2Branches data.name)
--                 Just 0 ->
--                     SubTree (Leaf data.name)
--                 length ->
--                     Branch ( Leaf data.name, length )
