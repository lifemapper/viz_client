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
module DecodeTree exposing (..)

import Json.Decode exposing (Decoder, int, string, float, maybe, list, lazy, field, succeed, fail)


type alias TreeData =
    { cladeId : Int
    , length : Maybe Float
    , name : String
    , squid : Maybe String
    }


type alias TreeRecord =
    { data : TreeData
    , children : List Tree
    }


type Tree
    = Node TreeData Tree Tree
    | Leaf TreeData


removeUnderscore : Decoder (Maybe String) -> Decoder String
removeUnderscore =
    Json.Decode.map
        (Maybe.withDefault ""
            >> String.map
                (\c ->
                    if c == '_' then
                        ' '
                    else
                        c
                )
        )


treeDecoder : Decoder Tree
treeDecoder =
    Json.Decode.map2 TreeRecord
        (Json.Decode.map4 TreeData
            (field "cladeId" int)
            (maybe <| field "length" float)
            (removeUnderscore <| maybe <| field "name" string)
            (maybe <| field "squid" string)
        )
        (field "children" (lazy (\_ -> (list treeDecoder))))
        |> Json.Decode.andThen checkBinary


checkBinary : TreeRecord -> Decoder Tree
checkBinary { data, children } =
    case children of
        [] ->
            succeed (Leaf data)

        [ _ ] ->
            fail "found node with single child."

        [ left, right ] ->
            succeed (Node data left right)

        _ ->
            fail "found node with more than two children."
