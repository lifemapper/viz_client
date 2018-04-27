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


module Newick exposing (..)

import Combine exposing (..)
import Combine.Num exposing (float, int)


type alias Name =
    String


unparseName : Name -> String
unparseName =
    identity


type alias Length =
    Maybe Float


unparseLength : Length -> String
unparseLength =
    Maybe.map (\l -> ":" ++ (toString l)) >> Maybe.withDefault ""


type Tree
    = SubTree SubTree
    | Branch Branch


unparse : Tree -> String
unparse tree =
    (case tree of
        SubTree st ->
            unparseSubTree st

        Branch b ->
            unparseBranch b
    )
        ++ ";"


type SubTree
    = Leaf Name
    | Branches (List Branch) Name


unparseSubTree : SubTree -> String
unparseSubTree st =
    case st of
        Leaf name ->
            unparseName name

        Branches bs name ->
            "(" ++ (bs |> List.map unparseBranch |> String.join ",") ++ ")" ++ (unparseName name)


type alias Branch =
    ( SubTree, Length )


unparseBranch : Branch -> String
unparseBranch ( st, l ) =
    (unparseSubTree st) ++ (unparseLength l)


tree : Parser s Tree
tree =
    choice
        [ ((lazy subTree) <* string ";") |> map SubTree
        , ((lazy branch) <* string ";") |> map Branch
        ]
        <* end


subTree : () -> Parser s SubTree
subTree () =
    choice
        [ lazy internal
        , leaf
        ]


leaf : Parser s SubTree
leaf =
    name |> map Leaf


name : Parser s Name
name =
    choice
        [ string "'" *> quotedName <* string "'" -- |> map (Debug.log "quotedName")
        , regex "[^\\s;:]*" -- |> map (Debug.log "bare name")
        ]


quotedName : Parser s Name
quotedName =
    String.join "" <$> many (choice [ string "''" $> "'", regex "[^']*" ])


internal : () -> Parser s SubTree
internal () =
    (branchSet |> lazy |> parens) |> andThen (\branches -> name |> map (Branches branches))


branchSet : () -> Parser s (List Branch)
branchSet () =
    sepBy1 (string ",") (lazy branch)


branch : () -> Parser s Branch
branch () =
    (lazy subTree) |> andThen (\st -> length |> map ((,) st))


length : Parser s Length
length =
    maybe (string ":" *> floatWithExp)


floatWithExp : Parser s Float
floatWithExp =
    regex "[-+]?(?:\\d*\\.?\\d+|\\d+\\.?\\d*)(?:[eE][-+]?\\d+)?"
        |> map
            (\str ->
                case String.toFloat str of
                    Ok res ->
                        res

                    Err m ->
                        Debug.crash ("impossible float: " ++ (toString m))
            )
        -- |> map (Debug.log "floatWithExp")
