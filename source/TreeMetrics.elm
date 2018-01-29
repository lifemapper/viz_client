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


module TreeMetrics exposing (treeLength, treeDepth, treeBreadth)

import DecodeTree exposing (Tree(..), TreeData)


treeLength : Tree -> Float
treeLength tree =
    case tree of
        Leaf { length } ->
            length |> Maybe.withDefault 0

        Node { length } left right ->
            let
                thisLength =
                    length |> Maybe.withDefault 0
            in
                thisLength + Basics.max (treeLength left) (treeLength right)


treeDepth : Tree -> Int
treeDepth tree =
    case tree of
        Leaf _ ->
            1

        Node _ left right ->
            1 + Basics.max (treeDepth left) (treeDepth right)


treeBreadth : Tree -> Int
treeBreadth tree =
    case tree of
        Leaf _ ->
            1

        Node _ left right ->
            (treeBreadth left) + (treeBreadth right)