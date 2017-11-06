{-
Copyright (C) 2017, University of Kansas Center for Research

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
module TreeZipper exposing (Position(..), TreeZipper, start, moveToward, getTree, getData, getPosition)

import DecodeTree exposing (Tree(..), TreeData)


type Position
    = Root
    | LeftBranch
    | RightBranch


type Context
    = Top
    | Left TreeData Context Tree
    | Right TreeData Tree Context


type TreeZipper
    = TreeZipper Tree Context


start : Tree -> TreeZipper
start tree =
    TreeZipper tree Top


moveToward : Position -> TreeZipper -> TreeZipper
moveToward dir zipper =
    case dir of
        Root ->
            up zipper

        LeftBranch ->
            left zipper

        RightBranch ->
            right zipper


left : TreeZipper -> TreeZipper
left ((TreeZipper tree context) as zipper) =
    case tree of
        Node data left right ->
            TreeZipper left (Left data context right)

        _ ->
            zipper


right : TreeZipper -> TreeZipper
right ((TreeZipper tree context) as zipper) =
    case tree of
        Node data left right ->
            TreeZipper right (Right data left context)

        _ ->
            zipper


up : TreeZipper -> TreeZipper
up ((TreeZipper tree context) as zipper) =
    case context of
        Top ->
            zipper

        Left data upContext right ->
            TreeZipper (Node data tree right) upContext

        Right data left upContext ->
            TreeZipper (Node data left tree) upContext


getTree : TreeZipper -> Tree
getTree (TreeZipper tree context) =
    tree


getData : TreeZipper -> TreeData
getData zipper =
    case getTree zipper of
        Leaf data ->
            data

        Node data _ _ ->
            data


getPosition : TreeZipper -> Position
getPosition (TreeZipper _ context) =
    case context of
        Top ->
            Root

        Left _ _ _ ->
            LeftBranch

        Right _ _ _ ->
            RightBranch
