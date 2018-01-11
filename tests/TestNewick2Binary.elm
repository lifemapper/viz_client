module TestNewick2Binary exposing (..)

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

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Newick exposing (..)
import Newick2Binary exposing (..)
import DecodeTree as Binary
import Combine
import ExampleTrees


suite : Test
suite =
    describe "The Newick2Binary module"
        [ describe "Heuchera tree"
            [ test "newick2Binary" <|
                \_ ->
                    Expect.equal ExampleTrees.heucheraBinary
                        (newick2Binary ExampleTrees.taxLabelsParsed ExampleTrees.heucheraNewick)
            ]
        , describe "Heuchera tree 2"
            [ test "newick2Binary" <|
                \_ ->
                    Expect.equal ExampleTrees.heucheraBinary2
                        (newick2Binary ExampleTrees.taxLabelsParsed ExampleTrees.heucheraNewick2)
            ]
        ]
