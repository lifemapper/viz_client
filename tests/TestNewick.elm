module TestNewick exposing (..)

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
import Combine
import ExampleTrees


fuzzName : Fuzzer Newick.Name
fuzzName =
    [ "a", "b", "c", "_" ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
        |> Fuzz.list
        |> Fuzz.map (String.join "")


fuzzLength : Fuzzer Newick.Length
fuzzLength =
    Fuzz.maybe Fuzz.float


fuzzTree : Fuzzer Newick.Tree
fuzzTree =
    Fuzz.oneOf [ fuzzSubtree 0 |> Fuzz.map SubTree, fuzzBranch 0 |> Fuzz.map Branch ]


fuzzBranch : Int -> Fuzzer Newick.Branch
fuzzBranch i =
    Fuzz.tuple ( fuzzSubtree (i + 1), fuzzLength )


fuzzSubtree : Int -> Fuzzer Newick.SubTree
fuzzSubtree i =
    if i > 2 then
        Fuzz.map Leaf fuzzName
    else
        let
            branches =
                Fuzz.list (fuzzBranch i)

            atLeastOne =
                Fuzz.map2 (::) (fuzzBranch i) branches

            atLeastTwo =
                Fuzz.map2 (::) (fuzzBranch i) atLeastOne
        in
            Fuzz.oneOf
                [ Fuzz.map Leaf fuzzName
                , Fuzz.map2 Branches atLeastTwo fuzzName
                ]


tryParse : Combine.Parser () r -> String -> Result String r
tryParse parser input =
    case Combine.parse parser input of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (input ++ " " ++ String.join " or " errors)


suite : Test
suite =
    describe "The Newick module"
        [ describe "Example trees from Wikipedia"
            [ test "(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F; distances and all names" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (SubTree
                                (Branches
                                    ([ ( Leaf "A", (Just 0.1) )
                                     , ( Leaf "B", (Just 0.2) )
                                     , ( Branches ([ ( Leaf "C", (Just 0.3) ), ( Leaf "D", (Just 0.4) ) ]) "E", (Just 0.5) )
                                     ]
                                    )
                                    "F"
                                )
                            )
                        )
                        (tryParse Newick.tree "(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;")
            , test "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5); distances and leaf names (popular)" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (SubTree
                                (Branches
                                    ([ ( Leaf "A", (Just 0.1) )
                                     , ( Leaf "B", (Just 0.2) )
                                     , ( Branches ([ ( Leaf "C", (Just 0.3) ), ( Leaf "D", (Just 0.4) ) ]) "", (Just 0.5) )
                                     ]
                                    )
                                    ""
                                )
                            )
                        )
                        (tryParse Newick.tree "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);")
            , test "(:0.1,:0.2,(:0.3,:0.4):0.5):0.0; all have a distance to parent" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (Branch
                                ( Branches
                                    ([ ( Leaf "", (Just 0.1) )
                                     , ( Leaf "", (Just 0.2) )
                                     , ( Branches ([ ( Leaf "", (Just 0.3) ), ( Leaf "", (Just 0.4) ) ]) "", (Just 0.5) )
                                     ]
                                    )
                                    ""
                                , Just 0
                                )
                            )
                        )
                        (tryParse Newick.tree "(:0.1,:0.2,(:0.3,:0.4):0.5):0.0;")
            , test "(:0.1,:0.2,(:0.3,:0.4):0.5); all but root node have a distance to parent" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (SubTree
                                (Branches
                                    ([ ( Leaf "", (Just 0.1) )
                                     , ( Leaf "", (Just 0.2) )
                                     , ( Branches ([ ( Leaf "", (Just 0.3) ), ( Leaf "", (Just 0.4) ) ]) "", (Just 0.5) )
                                     ]
                                    )
                                    ""
                                )
                            )
                        )
                        (tryParse Newick.tree "(:0.1,:0.2,(:0.3,:0.4):0.5);")
            , test "(A,B,(C,D)E)F; all nodes are named" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (SubTree
                                (Branches
                                    ([ ( Leaf "A", Nothing )
                                     , ( Leaf "B", Nothing )
                                     , ( Branches ([ ( Leaf "C", Nothing ), ( Leaf "D", Nothing ) ]) "E", Nothing )
                                     ]
                                    )
                                    "F"
                                )
                            )
                        )
                        (tryParse Newick.tree "(A,B,(C,D)E)F;")
            , test "(A,B,(C,D)); leaf nodes are named" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (SubTree
                                (Branches
                                    ([ ( Leaf "A", Nothing )
                                     , ( Leaf "B", Nothing )
                                     , ( Branches ([ ( Leaf "C", Nothing ), ( Leaf "D", Nothing ) ]) "", Nothing )
                                     ]
                                    )
                                    ""
                                )
                            )
                        )
                        (tryParse Newick.tree "(A,B,(C,D));")
            , test "(,,(,)); no nodes are named" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (SubTree
                                (Branches
                                    ([ ( Leaf "", Nothing )
                                     , ( Leaf "", Nothing )
                                     , ( Branches ([ ( Leaf "", Nothing ), ( Leaf "", Nothing ) ]) "", Nothing )
                                     ]
                                    )
                                    ""
                                )
                            )
                        )
                        (tryParse Newick.tree "(,,(,));")
            , test "((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A; a tree rooted on a leaf node (rare)" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (SubTree
                                (Branches
                                    ([ ( Branches
                                            ([ ( Leaf "B", Just 0.2 )
                                             , ( Branches ([ ( Leaf "C", Just 0.3 ), ( Leaf "D", Just 0.4 ) ]) "E", Just 0.5 )
                                             ]
                                            )
                                            "F"
                                       , Just 0.1
                                       )
                                     ]
                                    )
                                    "A"
                                )
                            )
                        )
                        (tryParse Newick.tree "((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A;")
            ]
        , describe "Heuchera tree"
            [ test "Parsing Huechera tree" <|
                \_ ->
                    Expect.equal
                        (Ok ExampleTrees.heucheraNewick)
                        (tryParse Newick.tree ExampleTrees.heucheraString)
            , test "Unparse Heuchera tree" <|
                \_ ->
                    Expect.equal
                        ExampleTrees.heucheraString
                        (Newick.unparse ExampleTrees.heucheraNewick)
            ]
        , describe "Heuchera tree 2"
            [ test "Parsing Huechera tree" <|
                \_ ->
                    Expect.equal
                        (Ok ExampleTrees.heucheraNewick2)
                        (tryParse Newick.tree ExampleTrees.heucheraString2)
            , test "Unparse Heuchera tree" <|
                \_ ->
                    Expect.equal
                        ExampleTrees.heucheraString2
                        (Newick.unparse ExampleTrees.heucheraNewick2)
            ]
          -- , describe "Fuzzing tests"
          --     -- Skipping these because there is ambiguity in the Newick grammar
          --     -- that breaks the tests.
          --     [ skip <| fuzz fuzzTree "Fuzz tree parsing" <|
          --         \tree -> Expect.equal (Ok tree) (tryParse Newick.tree (Newick.unparse tree))
          --     ]
        ]
