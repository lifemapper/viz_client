module TestNewick2Binary exposing (..)

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
        ]
