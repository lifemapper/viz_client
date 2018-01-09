module TestTaxLabels exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import TaxLabels exposing (..)
import Combine
import ExampleTrees exposing (taxLabelsParsed, taxLabelsText)


tryParse : Combine.Parser () r -> String -> Result String r
tryParse parser input =
    case Combine.parse parser input of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (input ++ " " ++ String.join " or " errors)


suite : Test
suite =
    describe "Parsing TAXLABELS statements"
        [ test "heuchera labels" <|
            \_ ->
                Expect.equal (Ok taxLabelsParsed) (tryParse taxLabels taxLabelsText)
        ]
