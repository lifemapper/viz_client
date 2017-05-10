module Encoder exposing (..)

import Decoder exposing (..)
import Json.Encode exposing (..)


encodeAlgorithm : Algorithm -> Value
encodeAlgorithm (Algorithm { code, parameters }) =
    object [ ( "code", string code ), ( "parameters", encodeAlgorithmParameters parameters ) ]


encodeAlgorithmParameters : AlgorithmParameters -> Value
encodeAlgorithmParameters (AlgorithmParameters items) =
    list <| List.map encodeAlgorithmParametersItem items


encodeAlgorithmParametersItem : AlgorithmParametersItem -> Value
encodeAlgorithmParametersItem (AlgorithmParametersItem { name, value }) =
    object [ ( "name", string name ), ( "value", string value ) ]
