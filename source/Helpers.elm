module Helpers exposing (..)

import Array

type alias Index =
    List Int


unsafeGet : Int -> Array.Array a -> a
unsafeGet i array =
    case Array.get i array of
        Just value -> value
        Nothing -> Debug.crash "Array out of bounds."
