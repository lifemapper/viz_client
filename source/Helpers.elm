module Helpers exposing (..)

import Array


type alias Index =
    List Int


unsafeGet : Int -> Array.Array a -> a
unsafeGet i array =
    case Array.get i array of
        Just value ->
            value

        Nothing ->
            Debug.crash "Array out of bounds."


removeElem : Int -> Array.Array a -> Array.Array a
removeElem i =
    Array.toIndexedList
        >> List.filterMap
            (\( j, a ) ->
                if j == i then
                    Nothing
                else
                    Just a
            )
        >> Array.fromList
