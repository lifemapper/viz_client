module Helpers exposing (..)

import Array


type alias Index =
    List Int


undefined : () -> a
undefined _ =
    Debug.crash "Undefined"


unsafeFromMaybe : String -> Maybe a -> a
unsafeFromMaybe error maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.crash error


chain : (model -> ( model, Cmd msg )) -> (model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
chain first second model =
    let
        ( model1, cmd1 ) =
            first model

        ( model2, cmd2 ) =
            second model1
    in
        model2 ! [ cmd1, cmd2 ]


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
