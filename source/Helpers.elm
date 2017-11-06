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
