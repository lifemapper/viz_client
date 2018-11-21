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


module McpaModel exposing (..)

import DecodeTree exposing (Tree)
import ParseNexusTree exposing (parseNexusTree)
import TreeMetrics exposing (..)


type alias Flags =
    { data : String
    , taxonTree : String
    }


type alias TreeInfo =
    { root : Tree
    , length : Float
    , depth : Int
    , breadth : Int
    }


type alias Model data =
    { treeInfo : TreeInfo
    , variables : List String
    , data : data
    , flaggedNodes : ( List Int, List Int )
    , selectedVariable : String
    , selectedNode : Maybe Int
    , showBranchLengths : Bool
    }


init : (String -> ( List String, data )) -> Flags -> ( Model data, Cmd Msg )
init parseData flags =
    let
        ( variables, data ) =
            parseData flags.data

        root =
            case parseNexusTree flags.taxonTree of
                Ok tree ->
                    tree

                Err err ->
                    Debug.crash ("failed parsing Nexus tree data: " ++ err)

        treeInfo =
            { root = root
            , length = treeLength root
            , depth = treeDepth root
            , breadth = treeBreadth root
            }
    in
        ( { treeInfo = treeInfo
          , variables = variables
          , selectedVariable =
                if List.member "Env - Adjusted R-squared" variables then
                    "Env - Adjusted R-squared"
                else if List.member "BG - Adjusted R-squared" variables then
                    "BG - Adjusted R-squared"
                else
                    List.head variables |> Maybe.withDefault ""
          , flaggedNodes = ( [], [] )
          , selectedNode = Nothing
          , showBranchLengths = False
          , data = data
          }
        , Cmd.none
        )


subscriptions : Model data -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = SelectVariable String
    | ToggleShowLengths
    | SelectNode Int


update : Msg -> Model data -> ( Model data, Cmd Msg )
update msg model =
    case msg of
        SelectVariable v ->
            ( { model | selectedVariable = v }, Cmd.none )

        ToggleShowLengths ->
            ( { model | showBranchLengths = not model.showBranchLengths }, Cmd.none )

        SelectNode cladeId ->
            ( { model | selectedNode = Just cladeId }, Cmd.none )
