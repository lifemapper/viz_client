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

import ParseMcpa exposing (McpaData, parseMcpa)
import ParseAncState exposing (AncStateData, parseAncState)
import DecodeTree exposing (Tree)
import ParseNexusTree exposing (parseNexusTree)
import TreeMetrics exposing (..)


type alias Flags =
    { mcpaMatrix : String
    , taxonTree : String
    , ancState : String
    }


type alias TreeInfo =
    { root : Tree
    , length : Float
    , depth : Int
    , breadth : Int
    }


type alias Model =
    { treeInfo : TreeInfo
    , mcpaVariables : List String
    , ancStateVars : List String
    , ancState : AncStateData
    , mcpaData : McpaData
    , selectedVariable : String
    , selectedNode : Maybe Int
    , showBranchLengths : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( variables, data ) =
            case parseMcpa flags.mcpaMatrix of
                Ok result ->
                    result

                Err err ->
                    Debug.crash ("failed to decode MCPA matrix: " ++ err)

        ( ancStateVars, ancState ) =
            case parseAncState flags.ancState of
                Ok result ->
                    result

                Err err ->
                    Debug.crash ("failed to decode ancState matrix: " ++ err)

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
          , mcpaVariables = variables
          , ancStateVars = ancStateVars
          , selectedVariable = List.head variables |> Maybe.withDefault ""
          , selectedNode = Nothing
          , showBranchLengths = False
          , mcpaData = data
          , ancState = ancState
          }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = SelectVariable String
    | ToggleShowLengths
    | SelectNode Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectVariable v ->
            ( { model | selectedVariable = v }, Cmd.none )

        ToggleShowLengths ->
            ( { model | showBranchLengths = not model.showBranchLengths }, Cmd.none )

        SelectNode cladeId ->
            ( { model | selectedNode = Just cladeId }, Cmd.none )
