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
import DecodeTree exposing (Tree)
import ParseNexusTree exposing (parseNexusTree)
import TreeZipper exposing (TreeZipper, Position(..), moveToward, getTree, getData, getPosition)
import TreeMetrics exposing (..)
import Animation as A exposing (Animation)
import AnimationFrame
import Time exposing (Time)
import Ease


type alias Flags =
    { mcpaMatrix : String
    , taxonTree : String
    }


type alias TreeInfo =
    { root : Tree
    , length : Float
    , depth : Int
    , breadth : Int
    }


type alias Model =
    { zipper : TreeZipper
    , treeInfo : TreeInfo
    , mcpaVariables : List String
    , mcpaData : McpaData
    , selectedVariable : String
    , animationState : AnimationState
    , mouseIn : Bool
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
        ( { animationState = Static
          , zipper = TreeZipper.start root
          , treeInfo = treeInfo
          , mcpaVariables = variables
          , selectedVariable = "ECO_NUM - 7"
          , mcpaData = data
          , mouseIn = False
          }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.animationState of
        Start _ ->
            AnimationFrame.times (AnimationMsg << CurrentTick)

        Running _ _ _ ->
            AnimationFrame.times (AnimationMsg << CurrentTick)

        _ ->
            Sub.none


type AnimationState
    = Start AnimationDirection
    | Running AnimationDirection Time Animation
    | Static


type AnimationDirection
    = AnimateUpLeft
    | AnimateUpRight
    | AnimateLeft
    | AnimateRight


type Msg
    = SetMouseIn Bool
    | JumpUp
    | JumpLeft
    | JumpRight
    | SelectVariable String
    | AnimationMsg AnimationMsg


type AnimationMsg
    = KeyUp String
    | CurrentTick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMouseIn mouseIn ->
            ( { model | mouseIn = mouseIn }, Cmd.none )

        JumpUp ->
            ( { model | zipper = moveToward Root model.zipper }, Cmd.none )

        JumpLeft ->
            ( { model | zipper = moveToward LeftBranch model.zipper }, Cmd.none )

        JumpRight ->
            ( { model | zipper = moveToward RightBranch model.zipper }, Cmd.none )

        SelectVariable v ->
            ( { model | selectedVariable = v }, Cmd.none )

        AnimationMsg msg_ ->
            updateAnimation msg_ model


updateAnimation : AnimationMsg -> Model -> ( Model, Cmd Msg )
updateAnimation msg ({ zipper, animationState } as model) =
    case animationState of
        Static ->
            case msg of
                KeyUp "ArrowDown" ->
                    case getPosition zipper of
                        LeftBranch ->
                            ( { model | animationState = Start AnimateUpLeft }, Cmd.none )

                        RightBranch ->
                            ( { model | animationState = Start AnimateUpRight }, Cmd.none )

                        Root ->
                            ( model, Cmd.none )

                KeyUp "ArrowLeft" ->
                    if zipper /= moveToward LeftBranch zipper then
                        ( { model | animationState = Start AnimateLeft }, Cmd.none )
                    else
                        ( model, Cmd.none )

                KeyUp "ArrowRight" ->
                    if zipper /= moveToward RightBranch zipper then
                        ( { model | animationState = Start AnimateRight }, Cmd.none )
                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Start dir ->
            case msg of
                CurrentTick time ->
                    let
                        animation =
                            A.animation time
                                |> A.duration (0.5 * Time.second)
                                |> A.ease Ease.inOutCirc
                    in
                        ( { model | animationState = Running dir time animation }, Cmd.none )

                KeyUp _ ->
                    ( model, Cmd.none )

        Running dir _ animation ->
            case msg of
                CurrentTick time ->
                    if A.isRunning time animation then
                        ( { model | animationState = Running dir time animation }, Cmd.none )
                    else
                        case dir of
                            AnimateUpLeft ->
                                ( { model | animationState = Static, zipper = moveToward Root zipper }, Cmd.none )

                            AnimateUpRight ->
                                ( { model | animationState = Static, zipper = moveToward Root zipper }, Cmd.none )

                            AnimateLeft ->
                                ( { model | animationState = Static, zipper = moveToward LeftBranch zipper }, Cmd.none )

                            AnimateRight ->
                                ( { model | animationState = Static, zipper = moveToward RightBranch zipper }, Cmd.none )

                KeyUp _ ->
                    ( model, Cmd.none )
