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
module McpaModel exposing (..)

import ExampleTree
import DecodeTree exposing (Tree)
import TreeZipper exposing (TreeZipper, Position(..), moveToward, getTree, getData, getPosition)
import Animation as A exposing (Animation)
import AnimationFrame
import Time exposing (Time)
import Ease


type alias Model =
    { zipper : TreeZipper
    , root : Tree
    , animationState : AnimationState
    , mouseIn : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { animationState = Static
      , zipper = TreeZipper.start ExampleTree.tree
      , root = ExampleTree.tree
      , mouseIn = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.animationState of
        Start _ ->
            AnimationFrame.times CurrentTick

        Running _ _ _ ->
            AnimationFrame.times CurrentTick

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
    = KeyUp String
    | CurrentTick Time
    | SetMouseIn Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMouseIn mouseIn ->
            ( { model | mouseIn = mouseIn }, Cmd.none )

        _ ->
            updateAnimation msg model


updateAnimation : Msg -> Model -> ( Model, Cmd Msg )
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

                _ ->
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

                _ ->
                    ( model, Cmd.none )
