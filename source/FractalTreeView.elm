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


module FractalTreeView exposing (view)

import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import DecodeTree exposing (Tree(..), TreeData)
import TreeZipper exposing (TreeZipper, Position(..), moveToward, getTree, getData, getPosition)
import Animation as A exposing (Animation)
import McpaModel exposing (..)


type Transform
    = Translate Float
    | Scale Float
    | Rotate Float


transformToAttr : Transform -> String
transformToAttr t =
    case t of
        Translate x ->
            "translate(" ++ (toString x) ++ ", 0)"

        Scale x ->
            "scale(" ++ (toString x) ++ ")"

        Rotate x ->
            "rotate(" ++ (toString x) ++ ")"


view : Model -> Html.Html Msg
view { zipper, animationState, mouseIn } =
    let
        cbx =
            if mouseIn then
                clickBoxes
            else
                []

        ( transforms, tree, mapClade ) =
            case animationState of
                Static ->
                    ( [], zipper |> getTree, getData zipper |> .cladeId )

                Start _ ->
                    ( [], zipper |> getTree, getData zipper |> .cladeId )

                Running AnimateLeft time animation ->
                    let
                        transforms =
                            [ Rotate (animation |> A.from 0 |> A.to 90 |> A.animate time)
                            , Scale (1.0 / (animation |> A.from 1 |> A.to 0.7 |> A.animate time))
                            , Translate (animation |> A.from 0 |> A.to 0.25 |> A.animate time)
                            ]
                    in
                        ( transforms, zipper |> getTree, getData zipper |> .cladeId )

                Running AnimateRight time animation ->
                    let
                        transforms =
                            [ Rotate (animation |> A.from 0 |> A.to -90 |> A.animate time)
                            , Scale (1.0 / (animation |> A.from 1 |> A.to 0.7 |> A.animate time))
                            , Translate (animation |> A.from 0 |> A.to -0.25 |> A.animate time)
                            ]
                    in
                        ( transforms, zipper |> getTree, getData zipper |> .cladeId )

                Running AnimateUpLeft time animation ->
                    let
                        transforms =
                            [ Rotate (animation |> A.from 90 |> A.to 0 |> A.animate time)
                            , Scale (1.0 / (animation |> A.from 0.7 |> A.to 1 |> A.animate time))
                            , Translate (animation |> A.from 0.25 |> A.to 0 |> A.animate time)
                            ]
                    in
                        ( transforms, moveToward Root zipper |> getTree, getData zipper |> .cladeId )

                Running AnimateUpRight time animation ->
                    let
                        transforms =
                            [ Rotate (animation |> A.from -90 |> A.to 0 |> A.animate time)
                            , Scale (1.0 / (animation |> A.from 0.7 |> A.to 1 |> A.animate time))
                            , Translate (animation |> A.from -0.25 |> A.to 0 |> A.animate time)
                            ]
                    in
                        ( transforms, moveToward Root zipper |> getTree, getData zipper |> .cladeId )
    in
        Html.div
            [ Html.Events.on "keyup" (Decode.map (KeyUp >> AnimationMsg) <| Decode.field "key" Decode.string)
            , Html.Attributes.style [ ( "display", "flex" ), ( "flex-direction", "flex-row" ) ]
            , Html.Attributes.tabindex 0
            ]
            [ svg
                [ width "800"
                , height "800"
                , viewBox "-0.5 -0.5 1 1"
                , Html.Attributes.style [ ( "background", "lightblue" ), ( "font-family", "sans-serif" ) ]
                , Html.Events.onMouseEnter (SetMouseIn True)
                , Html.Events.onMouseLeave (SetMouseIn False)
                ]
                (drawTree (transforms |> List.map transformToAttr |> String.join " ") tree :: cbx)
            , Html.div
                [ Html.Attributes.class "leaflet-map"
                , Html.Attributes.attribute "data-map-column" (mapClade |> toString)
                , Html.Attributes.style [ ( "width", "800px" ), ( "height", "800px" ) ]
                ]
                []
            ]


drawTree : String -> Tree -> Svg msg
drawTree trans tree =
    g [ transform trans ]
        (line [ x1 "0", x2 "0", y1 "0.5", y2 "0", stroke "darkolivegreen", strokeWidth "0.014" ] []
            :: drawTree_ 0 0 tree
        )


drawTree_ : Int -> Int -> Tree -> List (Svg msg)
drawTree_ depth quadrant tree =
    let
        textTransform =
            if quadrant < 2 then
                "rotate(90)"
            else
                "rotate(-90)"

        descend depth =
            case tree of
                Leaf data ->
                    [ ellipse [ cx "0", cy "0", rx "0.2", ry "0.3", fill "green", stroke "lightgreen", strokeWidth "0.005" ]
                        []
                    , text_ [ x "0", y "0", textAnchor "middle", fontSize "0.05", transform textTransform, fill "white" ]
                        [ text data.name ]
                    ]

                Node data left right ->
                    [ line [ x1 "-0.25", x2 "0.25", y1 "0", y2 "0", stroke "darkolivegreen", strokeWidth "0.01" ] []
                    , g [ transform "translate(-0.25, 0) scale(0.7) rotate(-90)" ]
                        (drawTree_ depth ((quadrant + 1) % 4) left)
                    , g [ transform "translate(0.25, 0) scale(0.7) rotate(90)" ]
                        (drawTree_ depth ((quadrant - 1) % 4) right)
                    ]
    in
        if depth < 11 then
            descend (depth + 1)
        else
            []


clickBoxes : List (Html.Html Msg)
clickBoxes =
    [ rect
        [ x "-0.5"
        , y "-0.5"
        , width "0.5"
        , height "0.9"
        , fill "blue"
        , fillOpacity "0.2"
        , strokeWidth "0.001"
        , stroke "grey"
        , Html.Events.onClick (AnimationMsg <| KeyUp "ArrowLeft")
        ]
        []
    , rect
        [ x "0"
        , y "-0.5"
        , width "0.5"
        , height "0.9"
        , fill "red"
        , fillOpacity "0.2"
        , strokeWidth "0.001"
        , stroke "grey"
        , Html.Events.onClick (AnimationMsg <| KeyUp "ArrowRight")
        ]
        []
    , rect
        [ x "-0.5"
        , y "0.4"
        , width "1"
        , height "0.1"
        , fill "purple"
        , fillOpacity "0.2"
        , strokeWidth "0.001"
        , stroke "grey"
        , Html.Events.onClick (AnimationMsg <| KeyUp "ArrowDown")
        ]
        []
    ]
