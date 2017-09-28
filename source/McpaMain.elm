module McpaMain exposing (..)

import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import ExampleTree
import DecodeTree exposing (Tree(..), TreeData)
import Time exposing (Time)
import Animation as A exposing (Animation)
import AnimationFrame
import Ease


type Context
    = Top
    | Left TreeData Context Tree
    | Right TreeData Tree Context


type alias TreeZipper =
    ( Tree, Context )


top : Tree -> TreeZipper
top tree =
    ( tree, Top )


left : TreeZipper -> TreeZipper
left ( tree, context ) =
    case tree of
        Node data left right ->
            ( left, Left data context right )

        _ ->
            ( tree, context )


right : TreeZipper -> TreeZipper
right ( tree, context ) =
    case tree of
        Node data left right ->
            ( right, Right data left context )

        _ ->
            ( tree, context )


up : TreeZipper -> TreeZipper
up ( tree, context ) =
    case context of
        Top ->
            ( tree, context )

        Left data upContext right ->
            ( Node data tree right, upContext )

        Right data left upContext ->
            ( Node data left tree, upContext )


getTree : TreeZipper -> Tree
getTree ( tree, context ) =
    tree


type AnimationDirection
    = AnimateUpLeft
    | AnimateUpRight
    | AnimateLeft
    | AnimateRight


type Model
    = Static TreeZipper
    | StartAnimation AnimationDirection TreeZipper
    | Animating AnimationDirection Time Animation TreeZipper


type Msg
    = KeyUp String
    | CurrentTick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Static zipper ->
            case msg of
                KeyUp "ArrowDown" ->
                    case zipper of
                        ( _, Left _ _ _ ) ->
                            ( StartAnimation AnimateUpLeft zipper, Cmd.none )

                        ( _, Right _ _ _ ) ->
                            ( StartAnimation AnimateUpRight zipper, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                KeyUp "ArrowLeft" ->
                    ( StartAnimation AnimateLeft zipper, Cmd.none )

                KeyUp "ArrowRight" ->
                    ( StartAnimation AnimateRight zipper, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartAnimation dir zipper ->
            case msg of
                CurrentTick time ->
                    let
                        animation =
                            A.animation time
                                |> A.duration (0.5 * Time.second)
                                |> A.ease Ease.inOutCirc
                    in
                        ( Animating dir time animation zipper, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Animating dir _ animation zipper ->
            case msg of
                CurrentTick time ->
                    if A.isRunning time animation then
                        ( Animating dir time animation zipper, Cmd.none )
                    else
                        case dir of
                            AnimateUpLeft ->
                                ( Static (up zipper), Cmd.none )

                            AnimateUpRight ->
                                ( Static (up zipper), Cmd.none )

                            AnimateLeft ->
                                ( Static (left zipper), Cmd.none )

                            AnimateRight ->
                                ( Static (right zipper), Cmd.none )

                _ ->
                    ( model, Cmd.none )


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
view model =
    let
        ( transforms, zipper ) =
            case model of
                Static zipper ->
                    ( [], zipper )

                StartAnimation _ zipper ->
                    ( [], zipper )

                Animating AnimateLeft time animation zipper ->
                    let
                        transforms =
                            [ Rotate (animation |> A.from 0 |> A.to 90 |> A.animate time)
                            , Scale (1.0 / (animation |> A.from 1 |> A.to 0.7 |> A.animate time))
                            , Translate (animation |> A.from 0 |> A.to 0.25 |> A.animate time)
                            ]
                    in
                        ( transforms, zipper )

                Animating AnimateRight time animation zipper ->
                    let
                        transforms =
                            [ Rotate (animation |> A.from 0 |> A.to -90 |> A.animate time)
                            , Scale (1.0 / (animation |> A.from 1 |> A.to 0.7 |> A.animate time))
                            , Translate (animation |> A.from 0 |> A.to -0.25 |> A.animate time)
                            ]
                    in
                        ( transforms, zipper )

                Animating AnimateUpLeft time animation zipper ->
                    let
                        transforms =
                            [ Rotate (animation |> A.from 90 |> A.to 0 |> A.animate time)
                            , Scale (1.0 / (animation |> A.from 0.7 |> A.to 1 |> A.animate time))
                            , Translate (animation |> A.from 0.25 |> A.to 0 |> A.animate time)
                            ]
                    in
                        ( transforms, up zipper )

                Animating AnimateUpRight time animation zipper ->
                    let
                        transforms =
                            [ Rotate (animation |> A.from -90 |> A.to 0 |> A.animate time)
                            , Scale (1.0 / (animation |> A.from 0.7 |> A.to 1 |> A.animate time))
                            , Translate (animation |> A.from -0.25 |> A.to 0 |> A.animate time)
                            ]
                    in
                        ( transforms, up zipper )
    in
        Html.div
            [ Html.Events.on "keyup" (Decode.map KeyUp <| Decode.field "key" Decode.string)
            , Html.Attributes.tabindex 0
            ]
            [ svg
                [ width "800"
                , height "800"
                , viewBox "-0.5 -0.5 1 1"
                , Html.Attributes.style [ ( "background", "lightblue" ), ( "font-family", "sans-serif" ) ]
                ]
                [ drawTree (transforms |> List.map transformToAttr |> String.join " ") (getTree zipper) ]
            ]


drawTree : String -> Tree -> Svg msg
drawTree trans tree =
    g [ transform trans ]
        (line [ x1 "0", x2 "0", y1 "0.5", y2 "0", stroke "darkolivegreen", strokeWidth "0.014" ] []
            :: drawTree_ 0 tree
        )


drawTree_ : Int -> Tree -> List (Svg msg)
drawTree_ depth tree =
    let
        descend depth =
            case tree of
                Leaf data ->
                    [ ellipse [ cx "0", cy "0", rx "0.2", ry "0.3", fill "green", stroke "lightgreen", strokeWidth "0.005" ]
                        []
                    , text_ [ x "0", y "0", textAnchor "middle", fontSize "0.05", transform "rotate(-90)", fill "white" ]
                        [ text data.name ]
                    ]

                Node data left right ->
                    [ line [ x1 "-0.25", x2 "0.25", y1 "0", y2 "0", stroke "darkolivegreen", strokeWidth "0.01" ] []
                    , g [ transform "translate(-0.25, 0) scale(0.7) rotate(-90)" ]
                        (drawTree_ depth left)
                    , g [ transform "translate(0.25, 0) scale(0.7) rotate(90)" ]
                        (drawTree_ depth right)
                    ]
    in
        if depth < 10 then
            descend (depth + 1)
        else
            []


main : Program Never Model Msg
main =
    Html.program
        { init = ( Static <| top ExampleTree.tree, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always <| AnimationFrame.times CurrentTick
        }
