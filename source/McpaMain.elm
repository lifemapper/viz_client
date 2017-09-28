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
import Animation exposing (Animation)
import AnimationFrame


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
                CurrentTick time_ ->
                    ( Animating dir time_ (Animation.animation time_ |> Animation.duration (1 * Time.second)) zipper, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Animating dir time animation zipper ->
            case msg of
                CurrentTick time_ ->
                    if Animation.isRunning time_ animation then
                        ( Animating dir time_ animation zipper, Cmd.none )
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


view : Model -> Html.Html Msg
view model =
    let
        ( transform, zipper ) =
            case model of
                Static zipper ->
                    ( "", zipper )

                StartAnimation _ zipper ->
                    ( "", zipper )

                Animating AnimateLeft time animation zipper ->
                    let
                        x =
                            Animation.animate time animation

                        translate =
                            0.25 * x

                        scale =
                            1.0 / (animation |> Animation.from 1 |> Animation.to 0.7 |> Animation.animate time)

                        rotate =
                            90 * x
                    in
                        ( "rotate("
                            ++ (toString rotate)
                            ++ ") "
                            ++ "scale("
                            ++ (toString scale)
                            ++ ") "
                            ++ "translate("
                            ++ (toString translate)
                            ++ ", 0) "
                        , zipper
                        )

                Animating AnimateRight time animation zipper ->
                    let
                        x =
                            Animation.animate time animation

                        translate =
                            -0.25 * x

                        scale =
                            1.0 / (animation |> Animation.from 1 |> Animation.to 0.7 |> Animation.animate time)

                        rotate =
                            -90 * x
                    in
                        ( "rotate("
                            ++ (toString rotate)
                            ++ ") "
                            ++ "scale("
                            ++ (toString scale)
                            ++ ") "
                            ++ "translate("
                            ++ (toString translate)
                            ++ ", 0) "
                        , zipper
                        )

                Animating AnimateUpLeft time animation zipper ->
                    let
                        x =
                            Animation.animate time animation

                        translate =
                            0.25 * (1 - x)

                        scale =
                            1.0 / (animation |> Animation.from 0.7 |> Animation.to 1 |> Animation.animate time)

                        rotate =
                            90 * (1 - x)
                    in
                        ( "rotate("
                            ++ (toString rotate)
                            ++ ") "
                            ++ "scale("
                            ++ (toString scale)
                            ++ ") "
                            ++ "translate("
                            ++ (toString translate)
                            ++ ", 0) "
                        , up zipper
                        )

                Animating AnimateUpRight time animation zipper ->
                    let
                        x =
                            Animation.animate time animation

                        translate =
                            -0.25 * (1 - x)

                        scale =
                            1.0 / (animation |> Animation.from 0.7 |> Animation.to 1 |> Animation.animate time)

                        rotate =
                            -90 * (1 - x)
                    in
                        ( "rotate("
                            ++ (toString rotate)
                            ++ ") "
                            ++ "scale("
                            ++ (toString scale)
                            ++ ") "
                            ++ "translate("
                            ++ (toString translate)
                            ++ ", 0) "
                        , up zipper
                        )
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
                [ drawTree transform <| getTree zipper ]
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
