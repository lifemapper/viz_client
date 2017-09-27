module McpaMain exposing (..)

import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import ExampleTree
import DecodeTree exposing (Tree(..), TreeData)


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


type alias Model =
    TreeZipper


type Msg
    = KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg:" msg of
        KeyUp "ArrowDown" ->
            ( up model, Cmd.none )

        KeyUp "ArrowLeft" ->
            ( left model, Cmd.none )

        KeyUp "ArrowRight" ->
            ( right model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
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
            [ line [ x1 "0", x2 "0", y1 "0.5", y2 "0", stroke "darkolivegreen", strokeWidth "0.014" ] []
            , drawTree (getTree model)
            ]
        ]


drawTree : Tree -> Svg msg
drawTree tree =
    case tree of
        Leaf data ->
            g []
                [ ellipse [ cx "0", cy "0", rx "0.2", ry "0.3", fill "green", stroke "lightgreen", strokeWidth "0.005" ] []
                , text_ [ x "0", y "0", textAnchor "middle", fontSize "0.05", transform "rotate(-90)", fill "white" ]
                    [ text data.name ]
                ]

        Node data left right ->
            g []
                [ line [ x1 "-0.25", x2 "0.25", y1 "0", y2 "0", stroke "darkolivegreen", strokeWidth "0.01" ] []
                , g [ transform "translate(-0.25, 0) scale(0.7) rotate(-90)" ]
                    [ drawTree left ]
                , g [ transform "translate(0.25, 0) scale(0.7) rotate(90)" ]
                    [ drawTree right ]
                ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( top ExampleTree.tree, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
