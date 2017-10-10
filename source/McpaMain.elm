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


getData : TreeZipper -> TreeData
getData zipper =
    case getTree zipper of
        Leaf data ->
            data

        Node data _ _ ->
            data


type AnimationDirection
    = AnimateUpLeft
    | AnimateUpRight
    | AnimateLeft
    | AnimateRight


type AnimationState
    = Start AnimationDirection
    | Running AnimationDirection Time Animation
    | Static


type alias Model =
    { zipper : TreeZipper
    , animationState : AnimationState
    }


type Msg
    = KeyUp String
    | CurrentTick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ zipper, animationState } as model) =
    case animationState of
        Static ->
            case msg of
                KeyUp "ArrowDown" ->
                    case zipper of
                        ( _, Left _ _ _ ) ->
                            ( { model | animationState = Start AnimateUpLeft }, Cmd.none )

                        ( _, Right _ _ _ ) ->
                            ( { model | animationState = Start AnimateUpRight }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                KeyUp "ArrowLeft" ->
                    if zipper /= left zipper then
                        ( { model | animationState = Start AnimateLeft }, Cmd.none )
                    else
                        ( model, Cmd.none )

                KeyUp "ArrowRight" ->
                    if zipper /= right zipper then
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
                                ( { model | animationState = Static, zipper = up zipper }, Cmd.none )

                            AnimateUpRight ->
                                ( { model | animationState = Static, zipper = up zipper }, Cmd.none )

                            AnimateLeft ->
                                ( { model | animationState = Static, zipper = left zipper }, Cmd.none )

                            AnimateRight ->
                                ( { model | animationState = Static, zipper = right zipper }, Cmd.none )

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
view { zipper, animationState } =
    let
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
                        ( transforms, up zipper |> getTree, getData zipper |> .cladeId )

                Running AnimateUpRight time animation ->
                    let
                        transforms =
                            [ Rotate (animation |> A.from -90 |> A.to 0 |> A.animate time)
                            , Scale (1.0 / (animation |> A.from 0.7 |> A.to 1 |> A.animate time))
                            , Translate (animation |> A.from -0.25 |> A.to 0 |> A.animate time)
                            ]
                    in
                        ( transforms, up zipper |> getTree, getData zipper |> .cladeId )
    in
        Html.div
            [ Html.Events.on "keyup" (Decode.map KeyUp <| Decode.field "key" Decode.string)
            , Html.Attributes.style [ ( "display", "flex" ), ( "flex-direction", "flex-row" ) ]
            , Html.Attributes.tabindex 0
            ]
            [ svg
                [ width "800"
                , height "800"
                , viewBox "-0.5 -0.5 1 1"
                , Html.Attributes.style [ ( "background", "lightblue" ), ( "font-family", "sans-serif" ) ]
                ]
                [ drawTree (transforms |> List.map transformToAttr |> String.join " ") tree ]
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
        , Html.Events.onClick (KeyUp "ArrowLeft")
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
        , Html.Events.onClick (KeyUp "ArrowRight")
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
        , Html.Events.onClick (KeyUp "ArrowDown")
        ]
        []
    ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( { animationState = Static, zipper = top ExampleTree.tree }, Cmd.none )
        , update = update
        , view = view
        , subscriptions =
            (\model ->
                case model.animationState of
                    Start _ ->
                        AnimationFrame.times CurrentTick

                    Running _ _ _ ->
                        AnimationFrame.times CurrentTick

                    _ ->
                        Sub.none
            )
        }
