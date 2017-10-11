module McpaMain exposing (..)

import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import ExampleTree
import DecodeTree exposing (Tree(..), TreeData)
import TreeZipper exposing (TreeZipper, Position(..), moveToward, getTree, getData, getPosition)
import Time exposing (Time)
import Animation as A exposing (Animation)
import AnimationFrame
import Ease


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
    , mouseIn : Bool
    }


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
            [ Html.Events.on "keyup" (Decode.map KeyUp <| Decode.field "key" Decode.string)
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
        { init =
            ( { animationState = Static
              , zipper = TreeZipper.start ExampleTree.tree
              , mouseIn = False
              }
            , Cmd.none
            )
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
