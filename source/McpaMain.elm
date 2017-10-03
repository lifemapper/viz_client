module McpaMain exposing (..)

import Html
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import ExampleTree
import DecodeTree exposing (Tree(..), TreeData)
import LinearTree


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


type alias Model =
    TreeZipper


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        tree =
            getTree model

        treeDepth =
            LinearTree.treeDepth tree

        ( treeHeight, treeSvg ) =
            LinearTree.drawTree treeDepth tree
    in
        Html.div
            -- [ Html.Events.on "keyup" (Decode.map KeyUp <| Decode.field "key" Decode.string)
            [ Html.Attributes.style [ ( "display", "flex" ), ( "flex-direction", "flex-row" ) ]
            , Html.Attributes.tabindex 0
            ]
            [ svg
                [ width "800"
                , height "800"
                , viewBox ("0 0 " ++ (toString treeHeight) ++ " " ++ (toString treeHeight))
                , Html.Attributes.style [ ( "background", "lightblue" ), ( "font-family", "sans-serif" ) ]
                ]
                treeSvg
            , Html.div
                [ Html.Attributes.class "leaflet-map"
                  -- , Html.Attributes.attribute "data-map-column" (mapClade |> toString)
                , Html.Attributes.style [ ( "width", "800px" ), ( "height", "800px" ) ]
                ]
                []
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( top ExampleTree.tree, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
