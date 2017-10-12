module LinearTreeView exposing (view)

import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import TreeZipper exposing (TreeZipper, Position(..), moveToward, getTree, getData, getPosition)
import DecodeTree exposing (Tree(..), TreeData)
import McpaModel exposing (..)


view : Model -> Html.Html Msg
view { root, zipper } =
    let
        treeDepth_ =
            treeDepth root

        treeBreadth_ =
            treeBreadth root

        treeLength_ =
            treeLength root

        ( treeHeight, treeSvg ) =
            drawTree treeLength_ root zipper

        mapClade =
            getData zipper |> .cladeId
    in
        Html.div
            [ Html.Events.on "keyup" (Decode.map KeyUp <| Decode.field "key" Decode.string)
            , Html.Attributes.style [ ( "display", "flex" ), ( "flex-direction", "flex-row" ) ]
            ]
            [ svg
                [ width "800"
                , height "800"
                , viewBox ("0 0 100 100")
                , Html.Attributes.style [ ( "background", "white" ), ( "font-family", "sans-serif" ) ]
                , Html.Events.onClick (KeyUp "ArrowDown")
                ]
                treeSvg
            , Html.div
                [ Html.Attributes.class "leaflet-map"
                , Html.Attributes.attribute "data-map-column" (mapClade |> toString)
                , Html.Attributes.style [ ( "width", "800px" ), ( "height", "800px" ) ]
                ]
                []
            ]


treeLength : Tree -> Float
treeLength tree =
    case tree of
        Leaf { length } ->
            length |> Maybe.withDefault 0

        Node { length } left right ->
            let
                thisLength =
                    length |> Maybe.withDefault 0
            in
                thisLength + Basics.max (treeLength left) (treeLength right)


treeDepth : Tree -> Int
treeDepth tree =
    case tree of
        Leaf _ ->
            1

        Node _ left right ->
            1 + Basics.max (treeDepth left) (treeDepth right)


treeBreadth : Tree -> Int
treeBreadth tree =
    case tree of
        Leaf _ ->
            1

        Node _ left right ->
            (treeBreadth left) + (treeBreadth right)


scaleLength : Float -> Float -> Float
scaleLength totalLength thisLength =
    80 * thisLength / totalLength


drawTree : Float -> Tree -> TreeZipper -> ( Float, List (Svg Msg) )
drawTree totalLength tree zipper =
    case tree of
        Leaf data ->
            let
                length =
                    data.length |> Maybe.map (scaleLength totalLength) |> Maybe.withDefault 10
            in
                ( 1
                , [ line [ x1 "0", x2 (toString length), y1 "0.5", y2 "0.5", strokeWidth "0.1", stroke "black" ] []
                  , text_ [ x (toString (length + 2)), y "0.75", fontSize "0.8" ]
                        [ text data.name ]
                  ]
                )

        Node data left right ->
            let
                ( leftHeight, leftNodes ) =
                    drawTree totalLength left zipper

                ( rightHeight, rightNodes ) =
                    drawTree totalLength right zipper

                thisHeight =
                    leftHeight + rightHeight

                length =
                    data.length |> Maybe.map (scaleLength totalLength) |> Maybe.withDefault 10

                boxes =
                    if tree == getTree zipper then
                        [ rect
                            [ x "0"
                            , y "0"
                            , width "100"
                            , height <| toString leftHeight
                            , fill "blue"
                            , fillOpacity "0.2"
                            , Html.Events.onClick (KeyUp "ArrowLeft")
                            ]
                            []
                        , rect
                            [ x "0"
                            , y <| toString leftHeight
                            , width "100"
                            , height <| toString rightHeight
                            , fill "red"
                            , fillOpacity "0.2"
                            , Html.Events.onClick (KeyUp "ArrowRight")
                            ]
                            []
                        ]
                    else
                        []
            in
                ( thisHeight
                , [ line
                        [ x1 "0"
                        , x2 (toString length)
                        , y1 <| toString (thisHeight / 2.0)
                        , y2 <| toString (thisHeight / 2.0)
                        , strokeWidth "0.1"
                        , stroke "black"
                        ]
                        []
                  , line
                        [ x1 (toString length)
                        , x2 (toString length)
                        , y1 (toString (leftHeight / 2))
                        , y2 (toString (leftHeight + rightHeight / 2))
                        , strokeWidth "0.1"
                        , stroke "black"
                        ]
                        []
                  , g [ transform <| "translate(" ++ (toString length) ++ ",0)" ] leftNodes
                  , g [ transform <| "translate(" ++ (toString length) ++ "," ++ (toString leftHeight) ++ ")" ] rightNodes
                  ]
                    ++ boxes
                )
