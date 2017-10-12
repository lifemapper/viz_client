module LinearTreeView exposing (view)

import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import TreeZipper exposing (TreeZipper, Position(..), moveToward, getTree, getData, getPosition)
import McpaModel exposing (..)
import LinearTree

view : Model -> Html.Html Msg
view { zipper } =
    let
        tree =
            getTree zipper

        treeDepth =
            LinearTree.treeDepth tree

        ( treeHeight, treeSvg ) =
            LinearTree.drawTree treeDepth tree
    in
        Html.div
            [ Html.Events.on "keyup" (Decode.map KeyUp <| Decode.field "key" Decode.string)
            , Html.Attributes.style [ ( "display", "flex" ), ( "flex-direction", "flex-row" ) ]
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
