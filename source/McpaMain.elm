module McpaMain exposing (..)

import Html
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import ExampleTree
import DecodeTree exposing (Tree(..), TreeData)


type alias Model =
    {}


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( {}, Cmd.none )


view : Model -> Html.Html Msg
view _ =
    svg [ width "800", height "800", viewBox "-0.5 -0.5 1 1", Html.Attributes.style [("background", "lightblue"), ("font-family", "sans-serif")] ]
        [ drawTree ExampleTree.tree
        , line [ x1 "0", x2 "0", y1 "0.5", y2 "0", stroke "darkolivegreen", strokeWidth "0.014" ] []
        ]


drawTree : Tree -> Svg msg
drawTree tree =
    case tree of
        Leaf data ->
            g []
                [ ellipse [ cx "0", cy "0", rx "0.2", ry "0.3", fill "green", stroke "lightgreen", strokeWidth "0.005" ] []
                , text_ [ x "0", y "0", textAnchor "middle", fontSize "0.05", transform "rotate(90)", fill "white" ]
                    [ data.name |> Maybe.withDefault "" |> String.map (\c -> if c ==  '_' then ' ' else c) |> text ]
                ]

        Node data left right ->
            g []
                [ line [ x1 "-0.25", x2 "0.25", y1 "0", y2 "0", stroke "darkolivegreen", strokeWidth "0.01" ] []
                , g [ transform "translate(-0.25, 0) scale(0.7) rotate(90)" ]
                    [ drawTree left ]
                , g [ transform "translate(0.25, 0) scale(0.7) rotate(-90)" ]
                    [ drawTree right ]
                ]


main : Program Never {} Msg
main =
    Html.program
        { init = ( {}, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
