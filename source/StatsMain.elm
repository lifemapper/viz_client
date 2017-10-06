port module StatsMain exposing (..)

import List.Extra as List
import Maybe.Extra as Maybe
import Html
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import ExampleStats exposing (exampleStats)


type alias MouseEvent =
    { eventType : String
    , x : Float
    , y : Float
    , ctrlKey : Bool
    }


port mouseEvent : (MouseEvent -> msg) -> Sub msg


type alias Model =
    { selected : List Record
    , selecting : Maybe ( Point, Point )
    , data : List Record
    , scale : DataScale
    }


type Msg
    = MouseMsg MouseEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg event ->
            case event.eventType of
                "mousedown" ->
                    let
                        p =
                            Point (event.x / 800 * 1.2 + (-0.1)) (event.y / 800 * 1.2 + (-0.1))

                        selecting =
                            Just ( p, p )
                    in
                        ( { model | selecting = selecting }, Cmd.none )

                "mousemove" ->
                    case model.selecting of
                        Just ( p1, _ ) ->
                            let
                                p2 =
                                    Point (event.x / 800 * 1.2 + (-0.1)) (event.y / 800 * 1.2 + (-0.1))

                                selecting =
                                    Just ( p1, p2 )
                            in
                                ( { model | selecting = selecting }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                "mouseup" ->
                    case model.selecting of
                        Just ( p1_, _ ) ->
                            let
                                p2_ =
                                    Point (event.x / 800 * 1.2 + (-0.1)) (event.y / 800 * 1.2 + (-0.1))

                                ( p1, p2 ) =
                                    ( svg2data model.scale p1_, svg2data model.scale p2_ )

                                ( x1, y1 ) =
                                    ( Basics.min p1.x p2.x, Basics.min p1.y p2.y )

                                ( x2, y2 ) =
                                    ( Basics.max p1.x p2.x, Basics.max p1.y p2.y )

                                newlySelected =
                                    model.data |> List.filter (\d -> d.x > x1 && d.x < x2 && d.y > y1 && d.y < y2)

                                selected =
                                    if event.ctrlKey then
                                        model.selected ++ newlySelected
                                    else
                                        newlySelected
                            in
                                ( { model | selected = selected, selecting = Nothing }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


type alias Record =
    { siteId : Int, x : Float, y : Float }


extractRecord : List String -> Maybe Record
extractRecord record =
    let
        siteId =
            List.getAt 0 record |> Result.fromMaybe "No SiteId in CSV" |> Result.andThen String.toInt

        alpha =
            List.getAt 3 record |> Result.fromMaybe "No alpha in CSV" |> Result.andThen String.toFloat

        phi =
            List.getAt 6 record |> Result.fromMaybe "No phi in CSV" |> Result.andThen String.toFloat
    in
        Result.map3 Record siteId phi alpha |> Result.toMaybe


data_ : List Record
data_ =
    List.map extractRecord exampleStats.records |> Maybe.values


agg : (number -> number -> number) -> List number -> Maybe number
agg fn xs =
    case xs of
        [] ->
            Nothing

        x :: xs ->
            List.foldl fn x xs |> Just


type alias DataScale =
    { minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    }


computeScale : List Record -> DataScale
computeScale data =
    { minX =
        Maybe.withDefault 0 <| agg Basics.min <| List.map .x data
    , maxX =
        Maybe.withDefault 0 <| agg Basics.max <| List.map .x data
    , minY =
        Maybe.withDefault 0 <| agg Basics.min <| List.map .y data
    , maxY =
        Maybe.withDefault 0 <| agg Basics.max <| List.map .y data
    }


type alias Point =
    { x : Float
    , y : Float
    }


data2svg : DataScale -> Point -> Point
data2svg ds { x, y } =
    { x = (x - ds.minX) / (ds.maxX - ds.minX)
    , y = 1 - (y - ds.minY) / (ds.maxY - ds.minY)
    }


svg2data : DataScale -> Point -> Point
svg2data ds { x, y } =
    { x = x * (ds.maxX - ds.minX) + ds.minX
    , y = (1 - y) * (ds.maxY - ds.minY) + ds.minY
    }


drawScatter : DataScale -> List Record -> List Record -> List (Svg msg)
drawScatter scale data selected =
    let
        plot record =
            let
                point =
                    data2svg scale (Point record.x record.y)
            in
                circle
                    [ point.x |> toString |> cx
                    , point.y |> toString |> cy
                    , r "0.006"
                    , fillOpacity "0.8"
                    , if List.member record selected then
                        fill "red"
                      else
                        fill "black"
                    ]
                    []
    in
        ((g [] <| drawAxis scale.minX scale.maxX)
            :: (g [ transform "rotate(90) scale(1,-1)" ] <| drawAxis scale.minY scale.maxY)
            :: List.map plot data
        )


drawAxis : Float -> Float -> List (Svg msg)
drawAxis min max =
    [ line [ x1 "0", x2 "1", y1 "0", y2 "0", strokeWidth "0.001", stroke "black" ] []
    , line [ x1 "0.1", x2 "0.1", y1 "0", y2 "-0.02", strokeWidth "0.001", stroke "black" ] []
    , line [ x1 "0.2", x2 "0.2", y1 "0", y2 "-0.02", strokeWidth "0.001", stroke "black" ] []
    , line [ x1 "0.3", x2 "0.3", y1 "0", y2 "-0.02", strokeWidth "0.001", stroke "black" ] []
    , line [ x1 "0.4", x2 "0.4", y1 "0", y2 "-0.02", strokeWidth "0.001", stroke "black" ] []
    , line [ x1 "0.5", x2 "0.5", y1 "0", y2 "-0.02", strokeWidth "0.001", stroke "black" ] []
    , line [ x1 "0.6", x2 "0.6", y1 "0", y2 "-0.02", strokeWidth "0.001", stroke "black" ] []
    , line [ x1 "0.7", x2 "0.7", y1 "0", y2 "-0.02", strokeWidth "0.001", stroke "black" ] []
    , line [ x1 "0.8", x2 "0.8", y1 "0", y2 "-0.02", strokeWidth "0.001", stroke "black" ] []
    , line [ x1 "0.9", x2 "0.9", y1 "0", y2 "-0.02", strokeWidth "0.001", stroke "black" ] []
    , line [ x1 "1.0", x2 "1.0", y1 "0", y2 "-0.02", strokeWidth "0.001", stroke "black" ] []
    ]


view : Model -> Html.Html Msg
view model =
    let
        selectionBox =
            case model.selecting of
                Just ( p1, p2 ) ->
                    let
                        ( x_, y_ ) =
                            ( Basics.min p1.x p2.x, Basics.min p1.y p2.y )

                        ( w, h ) =
                            ( Basics.abs (p2.x - p1.x), Basics.abs (p2.y - p1.y) )
                    in
                        [ rect
                            [ x (toString x_)
                            , y (toString y_)
                            , width <| toString w
                            , height <| toString h
                            , fill "red"
                            , fillOpacity "0.4"
                            ]
                            []
                        ]

                _ ->
                    []

        selectedSiteIds =
            model.selected |> List.map (.siteId >> toString) |> String.join " "
    in
        Html.div
            -- [ Html.Events.on "keyup" (Decode.map KeyUp <| Decode.field "key" Decode.string)
            [ Html.Attributes.style [ ( "display", "flex" ), ( "flex-direction", "flex-row" ) ]
            , Html.Attributes.tabindex 0
            ]
            [ Html.div
                [ Html.Attributes.class "leaflet-map"
                , Html.Attributes.attribute "data-map-sites" selectedSiteIds
                , Html.Attributes.style [ ( "width", "800px" ), ( "height", "800px" ) ]
                ]
                []
            , svg
                [ width "800"
                , height "800"
                , viewBox "-0.1 -0.1 1.2 1.2"
                , Html.Attributes.id "plot"
                ]
                ([ g [ transform "" ] <| (drawScatter model.scale model.data model.selected) ] ++ selectionBox)
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( { selected = [], selecting = Nothing, data = data_, scale = computeScale data_ }, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always <| mouseEvent MouseMsg
        }
