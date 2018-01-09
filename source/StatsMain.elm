{-
   Copyright (C) 2018, University of Kansas Center for Research

   Lifemapper Project, lifemapper [at] ku [dot] edu,
   Biodiversity Institute,
   1345 Jayhawk Boulevard, Lawrence, Kansas, 66045, USA

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.
-}


port module StatsMain exposing (..)

import List.Extra as List
import Maybe.Extra as Maybe
import Html
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import ExampleStats exposing (exampleStats)
import Csv exposing (Csv)


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


type PixelPoint
    = PixelPoint Point


type SvgPoint
    = SvgPoint Point


type DataPoint
    = DataPoint Point


data2svg : DataScale -> DataPoint -> SvgPoint
data2svg ds (DataPoint { x, y }) =
    SvgPoint { x = (x - ds.minX) / (ds.maxX - ds.minX), y = 1 - (y - ds.minY) / (ds.maxY - ds.minY) }


svg2data : DataScale -> SvgPoint -> DataPoint
svg2data ds (SvgPoint { x, y }) =
    DataPoint { x = x * (ds.maxX - ds.minX) + ds.minX, y = (1 - y) * (ds.maxY - ds.minY) + ds.minY }


type alias SvgViewBox =
    { width : Float
    , height : Float
    , minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    }


svgViewBox : SvgViewBox
svgViewBox =
    { width = 800
    , height = 800
    , minX = -0.1
    , minY = 0
    , maxX = 1.1
    , maxY = 1.1
    }


pixel2svg : SvgViewBox -> PixelPoint -> SvgPoint
pixel2svg { width, height, minX, minY, maxX, maxY } (PixelPoint { x, y }) =
    SvgPoint { x = x / width * (maxX - minX) + minX, y = y / height * (maxY - minY) + minY }


inViewBox : SvgViewBox -> SvgPoint -> Bool
inViewBox { width, height, minX, minY, maxX, maxY } (SvgPoint { x, y }) =
    minX <= x && maxX >= x && minY <= y && maxY >= y


type alias MouseEvent =
    { eventType : String
    , x : Float
    , y : Float
    , ctrlKey : Bool
    }


port mouseEvent : (MouseEvent -> msg) -> Sub msg


port sitesSelected : (List Int -> msg) -> Sub msg


type alias Model =
    { selected : List Record
    , selecting : Maybe ( SvgPoint, SvgPoint )
    , data : Csv
    , xCol : Int
    , yCol : Int
    }


type Msg
    = MouseMsg MouseEvent
    | SitesSelectedMsg (List Int)
    | XColSelectedMsg String
    | YColSelectedMsg String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        XColSelectedMsg col ->
            col
                |> String.toInt
                |> Result.toMaybe
                |> Maybe.map (\i -> ( { model | xCol = i }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        YColSelectedMsg col ->
            col
                |> String.toInt
                |> Result.toMaybe
                |> Maybe.map (\i -> ( { model | yCol = i }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        SitesSelectedMsg sites ->
            let
                selected =
                    model.data
                        |> recordsFromCsv model.xCol model.yCol
                        |> List.filter (\r -> List.member r.siteId sites)
            in
                ( { model | selected = selected }, Cmd.none )

        MouseMsg event ->
            case event.eventType of
                "mousedown" ->
                    let
                        p =
                            pixel2svg svgViewBox (PixelPoint { x = event.x, y = event.y })

                        selecting =
                            if inViewBox svgViewBox p then
                                Just ( p, p )
                            else
                                Nothing
                    in
                        ( { model | selecting = selecting }, Cmd.none )

                "mousemove" ->
                    case model.selecting of
                        Just ( p1, _ ) ->
                            let
                                p2 =
                                    pixel2svg svgViewBox (PixelPoint { x = event.x, y = event.y })

                                selecting =
                                    Just ( p1, p2 )
                            in
                                ( { model | selecting = selecting }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                "mouseup" ->
                    case model.selecting of
                        Just ( p1_, _ ) ->
                            let
                                records =
                                    recordsFromCsv model.xCol model.yCol model.data

                                scale =
                                    computeScale records

                                p2_ =
                                    pixel2svg svgViewBox (PixelPoint { x = event.x, y = event.y })

                                ( DataPoint p1, DataPoint p2 ) =
                                    ( svg2data scale p1_, svg2data scale p2_ )

                                ( x1, y1 ) =
                                    ( Basics.min p1.x p2.x, Basics.min p1.y p2.y )

                                ( x2, y2 ) =
                                    ( Basics.max p1.x p2.x, Basics.max p1.y p2.y )

                                newlySelected =
                                    records |> List.filter (\d -> d.x > x1 && d.x < x2 && d.y > y1 && d.y < y2)

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


extractRecord : Int -> Int -> List String -> Maybe Record
extractRecord xCol yCol record =
    let
        siteId =
            List.getAt 0 record |> Result.fromMaybe "No SiteId in CSV" |> Result.andThen String.toInt

        x =
            List.getAt (xCol + 3) record |> Result.fromMaybe "missing column in CSV" |> Result.andThen String.toFloat

        y =
            List.getAt (yCol + 3) record |> Result.fromMaybe "missing column in CSV" |> Result.andThen String.toFloat
    in
        Result.map3 Record siteId x y |> Result.toMaybe


recordsFromCsv : Int -> Int -> Csv -> List Record
recordsFromCsv xCol yCol csv =
    List.map (extractRecord xCol yCol) csv.records |> Maybe.values


variables : Csv -> List String
variables csv =
    List.drop 3 csv.headers


agg : (number -> number -> number) -> List number -> Maybe number
agg fn xs =
    case xs of
        [] ->
            Nothing

        x :: xs ->
            List.foldl fn x xs |> Just


drawScatter : Model -> List (Svg msg)
drawScatter model =
    let
        vars =
            variables model.data

        xVar =
            List.getAt model.xCol vars |> Maybe.withDefault ""

        yVar =
            List.getAt model.yCol vars |> Maybe.withDefault ""

        records =
            recordsFromCsv model.xCol model.yCol model.data

        scale =
            computeScale records

        plot record =
            let
                (SvgPoint point) =
                    data2svg scale (DataPoint { x = record.x, y = record.y })
            in
                circle
                    [ point.x |> toString |> cx
                    , point.y |> toString |> cy
                    , r "0.006"
                    , fillOpacity "0.8"
                    , if List.member record model.selected then
                        fill "red"
                      else
                        fill "black"
                    ]
                    []
    in
        drawXAxis xVar scale.minX scale.maxX
            ++ drawYAxis yVar scale.minY scale.maxY
            ++ List.map plot records


drawXAxis : String -> Float -> Float -> List (Svg msg)
drawXAxis label min max =
    let
        ticks =
            List.range 1 10
                |> List.map (toFloat >> ((*) 0.1) >> toString)
                |> List.map (\x -> line [ x1 x, x2 x, y1 "1", y2 "1.02", strokeWidth "0.001", stroke "black" ] [])
    in
        (line [ x1 "0", x2 "1", y1 "1", y2 "1", strokeWidth "0.001", stroke "black" ] []
            :: ticks
        )


drawYAxis : String -> Float -> Float -> List (Svg msg)
drawYAxis label min max =
    let
        ticks =
            List.range 1 10
                |> List.map (toFloat >> ((*) 0.1) >> ((-) 1) >> toString)
                |> List.map (\y -> line [ y1 y, y2 y, x1 "0", x2 "-0.02", strokeWidth "0.001", stroke "black" ] [])
    in
        (line [ y1 "0", y2 "1", x1 "0", x2 "0", strokeWidth "0.001", stroke "black" ] []
            :: ticks
        )


svgViewBox2String : SvgViewBox -> String
svgViewBox2String { width, height, minX, minY, maxX, maxY } =
    [ minX, minY, maxX - minX, maxY - minY ] |> List.map toString |> String.join " "


view : Model -> Html.Html Msg
view model =
    let
        selectionBox =
            model.selecting
                |> Maybe.toList
                |> List.map
                    (\( SvgPoint p1, SvgPoint p2 ) ->
                        let
                            ( x_, y_ ) =
                                ( Basics.min p1.x p2.x, Basics.min p1.y p2.y )

                            ( w, h ) =
                                ( Basics.abs (p2.x - p1.x), Basics.abs (p2.y - p1.y) )
                        in
                            rect
                                [ x (toString x_)
                                , y (toString y_)
                                , width <| toString w
                                , height <| toString h
                                , fill "red"
                                , fillOpacity "0.4"
                                ]
                                []
                    )

        selectedSiteIds =
            model.selected |> List.map (.siteId >> toString) |> String.join " "

        records =
            recordsFromCsv model.xCol model.yCol model.data

        availableVariables =
            variables model.data

        variableSelector selected select =
            Html.select [ Html.Events.onInput select ]
                (availableVariables
                    |> List.indexedMap
                        (\i v ->
                            Html.option
                                [ Html.Attributes.selected (i == selected)
                                , Html.Attributes.value (toString i)
                                ]
                                [ Html.text v ]
                        )
                )
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
            , Html.div []
                [ Html.p [ Html.Attributes.style [ ( "text-align", "center" ), ( "margin-bottom", "0" ) ] ]
                    [ variableSelector model.yCol YColSelectedMsg
                    , Html.text " vs "
                    , variableSelector model.xCol XColSelectedMsg
                    ]
                , svg
                    [ width <| toString svgViewBox.width
                    , height <| toString svgViewBox.height
                    , viewBox <| svgViewBox2String svgViewBox
                    , Html.Attributes.id "plot"
                    ]
                    ([ g [ transform "" ] <| (drawScatter model) ] ++ selectionBox)
                ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( { selected = []
              , selecting = Nothing
              , data = exampleStats
              , xCol = 0
              , yCol = 3
              }
            , Cmd.none
            )
        , update = update
        , view = view
        , subscriptions =
            always <|
                Sub.batch
                    [ mouseEvent MouseMsg
                    , sitesSelected SitesSelectedMsg
                    ]
        }
