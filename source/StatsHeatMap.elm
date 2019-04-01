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


port module StatsHeatMap exposing (..)

import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import Dict exposing (Dict)
import Html
import Html.Attributes as A
import Html.Events
import Helpers exposing (formatNumber)


type alias StatsForSite =
    { id : Int
    , stats : List ( String, Maybe Float )
    }


port requestStats : () -> Cmd msg


port statsForSites :
    ({ sitesObserved : List StatsForSite
     , statNameLookup : List ( String, { name : String, description : String } )
     , statRanges : List ( String, { min : Float, max : Float } )
     }
     -> msg
    )
    -> Sub msg


type alias Model =
    { variables : List String
    , statNames : Dict String { name : String, description : String }
    , statRanges : Dict String { min : Float, max : Float }
    , selectedVariable : String
    }


type Msg
    = VariableSelectedMsg String
    | ReceivedStats
        { sitesObserved : List StatsForSite
        , statNameLookup : List ( String, { name : String, description : String } )
        , statRanges : List ( String, { min : Float, max : Float } )
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedStats { sitesObserved, statNameLookup, statRanges } ->
            let
                variables =
                    sitesObserved
                        |> List.concatMap (.stats >> List.map Tuple.first)
                        |> List.foldl Set.insert Set.empty
                        |> Set.toList
                        |> List.sortBy String.toLower

                selectedVariable =
                    "alpha"

                -- variables |> List.getAt 0 |> Maybe.withDefault ""
            in
                ( { model
                    | variables = variables
                    , statNames = Dict.fromList statNameLookup
                    , statRanges = Dict.fromList statRanges
                    , selectedVariable = selectedVariable
                  }
                , Cmd.none
                )

        VariableSelectedMsg col ->
            ( { model | selectedVariable = col }, Cmd.none )


type alias Record =
    { siteId : Int, x : Float, y : Float }


extractRecord : String -> String -> StatsForSite -> Maybe Record
extractRecord xCol yCol stats =
    let
        getValue col =
            stats.stats
                |> List.find (Tuple.first >> ((==) col))
                |> Maybe.map Tuple.second
                |> Maybe.join
                |> Result.fromMaybe "missing value"
    in
        Result.map2 (Record stats.id) (getValue xCol) (getValue yCol) |> Result.toMaybe


recordsFromStats : String -> String -> List StatsForSite -> List Record
recordsFromStats xCol yCol stats =
    List.map (extractRecord xCol yCol) stats |> Maybe.values


view : Model -> Html.Html Msg
view model =
    let
        variableSelector selected select =
            Html.select [ Html.Events.onInput select ]
                (model.variables
                    |> List.map
                        (\v ->
                            Html.option
                                [ A.selected (v == selected)
                                , A.value v
                                ]
                                [ Dict.get v
                                    model.statNames
                                    |> Maybe.map .name
                                    |> Maybe.withDefault v
                                    |> Html.text
                                ]
                        )
                )

        legend =
            Html.div
                [ A.style
                    [ ( "height", "400px" )
                      -- , ( "width", "50px" )
                    , ( "background", "linear-gradient(white, red)" )
                    , ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    , ( "justify-content", "space-between" )
                    , ( "margin", "50px 10px" )
                    , ( "border", "solid 2px" )
                    ]
                ]
                [ Html.p [ A.style [ ( "text-align", "right" ), ( "margin", "0 5px" ) ] ]
                    [ Dict.get model.selectedVariable model.statRanges
                        |> Maybe.map (.min >> formatNumber)
                        |> Maybe.withDefault ""
                        |> Html.text
                    ]
                , Html.p [ A.style [ ( "text-align", "right" ), ( "margin", "0 5px" ) ] ]
                    [ Dict.get model.selectedVariable model.statRanges
                        |> Maybe.map (.max >> formatNumber)
                        |> Maybe.withDefault ""
                        |> Html.text
                    ]
                ]

        text =
            Html.p [ A.style [ ( "width", "900px" ) ] ]
                [ Dict.get model.selectedVariable model.statNames
                    |> Maybe.map (.description >> flip (++) ".")
                    |> Maybe.withDefault ""
                    |> Html.text
                ]

        map =
            Html.div [ A.style [ ( "display", "flex" ), ( "margin-top", "10px" ) ] ]
                [ Html.div
                    [ A.class "leaflet-map"
                    , A.attribute "data-map-selected-var" model.selectedVariable
                    , A.style [ ( "width", "900px" ), ( "height", "500px" ) ]
                    ]
                    []
                , legend
                ]
    in
        Html.div [ A.style [ ( "font-family", "sans-serif" ) ] ]
            [ Html.h2 [] [ Html.text "BiotaPhy - Modeled Map Statistics" ]
            , variableSelector model.selectedVariable VariableSelectedMsg
            , map
            , text
            ]


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( { variables = []
              , statNames = Dict.empty
              , statRanges = Dict.empty
              , selectedVariable = ""
              }
            , requestStats ()
            )
        , update = update
        , view = view
        , subscriptions =
            always <| statsForSites ReceivedStats
        }
