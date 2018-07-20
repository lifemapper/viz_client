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
import Html.Attributes
import Html.Events


type alias StatsForSite =
    { id : Int
    , stats : List ( String, Maybe Float )
    }


port requestStats : () -> Cmd msg


port statsForSites :
    ({ sitesObserved : List StatsForSite
     , statNameLookup : List ( String, { name : String, description : String } )
     }
     -> msg
    )
    -> Sub msg


type alias Model =
    { variables : List String
    , statNames :
        Dict String { name : String, description : String }
    , selectedVariable : String
    }


type Msg
    = VariableSelectedMsg String
    | ReceivedStats
        { sitesObserved : List StatsForSite
        , statNameLookup : List ( String, { name : String, description : String } )
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedStats { sitesObserved, statNameLookup } ->
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
                                [ Html.Attributes.selected (v == selected)
                                , Html.Attributes.value v
                                ]
                                [ Dict.get v
                                    model.statNames
                                    |> Maybe.map .name
                                    |> Maybe.withDefault v
                                    |> Html.text
                                ]
                        )
                )
    in
        Html.div
            -- [ Html.Events.on "keyup" (Decode.map KeyUp <| Decode.field "key" Decode.string)
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "flex-direction", "flex-row" )
                , ( "font-family", "sans-serif" )
                ]
            , Html.Attributes.tabindex 0
            ]
            [ Html.div
                [ Html.Attributes.style [ ( "flex-grow", "1" ) ] ]
                [ Html.h3 [ Html.Attributes.style [ ( "text-align", "center" ), ( "text-decoration", "underline" ) ] ]
                    [ Html.text "Site Based Statistics Heat Map" ]
                , Html.div
                    [ Html.Attributes.class "leaflet-map"
                    , Html.Attributes.attribute "data-map-selected-var" model.selectedVariable
                    , Html.Attributes.style
                        [ ( "max-width", "900px" )
                        , ( "height", "500px" )
                        , ( "margin-left", "auto" )
                        , ( "margin-right", "auto" )
                        ]
                    ]
                    []
                , Html.p [ Html.Attributes.style [ ( "text-align", "center" ) ] ]
                    [ variableSelector model.selectedVariable VariableSelectedMsg
                    ]
                ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( { variables = []
              , statNames = Dict.empty
              , selectedVariable = ""
              }
            , requestStats ()
            )
        , update = update
        , view = view
        , subscriptions =
            always <| statsForSites ReceivedStats
        }
