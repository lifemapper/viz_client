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


module SubsetPam exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Events as Events
import Http
import QueryString as Q
import Decoder exposing (SolrList(..), SolrPAV(..), SolrPAVRecord)


type alias Facets =
    { algorithms : Set String
    , displayNames : Set String
    , modelScenarios : Set String
    , projectionScenarios : Set String
    , taxonClass : Set String
    , taxonFamily : Set String
    , taxonGenus : Set String
    , taxonKingdom : Set String
    , taxonOrder : Set String
    , taxonPhylum : Set String
    , taxonSpecies : Set String
    }


initFacets : Facets
initFacets =
    { algorithms = Set.empty
    , displayNames = Set.empty
    , modelScenarios = Set.empty
    , projectionScenarios = Set.empty
    , taxonClass = Set.empty
    , taxonFamily = Set.empty
    , taxonGenus = Set.empty
    , taxonKingdom = Set.empty
    , taxonOrder = Set.empty
    , taxonPhylum = Set.empty
    , taxonSpecies = Set.empty
    }


type alias Model =
    { facets : Facets
    , filters : Dict String String
    }


type Msg
    = GotSolrList SolrList
    | SetFilter String String


selector : (String -> Msg) -> Set String -> Html Msg
selector msg values =
    case Set.toList values of
        [] ->
            Html.select [] []

        [ value ] ->
            Html.select [] [ Html.option [] [ Html.text value ] ]

        values ->
            ("--" :: values)
                |> List.map (\v -> Html.option [] [ Html.text v ])
                |> Html.select [ Events.onInput msg ]


view : Model -> Html Msg
view { facets } =
    Html.div []
        [ Html.p [] [ Html.text "algorithm", selector (SetFilter "algorithmCode") facets.algorithms ]
        , Html.p [] [ Html.text "display name", selector (SetFilter "displayName") facets.displayNames ]
        , Html.p [] [ Html.text "model", selector (SetFilter "modelScenarioCode") facets.modelScenarios ]
        , Html.p [] [ Html.text "projection", selector (SetFilter "sdmProjScenarioCode") facets.projectionScenarios ]
        , Html.p [] [ Html.text "kingdom", selector (SetFilter "taxonKingdom") facets.taxonKingdom ]
        , Html.p [] [ Html.text "phylum", selector (SetFilter "taxonPhylum") facets.taxonPhylum ]
        , Html.p [] [ Html.text "class", selector (SetFilter "taxonClass") facets.taxonClass ]
        , Html.p [] [ Html.text "order", selector (SetFilter "taxonOrder") facets.taxonOrder ]
        , Html.p [] [ Html.text "family", selector (SetFilter "taxonFamily") facets.taxonFamily ]
        , Html.p [] [ Html.text "genus", selector (SetFilter "taxonGenus") facets.taxonGenus ]
        , Html.p [] [ Html.text "species", selector (SetFilter "taxonSpecies") facets.taxonSpecies ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSolrList (SolrList pavs) ->
            let
                facets =
                    List.foldr addAttrs initFacets pavs
            in
                ( { model | facets = facets }, Cmd.none )

        SetFilter key value ->
            let
                filters =
                    Dict.insert key value model.filters
            in
                ( { model | filters = filters }, getSolrList filters )


addAttrs : SolrPAV -> Facets -> Facets
addAttrs (SolrPAV pav) facets =
    let
        maybeInsert attr facet =
            attr |> Maybe.map (\a -> Set.insert a facet) |> Maybe.withDefault facet
    in
        { algorithms = maybeInsert pav.algorithmCode facets.algorithms
        , displayNames = Set.insert pav.displayName facets.displayNames
        , modelScenarios = maybeInsert pav.modelScenarioCode facets.modelScenarios
        , projectionScenarios = maybeInsert pav.sdmProjScenarioCode facets.projectionScenarios
        , taxonKingdom = maybeInsert pav.taxonKingdom facets.taxonKingdom
        , taxonPhylum = maybeInsert pav.taxonPhylum facets.taxonPhylum
        , taxonClass = maybeInsert pav.taxonClass facets.taxonClass
        , taxonOrder = maybeInsert pav.taxonOrder facets.taxonOrder
        , taxonFamily = maybeInsert pav.taxonFamily facets.taxonFamily
        , taxonGenus = maybeInsert pav.taxonGenus facets.taxonGenus
        , taxonSpecies = maybeInsert pav.taxonSpecies facets.taxonSpecies
        }


getSolrList : Dict String String -> Cmd Msg
getSolrList filters =
    let
        query =
            filters |> Dict.foldl Q.add Q.empty |> Q.render
    in
        Http.request
            { method = "GET"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = "http://notyeti-193.lifemapper.org/api/v2/globalPam" ++ query
            , body = Http.emptyBody
            , expect = Http.expectJson Decoder.decodeSolrList
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.send gotSolrList


gotSolrList : Result Http.Error SolrList -> Msg
gotSolrList result =
    case result of
        Ok list ->
            GotSolrList list

        Err err ->
            Debug.crash (toString err)


main : Program Never Model Msg
main =
    Html.program
        { init = ( { facets = initFacets, filters = Dict.empty }, getSolrList Dict.empty )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
