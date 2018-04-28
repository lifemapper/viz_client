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
import Html.Attributes
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
    , pavs : String
    , shapeGrid : Maybe String
    }


type Msg
    = GotSolrList SolrList
    | GotShapeGrid String
    | SetFilter String String
    | ClearFilter String


selector : Dict String String -> String -> Set String -> Html Msg
selector filters key values =
    case Dict.get key filters of
        Just value ->
            Html.span []
                [ Html.text value
                , Html.button [ Events.onClick <| ClearFilter key ] [ Html.text " X" ]
                ]

        Nothing ->
            case Set.toList values of
                [] ->
                    Html.select [] []

                [ value ] ->
                    Html.select [] [ Html.option [] [ Html.text value ] ]

                values ->
                    ("--" :: values)
                        |> List.map (\v -> Html.option [] [ Html.text v ])
                        |> Html.select [ Events.onInput <| SetFilter key ]


view : Model -> Html Msg
view { facets, filters, pavs, shapeGrid } =
    Html.div []
        [ Html.p [] [ Html.text "algorithm: ", selector filters "algorithmCode" facets.algorithms ]
        , Html.p [] [ Html.text "display name:", selector filters "displayName" facets.displayNames ]
        , Html.p [] [ Html.text "model: ", selector filters "modelScenarioCode" facets.modelScenarios ]
        , Html.p [] [ Html.text "projection: ", selector filters "sdmProjScenarioCode" facets.projectionScenarios ]
        , Html.p [] [ Html.text "kingdom: ", selector filters "taxonKingdom" facets.taxonKingdom ]
        , Html.p [] [ Html.text "phylum: ", selector filters "taxonPhylum" facets.taxonPhylum ]
        , Html.p [] [ Html.text "class: ", selector filters "taxonClass" facets.taxonClass ]
        , Html.p [] [ Html.text "order: ", selector filters "taxonOrder" facets.taxonOrder ]
        , Html.p [] [ Html.text "family: ", selector filters "taxonFamily" facets.taxonFamily ]
        , Html.p [] [ Html.text "genus: ", selector filters "taxonGenus" facets.taxonGenus ]
        , Html.p [] [ Html.text "species: ", selector filters "taxonSpecies" facets.taxonSpecies ]
        , Html.div
            [ Html.Attributes.class "leaflet-map"
            , Html.Attributes.attribute "data-map-pavs" pavs
            , Html.Attributes.attribute "data-map-shape-grid" <| Maybe.withDefault "" <| shapeGrid
            , Html.Attributes.style [ ( "width", "800px" ), ( "height", "800px" ) ]
            ]
            []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSolrList (SolrList pavs) ->
            let
                facets =
                    List.foldr addAttrs initFacets pavs

                pavsJoined =
                    pavs |> List.map (\(SolrPAV { compressedPAV }) -> compressedPAV) |> String.join "\n"
            in
                ( { model | facets = facets, pavs = pavsJoined }, Cmd.none )

        GotShapeGrid shp ->
            ( { model | shapeGrid = Just shp }, Cmd.none )

        SetFilter key value ->
            let
                filters =
                    Dict.insert key value model.filters
            in
                ( { model | filters = filters }, getSolrList filters )

        ClearFilter key ->
            let
                filters =
                    Dict.remove key model.filters
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


getShapeGrid : Cmd Msg
getShapeGrid =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://notyeti-193.lifemapper.org/api/v2/shapegrid/67472/geojson"
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotShapeGrid


gotShapeGrid : Result Http.Error String -> Msg
gotShapeGrid result =
    case result of
        Ok shapeGrid ->
            GotShapeGrid shapeGrid

        Err err ->
            Debug.crash (toString err)


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


init : ( Model, Cmd Msg )
init =
    { facets = initFacets
    , filters = Dict.empty
    , pavs = ""
    , shapeGrid = Nothing
    }
        ! [ getSolrList Dict.empty
          , getShapeGrid
          ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
