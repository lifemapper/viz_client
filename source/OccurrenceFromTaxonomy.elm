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


module OccurrenceFromTaxonomy exposing (Model, Msg, init, getTaxonIds, update, view)

import Maybe.Extra exposing ((?))
import Set exposing (Set)
import Dict exposing (Dict)
import Json.Decode as Json
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes
import Material.Options as Options
import Material.Typography as Typo
import Http
import QueryString as Q
import Decoder exposing (TaxonomyList(..), TaxonomyListItem(..), TaxonomyListItemRecord)
import ProgramFlags exposing (Flags)


type alias Facets =
    { taxonClass : Set String
    , taxonFamily : Set String
    , taxonGenus : Set String
    , taxonKingdom : Set String
    , taxonOrder : Set String
    , taxonPhylum : Set String
    }


initFacets : Facets
initFacets =
    { taxonClass = Set.empty
    , taxonFamily = Set.empty
    , taxonGenus = Set.empty
    , taxonOrder = Set.empty
    , taxonPhylum = Set.empty
    , taxonKingdom =
        Set.fromList
            [ "Animalia"
            , "Chromista"
            , "Fungi"
            , "Plantae"
            , "Protozoa"
            ]
    }


type alias Model =
    { facets : Facets
    , filters : Dict String String
    , selectedSpecies : List String
    , speciesForOccurrences : List TaxonomyListItem
    , selectedSpeciesForOccurrences : List String
    , taxa : List TaxonomyListItem
    , loading : Bool
    , flags : Flags
    }


init : Flags -> Model
init flags =
    { facets = initFacets
    , filters = Dict.empty
    , selectedSpecies = []
    , speciesForOccurrences = []
    , selectedSpeciesForOccurrences = []
    , taxa = []
    , loading = False
    , flags = flags
    }


getTaxonIds : Model -> Decoder.BoomOccurrenceSetTaxon_ids
getTaxonIds model =
    model.speciesForOccurrences
        |> List.filterMap
            (\(TaxonomyListItem { id }) ->
                id
                    |> Maybe.andThen (String.toInt >> Result.toMaybe)
            )
        |> Decoder.BoomOccurrenceSetTaxon_ids


type Msg
    = SetFilter String String
    | ClearFilter String
    | SpeciesSelected (List String)
    | SpeciesForOccurrencesSelected (List String)
    | GotSpecies TaxonomyList
    | TransferSelectedSpecies
    | RemoveSelectedSpecies


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFilter key value ->
            updateFilters (Dict.insert key value model.filters) model

        ClearFilter key ->
            updateFilters (Dict.remove key model.filters) model

        SpeciesSelected keys ->
            ( { model | selectedSpecies = keys }, Cmd.none )

        SpeciesForOccurrencesSelected keys ->
            ( { model | selectedSpeciesForOccurrences = keys }, Cmd.none )

        TransferSelectedSpecies ->
            let
                speciesToAdd =
                    model.taxa
                        |> List.filter
                            (\(TaxonomyListItem { taxon_key }) ->
                                taxon_key
                                    |> Maybe.map (flip List.member model.selectedSpecies)
                                    |> Maybe.withDefault False
                            )
                        |> List.filter (not << flip List.member model.speciesForOccurrences)

                speciesForOccurrences =
                    model.speciesForOccurrences
                        ++ speciesToAdd
                        |> List.sortBy (\(TaxonomyListItem { scientific_name }) -> scientific_name ? "")
            in
                ( { model
                    | speciesForOccurrences = speciesForOccurrences
                    , selectedSpecies = []
                    , selectedSpeciesForOccurrences = model.selectedSpecies
                  }
                , Cmd.none
                )

        RemoveSelectedSpecies ->
            let
                speciesForOccurrences =
                    model.speciesForOccurrences
                        |> List.filter
                            (\(TaxonomyListItem { taxon_key }) ->
                                taxon_key
                                    |> Maybe.map (not << flip List.member model.selectedSpeciesForOccurrences)
                                    |> Maybe.withDefault True
                            )
            in
                ( { model | speciesForOccurrences = speciesForOccurrences }, Cmd.none )

        GotSpecies (TaxonomyList list) ->
            let
                facets =
                    List.foldr addAttrs initFacets list

                taxa =
                    list |> List.sortBy (\(TaxonomyListItem { scientific_name }) -> scientific_name ? "")
            in
                ( { model | facets = facets, taxa = taxa, loading = False }, Cmd.none )


addAttrs : TaxonomyListItem -> Facets -> Facets
addAttrs (TaxonomyListItem item) facets =
    let
        maybeInsert attr facet =
            attr |> Maybe.map (\a -> Set.insert a facet) |> Maybe.withDefault facet
    in
        { taxonClass = maybeInsert item.taxon_class facets.taxonClass
        , taxonFamily = maybeInsert item.taxon_family facets.taxonFamily
        , taxonGenus = maybeInsert item.taxon_genus facets.taxonGenus
        , taxonKingdom = maybeInsert item.taxon_kingdom facets.taxonKingdom
        , taxonOrder = maybeInsert item.taxon_order facets.taxonOrder
        , taxonPhylum = maybeInsert item.taxon_phylum facets.taxonPhylum
        }


updateFilters : Dict String String -> Model -> ( Model, Cmd Msg )
updateFilters filters model =
    ( { model | filters = filters, loading = True, selectedSpecies = [] }, getSpecies model.flags filters )


getSpecies : Flags -> Dict String String -> Cmd Msg
getSpecies flags filters =
    let
        query =
            filters |> Dict.foldr Q.add Q.empty |> Q.render
    in
        Http.request
            { method = "GET"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = flags.apiRoot ++ "taxonomy" ++ query
            , body = Http.emptyBody
            , expect = Http.expectJson Decoder.decodeTaxonomyList
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.send gotSpecies


gotSpecies : Result Http.Error TaxonomyList -> Msg
gotSpecies result =
    case result of
        Ok list ->
            GotSpecies list

        Err err ->
            Debug.crash (toString err)


view : Model -> Html Msg
view ({ facets, filters, loading } as model) =
    Html.div [ Html.Attributes.style [ ( "display", "flex" ) ] ]
        [ Html.div []
            [ Options.styled Html.p [ Typo.subhead ] [ Html.text "Filter available species" ]
            , Html.table []
                [ Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Kingdom: " ]
                    , Html.td [] [ selector loading filters "taxonKingdom" facets.taxonKingdom ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Phylum: " ]
                    , Html.td [] [ selector loading filters "taxonPhylum" facets.taxonPhylum ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Class: " ]
                    , Html.td [] [ selector loading filters "taxonClass" facets.taxonClass ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Order: " ]
                    , Html.td [] [ selector loading filters "taxonOrder" facets.taxonOrder ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Family: " ]
                    , Html.td [] [ selector loading filters "taxonFamily" facets.taxonFamily ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Genus: " ]
                    , Html.td [] [ selector loading filters "taxonGenus" facets.taxonGenus ]
                    ]
                ]
            ]
        , Html.div [ Html.Attributes.style [ ( "margin-left", "20px" ) ] ]
            [ Options.styled Html.p [ Typo.subhead ] [ Html.text "Matching species" ]
            , Html.select
                [ Html.Attributes.style [ ( "height", "400px" ), ( "width", "600px" ) ]
                , Html.Attributes.multiple True
                , Events.on "change" (itemsSelected SpeciesSelected)
                ]
                (List.map (displayName model.selectedSpecies) model.taxa)
            ]
        , Html.div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "justify-content", "space-around" )
                , ( "margin", "20px" )
                ]
            ]
            [ Html.button [ Events.onClick TransferSelectedSpecies ] [ Html.text ">" ]
            , Html.button [ Events.onClick RemoveSelectedSpecies ] [ Html.text "<" ]
            ]
        , Html.div []
            [ Html.p []
                [ Options.span [ Typo.subhead ] [ Html.text "Selected species" ]
                , Html.text " (occurrence points for these species will be downloaded from iDigBio)"
                ]
            , Html.select
                [ Html.Attributes.style [ ( "height", "400px" ), ( "width", "600px" ) ]
                , Html.Attributes.multiple True
                , Events.on "change" (itemsSelected SpeciesForOccurrencesSelected)
                ]
                (List.map (displayName model.selectedSpeciesForOccurrences) model.speciesForOccurrences)
            ]
        ]


itemsSelected : (List String -> msg) -> Json.Decoder msg
itemsSelected msg =
    (Json.at [ "target", "selectedOptions" ] <| Json.keyValuePairs <| Json.maybe (Json.field "value" Json.string))
        |> Json.map (List.filterMap Tuple.second)
        |> Json.map msg


displayName : List String -> TaxonomyListItem -> Html Msg
displayName selectedSpecies (TaxonomyListItem { scientific_name, taxon_key }) =
    let
        key =
            taxon_key |> Maybe.withDefault ""

        selected =
            List.member key selectedSpecies
    in
        Html.option [ Html.Attributes.value key, Html.Attributes.selected selected ]
            [ Html.text <| Maybe.withDefault "" scientific_name ]


selector : Bool -> Dict String String -> String -> Set String -> Html Msg
selector loading filters key values =
    case Dict.get key filters of
        Just value ->
            Html.span []
                [ Html.button [ Events.onClick <| ClearFilter key, Html.Attributes.disabled loading ] [ Html.text "X" ]
                , Html.text (" " ++ value)
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
                        |> Html.select [ Events.onInput <| SetFilter key, Html.Attributes.disabled loading ]
