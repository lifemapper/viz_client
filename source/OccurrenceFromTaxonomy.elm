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
import List.Extra as List
import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode as Encode
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes
import Material.Options as Options
import Material.Typography as Typo
import Http
import QueryString as Q
import ProgramFlags exposing (Flags)
import Decoder


type TaxonomyList
    = TaxonomyList (List TaxonomyListItem)


type TaxonomyListItem
    = TaxonomyListItem { taxon_key : String, scientific_name : String }


type TaxonRank
    = TaxonKingdom
    | TaxonPhylum
    | TaxonClass
    | TaxonOrder
    | TaxonFamily
    | TaxonGenus
    | TaxonSpecies


ranks : List TaxonRank
ranks =
    [ TaxonKingdom
    , TaxonPhylum
    , TaxonClass
    , TaxonOrder
    , TaxonFamily
    , TaxonGenus
    , TaxonSpecies
    ]


rankString : TaxonRank -> String
rankString rank =
    case rank of
        TaxonKingdom ->
            "taxon_kingdom"

        TaxonPhylum ->
            "taxon_phylum"

        TaxonClass ->
            "taxon_class"

        TaxonOrder ->
            "taxon_order"

        TaxonFamily ->
            "taxon_family"

        TaxonGenus ->
            "taxon_genus"

        TaxonSpecies ->
            "taxon_species"


type alias Model =
    { filters : Dict String String
    , options : Dict String (List String)
    , rank : Maybe TaxonRank
    , selectedSpecies : List String
    , speciesForOccurrences : List TaxonomyListItem
    , selectedSpeciesForOccurrences : List String
    , taxa : List TaxonomyListItem
    , speciesFound : Int
    , loading : Bool
    , flags : Flags
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { filters = Dict.empty
    , options = Dict.empty
    , rank = Just TaxonKingdom
    , selectedSpecies = []
    , speciesForOccurrences = []
    , selectedSpeciesForOccurrences = []
    , taxa = []
    , speciesFound = 0
    , loading = False
    , flags = flags
    }
        ! [ getFacets flags Dict.empty TaxonKingdom ]


getTaxonIds : Model -> Decoder.BoomOccurrenceSetTaxon_ids
getTaxonIds model =
    model.speciesForOccurrences
        |> List.filterMap (\(TaxonomyListItem { taxon_key }) -> taxon_key |> String.toInt |> Result.toMaybe)
        |> Decoder.BoomOccurrenceSetTaxon_ids


type Msg
    = SetFilter TaxonRank String
    | ClearFilter TaxonRank
    | SpeciesSelected (List String)
    | SpeciesForOccurrencesSelected (List String)
    | TransferSelectedSpecies
    | RemoveSelectedSpecies
    | GotFacets TaxonRank Int (List ( String, String )) (List ( String, Int ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFacets facetField speciesFound species facets ->
            let
                ( higher, lower ) =
                    List.splitWhen ((==) facetField) ranks ? ( [], [] )

                rank =
                    lower |> List.getAt 1

                options =
                    List.foldl (\rank -> Dict.remove (rankString rank)) model.options lower
                        |> Dict.insert (rankString facetField) (List.map Tuple.first facets)

                taxa =
                    species
                        |> List.sortBy Tuple.second
                        |> List.map
                            (\( taxon_key, scientific_name ) ->
                                TaxonomyListItem { taxon_key = taxon_key, scientific_name = scientific_name }
                            )
            in
                ( { model | rank = rank, options = options, taxa = taxa, speciesFound = speciesFound, loading = False }, Cmd.none )

        SetFilter rank value ->
            let
                ( higher, lower ) =
                    List.splitWhen ((==) rank) ranks ? ( [], [] )

                filters =
                    List.foldl (\rank -> Dict.remove (rankString rank)) model.filters lower
                        |> Dict.insert (rankString rank) value
            in
                updateFilters filters model

        ClearFilter rank ->
            let
                ( higher, lower ) =
                    List.splitWhen ((==) rank) ranks ? ( [], [] )

                filters =
                    List.foldl (\rank -> Dict.remove (rankString rank)) model.filters lower
            in
                updateFilters filters { model | rank = Just rank }

        SpeciesSelected keys ->
            ( { model | selectedSpecies = keys }, Cmd.none )

        SpeciesForOccurrencesSelected keys ->
            ( { model | selectedSpeciesForOccurrences = keys }, Cmd.none )

        TransferSelectedSpecies ->
            let
                speciesToAdd =
                    model.taxa
                        |> List.filter
                            (\(TaxonomyListItem { taxon_key }) -> List.member taxon_key model.selectedSpecies)
                        |> List.filter (not << flip List.member model.speciesForOccurrences)

                speciesForOccurrences =
                    model.speciesForOccurrences
                        ++ speciesToAdd
                        |> List.sortBy (\(TaxonomyListItem { scientific_name }) -> scientific_name)
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
                            (\(TaxonomyListItem { taxon_key }) -> not <| List.member taxon_key model.selectedSpeciesForOccurrences)
            in
                ( { model | speciesForOccurrences = speciesForOccurrences }, Cmd.none )


updateFilters : Dict String String -> Model -> ( Model, Cmd Msg )
updateFilters filters model =
    ( { model | filters = filters, loading = True, selectedSpecies = [] }
    , Maybe.map (getFacets model.flags filters) model.rank ? Cmd.none
    )


type FacetCount
    = FacetField String
    | FacetCount Int


type alias SolrResponse =
    { facetCounts : List ( String, List FacetCount )
    , numFound : Int
    , species : List ( String, String )
    }


getFacets : Flags -> Dict String String -> TaxonRank -> Cmd Msg
getFacets flags filters facetField =
    let
        q =
            if filters |> Dict.isEmpty then
                "*:*"
            else
                filters |> Dict.toList |> List.map (\( k, v ) -> k ++ ":" ++ v) |> String.join " AND "

        query =
            Q.empty
                |> Q.add "q" q
                |> Q.add "facet" "on"
                |> Q.add "facet.field" (rankString facetField)
                |> Q.add "facet.sort" "index"
                |> Q.add "facet.mincount" "1"
                |> Q.add "facet.limit" "-1"
                |> Q.add "wt" "json"
                |> Q.add "rows" "1000"
                |> Q.render
                |> String.dropLeft 1
                -- remove the leading ?
                |>
                    Encode.string
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = flags.apiRoot ++ "rawsolr"
            , body = Http.jsonBody <| Encode.object [ ( "collection", Encode.string "taxonomy" ), ( "query_string", query ) ]
            , expect =
                Http.expectJson <|
                    Json.map3 SolrResponse
                        (Json.at [ "facet_counts", "facet_fields" ] <|
                            Json.keyValuePairs <|
                                Json.list <|
                                    Json.oneOf [ Json.map FacetField Json.string, Json.map FacetCount Json.int ]
                        )
                        (Json.at [ "response", "numFound" ] Json.int)
                        (Json.at [ "response", "docs" ] <|
                            Json.list <|
                                Json.map2 (,)
                                    (Json.field "taxon_key" Json.string)
                                    (Json.field "scientific_name" Json.string)
                        )
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.send (gotFacets facetField)


gotFacets : TaxonRank -> Result Http.Error SolrResponse -> Msg
gotFacets facetField result =
    case result of
        Ok { facetCounts, numFound, species } ->
            case facetCounts of
                [ ( rank, facetCounts ) ] ->
                    facetCounts
                        |> List.groupsOf 2
                        |> List.filterMap
                            (\group ->
                                case group of
                                    [ FacetField s, FacetCount c ] ->
                                        if c > 0 then
                                            Just ( s, c )
                                        else
                                            Nothing

                                    _ ->
                                        Debug.log "Unexpected facet counts structure" group
                                            |> always Nothing
                            )
                        |> List.filter (\( s, _ ) -> s /= "Sedis" && s /= "N")
                        |> GotFacets facetField numFound species

                _ ->
                    Debug.crash "bad solr response" facetCounts

        _ ->
            Debug.crash "problem getting taxonomy facets" result


view : Model -> Html Msg
view ({ options, filters, loading } as model) =
    Html.div [ Html.Attributes.style [ ( "display", "flex" ) ] ]
        [ Html.div []
            [ Options.styled Html.p [ Typo.subhead ] [ Html.text "Filter available species" ]
            , Html.table []
                (ranks
                    |> List.remove TaxonSpecies
                    |> List.map
                        (\rank ->
                            Html.tr []
                                [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ]
                                    [ toString rank ++ ": " |> String.dropLeft 5 |> Html.text ]
                                , Html.td [] [ selector loading filters rank <| Dict.get (rankString rank) options ? [] ]
                                ]
                        )
                )
            ]
        , Html.div [ Html.Attributes.style [ ( "margin-left", "20px" ) ] ]
            [ Options.styled Html.p [ Typo.subhead, Options.css "margin" "0" ] [ Html.text "Matching species" ]
            , Html.p []
                [ Html.text <| " (found " ++ (toString model.speciesFound) ++ ")" ]
            , if model.speciesFound > List.length model.taxa then
                Html.textarea
                    [ Html.Attributes.style [ ( "height", "400px" ), ( "width", "300px" ) ]
                    , Html.Attributes.readonly True
                    ]
                    [ Html.text "Too many matching species. Continue narrowing search using filters." ]
              else
                Html.select
                    [ Html.Attributes.style [ ( "height", "400px" ), ( "width", "300px" ), ( "overflow", "auto" ) ]
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
            [ Options.styled Html.p [ Typo.subhead, Options.css "margin" "0" ] [ Html.text "Selected species" ]
            , Html.p [] [ Html.text " (occurrence points from iDigBio)" ]
            , Html.select
                [ Html.Attributes.style [ ( "height", "400px" ), ( "width", "300px" ), ( "overflow", "auto" ) ]
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
        selected =
            List.member taxon_key selectedSpecies
    in
        Html.option [ Html.Attributes.value taxon_key, Html.Attributes.selected selected ]
            [ Html.text scientific_name ]


selector : Bool -> Dict String String -> TaxonRank -> List String -> Html Msg
selector loading filters rank values =
    case Dict.get (rankString rank) filters of
        Just value ->
            Html.span []
                [ Html.button [ Events.onClick <| ClearFilter rank, Html.Attributes.disabled loading ] [ Html.text "X" ]
                , Html.text (" " ++ value)
                ]

        Nothing ->
            case values of
                [] ->
                    Html.select [] []

                [ value ] ->
                    Html.select [] [ Html.option [] [ Html.text value ] ]

                values ->
                    ("--" :: values)
                        |> List.map (\v -> Html.option [] [ Html.text v ])
                        |> Html.select [ Events.onInput <| SetFilter rank, Html.Attributes.disabled loading ]
