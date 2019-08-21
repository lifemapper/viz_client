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
import Material
import Material.Button as Button
import Material.Options as Options
import Material.Typography as Typo
import Material.Spinner as Loading
import Material.Tooltip as Tooltip
import Material.Icon as Icon
import Material.Toggles as Toggles
import Helpers exposing (Index)
import Http
import QueryString as Q
import ProgramFlags exposing (Flags)
import Decoder
import Encoder


type TaxonomyList
    = TaxonomyList (List TaxonomyListItem)


type TaxonomyListItem
    = TaxonomyListItem
        { taxon_key : String
        , scientific_name : String
        , use : Bool
        , count : Maybe Int
        , inTree : Maybe Bool
        }


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
            "scientific_name"


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
    , state : State
    , flags : Flags
    }


type State
    = SelectingSpecies
    | RequestingPoints
    | RequestingTree
    | GotMatches (List TaxonomyListItem)


type alias Match =
    { name : Maybe String
    , taxonId : Maybe Int
    , use : Bool
    , count : Maybe Int
    , inTree : Maybe Bool
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
    , state = SelectingSpecies
    , flags = flags
    }
        ! [ getFacets flags Dict.empty TaxonKingdom ]


getTaxonIds : Model -> List Int
getTaxonIds model =
    case model.state of
        GotMatches items ->
            items
                |> List.filterMap
                    (\(TaxonomyListItem { taxon_key, use }) ->
                        if use then
                            taxon_key |> String.toInt |> Result.toMaybe
                        else
                            Nothing
                    )

        _ ->
            []


type Msg
    = SetFilter TaxonRank String
    | ClearFilter TaxonRank
    | SpeciesSelected (List String)
    | SpeciesForOccurrencesSelected (List String)
    | TransferSelectedSpecies
    | RemoveSelectedSpecies
    | GotFacets TaxonRank Int (List ( String, String )) (List ( String, Int ))
    | CheckTaxa
    | GotPointsMsg (List TaxonomyListItem)
    | GotTreeMsg (List TaxonomyListItem)
    | GoBack
    | ToggleUseName Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleUseName i ->
            case model.state of
                GotMatches matches ->
                    let
                        matches_ =
                            matches
                                |> List.indexedMap
                                    (\j (TaxonomyListItem match) ->
                                        if j == i then
                                            TaxonomyListItem { match | use = not match.use }
                                        else
                                            TaxonomyListItem match
                                    )
                    in
                        { model | state = GotMatches matches_ } ! []

                _ ->
                    model ! []

        GoBack ->
            { model | state = SelectingSpecies } ! []

        CheckTaxa ->
            { model | state = RequestingPoints } ! [ requestPoints model.flags model.speciesForOccurrences ]

        GotPointsMsg items ->
            { model | state = RequestingTree } ! [ requestTree model.flags items ]

        GotTreeMsg items ->
            { model | state = GotMatches items } ! []

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
                                TaxonomyListItem
                                    { taxon_key = taxon_key
                                    , scientific_name = scientific_name
                                    , count = Nothing
                                    , inTree = Nothing
                                    , use = False
                                    }
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
    , if model.rank == Just TaxonSpecies then
        Cmd.none
      else
        Maybe.map (getFacets model.flags filters) model.rank ? Cmd.none
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


requestPoints : Flags -> List TaxonomyListItem -> Cmd Msg
requestPoints flags matches =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = flags.apiRoot ++ "biotaphypoints"
        , body =
            matches
                |> List.filterMap (\(TaxonomyListItem { taxon_key }) -> String.toInt taxon_key |> Result.toMaybe)
                |> Decoder.BiotaphyPointsPost
                |> Encoder.encodeBiotaphyPointsPost
                |> Http.jsonBody
        , expect = Http.expectJson Decoder.decodeBiotaphyPointsResponse
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (gotBiotaphyPointsResponse matches)


gotBiotaphyPointsResponse : List TaxonomyListItem -> Result Http.Error Decoder.BiotaphyPointsResponse -> Msg
gotBiotaphyPointsResponse matches response =
    case response of
        Ok (Decoder.BiotaphyPointsResponse items) ->
            matches
                |> List.map
                    (\(TaxonomyListItem match) ->
                        items
                            |> List.map (\(Decoder.BiotaphyPointsResponseItem item) -> item)
                            |> List.filter (\{ taxon_id } -> match.taxon_key == toString taxon_id)
                            |> List.head
                            |> Maybe.map (\{ count } -> { match | count = Just count })
                            |> Maybe.withDefault match
                            |> TaxonomyListItem
                    )
                |> GotPointsMsg

        Err err ->
            matches |> List.map (\(TaxonomyListItem item) -> TaxonomyListItem { item | count = Nothing }) |> GotPointsMsg


requestTree : Flags -> List TaxonomyListItem -> Cmd Msg
requestTree flags matches =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = flags.apiRoot ++ "opentree"
        , body =
            matches
                |> List.filterMap (\(TaxonomyListItem { taxon_key }) -> String.toInt taxon_key |> Result.toMaybe)
                |> Decoder.OpenTreePOST
                |> Encoder.encodeOpenTreePOST
                |> Http.jsonBody
        , expect = Http.expectJson Decoder.decodeOpenTreePOSTresponse
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (gotOpenTreeResponse matches)


gotOpenTreeResponse : List TaxonomyListItem -> Result Http.Error Decoder.OpenTreePOSTresponse -> Msg
gotOpenTreeResponse matches response =
    case response of
        Ok (Decoder.OpenTreePOSTresponse { unmatched_ids }) ->
            let
                (Decoder.OpenTreePOSTresponseUnmatched_ids unmatchedIds) =
                    unmatched_ids

                inTree match =
                    case String.toInt match.taxon_key |> Result.toMaybe of
                        Just id ->
                            not <| List.member id unmatchedIds

                        Nothing ->
                            False
            in
                matches
                    |> List.map (\(TaxonomyListItem match) -> TaxonomyListItem { match | inTree = Just (inTree match) })
                    |> GotTreeMsg

        Err err ->
            matches
                |> List.map (\(TaxonomyListItem match) -> TaxonomyListItem { match | inTree = Nothing })
                |> GotTreeMsg


view : (Material.Msg msg -> msg) -> (Msg -> msg) -> Index -> Material.Model -> Model -> Html msg
view mdlMsg mapMsg index mdl model =
    case model.state of
        SelectingSpecies ->
            selectSpeciesView mdlMsg mapMsg index mdl model

        RequestingPoints ->
            Options.div []
                [ Options.styled Html.p [ Typo.title ] [ Html.text "Requesting occurence points from iDigBio..." ]
                , Loading.spinner [ Loading.active True ]
                ]

        RequestingTree ->
            Options.div []
                [ Options.styled Html.p [ Typo.title ] [ Html.text "Requesting tree from Open Tree..." ]
                , Loading.spinner [ Loading.active True ]
                ]

        GotMatches matches ->
            Options.div []
                [ Options.styled Html.p [ Typo.title ] [ Html.text "Data provider results:" ]
                , matches
                    |> List.indexedMap (viewMatch mdlMsg mapMsg (3 :: index) mdl)
                    |> (++)
                        [ Html.tr []
                            [ Html.th [] [ Html.text "Searched" ]
                            , Html.th [] [ Html.text "iDigBio" ]
                            , Html.th [] [ Html.text "Open Tree" ]
                            , Html.th [] [ Html.text "Include in Project" ]
                            ]
                        ]
                    |> Html.table [ Html.Attributes.style [ ( "width", "500px" ) ] ]
                , Button.render mdlMsg
                    (2 :: index)
                    mdl
                    [ Button.raised
                    , Options.css "margin" "5px"
                    , Options.onClick <| mapMsg GoBack
                    ]
                    [ Html.text "Go Back" ]
                ]


selectSpeciesView : (Material.Msg msg -> msg) -> (Msg -> msg) -> Index -> Material.Model -> Model -> Html msg
selectSpeciesView mdlMsg mapMsg index mdl ({ options, filters, loading } as model) =
    Html.div []
        [ Html.div [ Html.Attributes.style [ ( "display", "flex" ) ] ]
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
                                    , Html.td []
                                        [ Html.map mapMsg <|
                                            selector loading filters rank <|
                                                Dict.get (rankString rank) options
                                                    ? []
                                        ]
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
                        |> Html.map mapMsg
                ]
            , Html.div
                [ Html.Attributes.style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    , ( "justify-content", "space-around" )
                    , ( "margin", "100px 20px 0 20px" )
                    , ( "height", "100px" )
                    ]
                ]
                [ Html.button [ Events.onClick TransferSelectedSpecies ] [ Html.text ">" ] |> Html.map mapMsg
                , Html.button [ Events.onClick RemoveSelectedSpecies ] [ Html.text "<" ] |> Html.map mapMsg
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
                    |> Html.map mapMsg
                ]
            ]
        , Html.p []
            [ Button.render mdlMsg
                (1 :: index)
                mdl
                [ Button.raised
                , Options.onClick <| mapMsg CheckTaxa
                , Options.disabled (List.length model.speciesForOccurrences < 1)
                ]
                [ Html.text "Match" ]
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


viewMatch : (Material.Msg msg -> msg) -> (Msg -> msg) -> Index -> Material.Model -> Int -> TaxonomyListItem -> Html msg
viewMatch mdlMsg mapMsg index mdl i (TaxonomyListItem item) =
    let
        checkbox =
            case item.count of
                Just 0 ->
                    []

                Just 1 ->
                    []

                Nothing ->
                    []

                _ ->
                    [ Toggles.checkbox mdlMsg
                        (i :: index)
                        mdl
                        [ Options.onToggle <| mapMsg <| ToggleUseName i
                        , Toggles.value item.use
                        , Options.css "margin-left" "40px"
                        ]
                        []
                    ]

        inTree =
            case item.inTree of
                Just True ->
                    [ Icon.i "done" ]

                Just False ->
                    [ Html.text "" ]

                Nothing ->
                    [ Icon.view "error_outline" [ Tooltip.attach mdlMsg (0 :: i :: index) ]
                    , Tooltip.render mdlMsg
                        (0 :: i :: index)
                        mdl
                        []
                        [ Html.text "There was a problem accessing the Open Tree API." ]
                    ]

        itemCount =
            case item.count of
                Just n ->
                    [ n |> toString |> Html.text ]

                Nothing ->
                    [ Icon.view "error_outline" [ Tooltip.attach mdlMsg (0 :: i :: index) ]
                    , Tooltip.render mdlMsg
                        (0 :: i :: index)
                        mdl
                        []
                        [ Html.text "There was a problem accessing the iDigBio API." ]
                    ]
    in
        Html.tr []
            [ Html.td [ Html.Attributes.style [ ( "white-space", "nowrap" ) ] ] [ Html.text item.scientific_name ]
            , Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] itemCount
            , Html.td [ Html.Attributes.style [ ( "text-align", "center" ) ] ] inTree
            , Html.td [] checkbox
            ]
