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


port module SubsetPam exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Json.Decode as Json
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes
import Http
import QueryString as Q
import Decoder exposing (SolrList(..), SolrPAV(..), SolrPAVRecord)
import ProgramFlags exposing (Flags)


port bboxSelected : (List Float -> msg) -> Sub msg


type alias Facets =
    { algorithms : Set String
    , display_names : Set ( String, String )
    , model_scenarios : Set String
    , projection_scenarios : Set String
    , taxon_class : Set String
    , taxon_family : Set String
    , taxon_genus : Set String
    , taxon_kingdom : Set String
    , taxon_order : Set String
    , taxon_phylum : Set String
    , taxon_species : Set String
    }


initFacets : Facets
initFacets =
    { algorithms = Set.empty
    , display_names = Set.empty
    , model_scenarios = Set.empty
    , projection_scenarios = Set.empty
    , taxon_class = Set.empty
    , taxon_family = Set.empty
    , taxon_genus = Set.empty
    , taxon_order = Set.empty
    , taxon_phylum = Set.empty
    , taxon_species = Set.empty
    , taxon_kingdom =
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
    , selectedSpecies : Maybe (List String)
    , pavs : List SolrPAV
    , shapegrid : Maybe String
    , loadingPavs : Bool
    , archive_name : String
    , postStatus : PostStatus
    , flags : Flags
    , gridset_id : Int
    }


type PostStatus
    = NotPosted
    | Posted
    | PostFinished
    | PostFailed
    | Processing { total : Int, completed : Int, id : Int }


type Msg
    = Nop
    | GotPamGridSetId String
    | GotPamGridSet { id : Int, shapegridId : Int }
    | GotSolrList SolrList
    | GotShapeGrid String
    | SetFilter String String
    | ClearFilter String
    | SpeciesSelected (List String)
    | SetArchiveName String
    | BBoxSelected (List Float)
    | RunMCPA
    | GotPostResponse (Result Http.Error Decoder.AtomObject)
    | StatusUpdate Decoder.Gridset


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


view : Model -> Html Msg
view model =
    case model.postStatus of
        NotPosted ->
            viewNotPosted model

        Posted ->
            Html.div [ Html.Attributes.style [ ( "font-family", "sans-serif" ), ( "text-align", "center" ) ] ]
                [ Html.text "Request sent. Waiting for response..." ]

        PostFinished ->
            Html.div [ Html.Attributes.style [ ( "font-family", "sans-serif" ), ( "text-align", "center" ) ] ]
                [ Html.text "Request accepted. Waiting for status updates..." ]

        PostFailed ->
            Html.div [ Html.Attributes.style [ ( "font-family", "sans-serif" ), ( "text-align", "center" ) ] ]
                [ Html.text "Server encountered an error processing request." ]

        Processing { id, total, completed } ->
            if total /= completed then
                Html.div [ Html.Attributes.style [ ( "font-family", "sans-serif" ), ( "text-align", "center" ) ] ]
                    [ Html.text <| "Processing request (" ++ (toString completed) ++ "/" ++ (toString total) ++ ")..." ]
            else
                Html.div [ Html.Attributes.style [ ( "font-family", "sans-serif" ), ( "text-align", "center" ) ] ]
                    [ Html.text "Processing complete. "
                    , Html.a
                        [ Html.Attributes.href <|
                            model.flags.apiRoot
                                ++ "gridset/"
                                ++ (toString id)
                                ++ "/package"
                        ]
                        [ Html.text "Download results." ]
                    ]


header : Bool -> Html msg
header loadingPavs =
    if loadingPavs then
        Html.text "Subset PAM with these filters (updating...)"
    else
        Html.text "Subset PAM with these filters"


speciesSelected : Json.Decoder Msg
speciesSelected =
    (Json.at [ "target", "selectedOptions" ] <| Json.keyValuePairs <| Json.maybe (Json.field "value" Json.string))
        |> Json.map (List.filterMap Tuple.second)
        |> Json.map SpeciesSelected


viewNotPosted : Model -> Html Msg
viewNotPosted { facets, filters, selectedSpecies, pavs, shapegrid, loadingPavs, archive_name } =
    Html.div [ Html.Attributes.style [ ( "font-family", "sans-serif" ), ( "display", "flex" ), ( "justify-content", "space-around" ) ] ]
        [ Html.div []
            [ Html.h3 [] [ header loadingPavs ]
            , Html.table [ Html.Attributes.style [ ( "width", "800px" ) ] ]
                [ Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Kingdom: " ]
                    , Html.td [] [ selector loadingPavs filters "taxon_kingdom" facets.taxon_kingdom ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Phylum: " ]
                    , Html.td [] [ selector loadingPavs filters "taxon_phylum" facets.taxon_phylum ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Class: " ]
                    , Html.td [] [ selector loadingPavs filters "taxon_class" facets.taxon_class ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Order: " ]
                    , Html.td [] [ selector loadingPavs filters "taxon_order" facets.taxon_order ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Family: " ]
                    , Html.td [] [ selector loadingPavs filters "taxon_family" facets.taxon_family ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Genus: " ]
                    , Html.td [] [ selector loadingPavs filters "taxon_genus" facets.taxon_genus ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Species: " ]
                    , Html.td [] [ selector loadingPavs filters "taxon_species" facets.taxon_species ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ), ( "width", "20px" ) ] ] [ Html.text "Algorithm: " ]
                    , Html.td [] [ selector loadingPavs filters "algorithm_code" facets.algorithms ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Model: " ]
                    , Html.td [] [ selector loadingPavs filters "model_scenario_code" facets.model_scenarios ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Projection: " ]
                    , Html.td [] [ selector loadingPavs filters "sdm_proj_scenario_code" facets.projection_scenarios ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "B-Box: " ]
                    , Html.td []
                        [ Html.input
                            [ Events.onInput (SetFilter "bbox")
                            , Html.Attributes.value (Dict.get "bbox" filters |> Maybe.withDefault "")
                            , Html.Attributes.readonly loadingPavs
                            , Html.Attributes.placeholder "minx,miny,maxx,maxy"
                            ]
                            []
                        ]
                    ]
                ]
            , Html.h3 [] [ Html.text "Matching species" ]
            , Html.select
                [ Html.Attributes.style [ ( "height", "400px" ), ( "width", "800px" ) ]
                , Html.Attributes.multiple True
                , Events.on "change" speciesSelected
                ]
                (List.map (display_name selectedSpecies) <| Set.toList facets.display_names)
            , Html.div []
                [ Html.input
                    [ Html.Attributes.placeholder "Archive name"
                    , Html.Attributes.style [ ( "margin-right", "5px" ) ]
                    , Events.onInput SetArchiveName
                    , Html.Attributes.value archive_name
                    ]
                    []
                , Html.button
                    [ Events.onClick RunMCPA
                    , Html.Attributes.disabled (loadingPavs || (archive_name == ""))
                    ]
                    [ Html.text "Subset PAM" ]
                ]
            ]
        , Html.div []
            [ Html.h3 [] [ Html.text "Heat map of matching species" ]
            , Html.div
                [ Html.Attributes.class "leaflet-map"
                , Html.Attributes.attribute "data-map-pavs" <| filterAndJoinPavs selectedSpecies pavs
                , Html.Attributes.attribute "data-map-shape-grid" <| Maybe.withDefault "" <| shapegrid
                , Html.Attributes.style [ ( "width", "800px" ), ( "height", "800px" ) ]
                ]
                []
            ]
        ]


filterAndJoinPavs : Maybe (List String) -> List SolrPAV -> String
filterAndJoinPavs selectedSpecies pavs =
    let
        filterBySpecies =
            case selectedSpecies of
                Just squids ->
                    List.filter (\(SolrPAV { squid }) -> List.member squid squids)

                Nothing ->
                    identity
    in
        pavs
            |> filterBySpecies
            |> List.map (\(SolrPAV { compressed_pav }) -> compressed_pav)
            |> String.join "\n"


display_name : Maybe (List String) -> ( String, String ) -> Html Msg
display_name selectedSpecies ( name, squid ) =
    let
        selected =
            Maybe.map (List.member squid) selectedSpecies |> Maybe.withDefault False
    in
        Html.option [ Html.Attributes.value squid, Html.Attributes.selected selected ]
            [ Html.text name ]


updateFilters : Dict String String -> Model -> ( Model, Cmd Msg )
updateFilters filters model =
    -- if Dict.member "taxon_kingdom" filters then
    ( { model | filters = filters, loadingPavs = True, selectedSpecies = Nothing }
    , getSolrList model.flags model.gridset_id filters
    )



-- else
--     ( { model | facets = initFacets, filters = filters }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        GotPamGridSetId id ->
            ( model, getPamGridSet model.flags id )

        GotPamGridSet { id, shapegridId } ->
            { model | gridset_id = id } ! [ getShapeGrid model.flags shapegridId, getSolrList model.flags id Dict.empty ]

        GotSolrList (SolrList pavs) ->
            let
                facets =
                    List.foldr addAttrs initFacets pavs
            in
                ( { model | facets = facets, pavs = pavs, loadingPavs = False }, Cmd.none )

        GotShapeGrid shp ->
            ( { model | shapegrid = Just shp }, Cmd.none )

        SetFilter key value ->
            updateFilters (Dict.insert key value model.filters) model

        ClearFilter key ->
            updateFilters (Dict.remove key model.filters) model

        SpeciesSelected squids ->
            ( { model | selectedSpecies = Just squids }, Cmd.none )

        SetArchiveName name ->
            ( { model | archive_name = String.trim name }, Cmd.none )

        BBoxSelected bbox ->
            updateFilters (Dict.insert "bbox" (bbox |> List.map toString |> String.join ",") model.filters) model

        RunMCPA ->
            ( { model | postStatus = Posted }
            , runMCPA model.flags
                model.gridset_id
                model.archive_name
                model.filters
                (model.selectedSpecies |> Maybe.withDefault [])
            )

        GotPostResponse result ->
            case result of
                Ok (Decoder.AtomObject { id }) ->
                    ( { model | postStatus = PostFinished }, checkStatus model.flags id )

                Err err ->
                    Debug.log "Post failed" err
                        |> always ( { model | postStatus = PostFailed }, Cmd.none )

        StatusUpdate (Decoder.Gridset { id, matrices }) ->
            let
                (Decoder.GridsetMatrices ms) =
                    matrices

                total =
                    ms |> List.length

                completed =
                    ms |> List.filter (\(Decoder.Matrix { status }) -> status == Just 300) |> List.length

                postStatus =
                    Processing { completed = completed, total = total, id = id }

                cmd =
                    if completed == total then
                        Cmd.none
                    else
                        checkStatus model.flags id
            in
                ( { model | postStatus = postStatus }, cmd )


addAttrs : SolrPAV -> Facets -> Facets
addAttrs (SolrPAV pav) facets =
    let
        maybeInsert attr facet =
            attr |> Maybe.map (\a -> Set.insert a facet) |> Maybe.withDefault facet
    in
        { algorithms = maybeInsert pav.algorithm_code facets.algorithms
        , display_names = Set.insert ( pav.display_name, pav.squid ) facets.display_names
        , model_scenarios = maybeInsert pav.model_scenario_code facets.model_scenarios
        , projection_scenarios = maybeInsert pav.sdm_proj_scenario_code facets.projection_scenarios
        , taxon_kingdom = maybeInsert pav.taxon_kingdom facets.taxon_kingdom
        , taxon_phylum = maybeInsert pav.taxon_phylum facets.taxon_phylum
        , taxon_class = maybeInsert pav.taxon_class facets.taxon_class
        , taxon_order = maybeInsert pav.taxon_order facets.taxon_order
        , taxon_family = maybeInsert pav.taxon_family facets.taxon_family
        , taxon_genus = maybeInsert pav.taxon_genus facets.taxon_genus
        , taxon_species = maybeInsert pav.taxon_species facets.taxon_species
        }


getPamGridSet : Flags -> String -> Cmd Msg
getPamGridSet flags id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = flags.apiRoot ++ "gridset/" ++ id
        , body = Http.emptyBody
        , expect =
            Http.expectJson <|
                Json.map2 (,) (Json.field "id" Json.int) (Json.field "shapegridId" Json.int)
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotPamGridSet


gotPamGridSet : Result Http.Error ( Int, Int ) -> Msg
gotPamGridSet result =
    case result of
        Err err ->
            Debug.crash (toString err)

        Ok ( id, shapegridId ) ->
            GotPamGridSet { id = id, shapegridId = shapegridId }


getPamGridSets : Flags -> Cmd Msg
getPamGridSets flags =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = flags.apiRoot ++ "globalpam/gridset"
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (Json.field "gridset_id" <|
                    Json.list <|
                        Json.map2 (,)
                            (Json.field "gridset_id" Json.string)
                            (Json.field "count" Json.int)
                )
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotPamGridSets


gotPamGridSets : Result Http.Error (List ( String, Int )) -> Msg
gotPamGridSets result =
    case result of
        Err err ->
            Debug.crash (toString err)

        Ok [] ->
            Debug.crash "no global pam gridsets available"

        Ok gridsets ->
            let
                gridsetid =
                    gridsets
                        |> List.sortBy (Tuple.second >> (*) -1)
                        |> List.head
                        |> Maybe.map Tuple.first
            in
                case gridsetid of
                    Just id ->
                        GotPamGridSetId id

                    Nothing ->
                        Debug.crash "this shouldn't happen"


checkStatus : Flags -> Int -> Cmd Msg
checkStatus flags id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = flags.apiRoot ++ "gridset/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeGridset
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotGridSet


gotGridSet : Result Http.Error Decoder.Gridset -> Msg
gotGridSet result =
    case result of
        Ok gridSet ->
            StatusUpdate gridSet

        Err err ->
            Debug.crash (toString err)


getShapeGrid : Flags -> Int -> Cmd Msg
getShapeGrid flags shapegridId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = flags.apiRoot ++ "shapegrid/" ++ (toString shapegridId) ++ "/geojson"
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotShapeGrid


gotShapeGrid : Result Http.Error String -> Msg
gotShapeGrid result =
    case result of
        Ok shapegrid ->
            GotShapeGrid shapegrid

        Err err ->
            Debug.crash (toString err)


runMCPA : Flags -> Int -> String -> Dict String String -> List String -> Cmd Msg
runMCPA flags gridsetId archive_name filters selectedSpecies =
    let
        queryWithFilters =
            filters
                |> Dict.insert "archive_name" archive_name
                |> Dict.insert "gridset_id" (toString gridsetId)
                |> Dict.insert "user" "public"
                |> Dict.foldr Q.add Q.empty

        query =
            selectedSpecies
                |> List.foldl (Q.add "squid") queryWithFilters
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = flags.apiRoot ++ "globalPam" ++ (Q.render query)
            , body = Http.emptyBody
            , expect = Http.expectJson Decoder.decodeAtomObject
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.send GotPostResponse


getSolrList : Flags -> Int -> Dict String String -> Cmd Msg
getSolrList flags gridset_id filters =
    let
        query =
            filters
                |> Dict.insert "gridsetid" (toString gridset_id)
                |> Dict.insert "user" "public"
                |> Dict.foldr Q.add Q.empty
                |> Q.render
    in
        Http.request
            { method = "GET"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = flags.apiRoot ++ "globalPam" ++ query
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    { facets = initFacets
    , filters = Dict.empty
    , selectedSpecies = Nothing
    , pavs = []
    , shapegrid = Nothing
    , loadingPavs = True
    , archive_name = ""
    , postStatus = NotPosted
    , flags = flags
    , gridset_id =
        0
        -- yuck
    }
        ! [ getPamGridSets flags ]



-- , getShapeGrid flags, getSolrList flags Dict.empty ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.postStatus of
        NotPosted ->
            bboxSelected BBoxSelected

        _ ->
            Sub.none


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
