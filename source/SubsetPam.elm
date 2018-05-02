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
    , taxonOrder = Set.empty
    , taxonPhylum = Set.empty
    , taxonSpecies = Set.empty
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
    , pavs : String
    , shapeGrid : Maybe String
    , loadingPavs : Bool
    , archiveName : String
    , postStatus : PostStatus
    }


type PostStatus
    = NotPosted
    | Posted
    | PostFinished
    | PostFailed
    | Processing { total : Int, completed : Int, id : Int }


type Msg
    = Nop
    | GotSolrList SolrList
    | GotShapeGrid String
    | SetFilter String String
    | ClearFilter String
    | SetArchiveName String
    | RunMCPA
    | GotPostResponse (Result Http.Error Decoder.AtomObject)
    | StatusUpdate Decoder.GridSet


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
                            "http://gad210.nchc.org.tw/api/v2/gridset/"
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


viewNotPosted : Model -> Html Msg
viewNotPosted { facets, filters, pavs, shapeGrid, loadingPavs, archiveName } =
    Html.div [ Html.Attributes.style [ ( "font-family", "sans-serif" ), ( "display", "flex" ), ( "justify-content", "space-around" ) ] ]
        [ Html.div []
            [ Html.h3 [] [ header loadingPavs ]
            , Html.table [ Html.Attributes.style [ ( "width", "800px" ) ] ]
                [ Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Kingdom: " ]
                    , Html.td [] [ selector loadingPavs filters "taxonKingdom" facets.taxonKingdom ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Phylum: " ]
                    , Html.td [] [ selector loadingPavs filters "taxonPhylum" facets.taxonPhylum ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Class: " ]
                    , Html.td [] [ selector loadingPavs filters "taxonClass" facets.taxonClass ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Order: " ]
                    , Html.td [] [ selector loadingPavs filters "taxonOrder" facets.taxonOrder ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Family: " ]
                    , Html.td [] [ selector loadingPavs filters "taxonFamily" facets.taxonFamily ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Genus: " ]
                    , Html.td [] [ selector loadingPavs filters "taxonGenus" facets.taxonGenus ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Species: " ]
                    , Html.td [] [ selector loadingPavs filters "taxonSpecies" facets.taxonSpecies ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ), ( "width", "20px" ) ] ] [ Html.text "Algorithm: " ]
                    , Html.td [] [ selector loadingPavs filters "algorithmCode" facets.algorithms ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Model: " ]
                    , Html.td [] [ selector loadingPavs filters "modelScenarioCode" facets.modelScenarios ]
                    ]
                , Html.tr []
                    [ Html.td [ Html.Attributes.style [ ( "text-align", "right" ) ] ] [ Html.text "Projection: " ]
                    , Html.td [] [ selector loadingPavs filters "sdmProjScenarioCode" facets.projectionScenarios ]
                    ]
                ]
            , Html.h3 [] [ Html.text "Matching species" ]
            , Html.ul [ Html.Attributes.style [ ( "height", "400px" ), ( "overflow-y", "auto" ), ( "border", "1px solid grey" ) ] ]
                (List.map displayName <| Set.toList facets.displayNames)
            , Html.div []
                [ Html.input
                    [ Html.Attributes.placeholder "Archive name"
                    , Html.Attributes.style [ ( "margin-right", "5px" ) ]
                    , Events.onInput SetArchiveName
                    , Html.Attributes.value archiveName
                    ]
                    []
                , Html.button
                    [ Events.onClick RunMCPA
                    , Html.Attributes.disabled (loadingPavs || (archiveName == ""))
                    ]
                    [ Html.text "Subset PAM" ]
                ]
            ]
        , Html.div []
            [ Html.h3 [] [ Html.text "Heat map of matching species" ]
            , Html.div
                [ Html.Attributes.class "leaflet-map"
                , Html.Attributes.attribute "data-map-pavs" pavs
                , Html.Attributes.attribute "data-map-shape-grid" <| Maybe.withDefault "" <| shapeGrid
                , Html.Attributes.style [ ( "width", "800px" ), ( "height", "800px" ) ]
                ]
                []
            ]
        ]


displayName : String -> Html Msg
displayName name =
    Html.li [] [ Html.text name ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        GotSolrList (SolrList pavs) ->
            let
                facets =
                    List.foldr addAttrs initFacets pavs

                pavsJoined =
                    pavs |> List.map (\(SolrPAV { compressedPAV }) -> compressedPAV) |> String.join "\n"
            in
                ( { model | facets = facets, pavs = pavsJoined, loadingPavs = False }, Cmd.none )

        GotShapeGrid shp ->
            ( { model | shapeGrid = Just shp }, Cmd.none )

        SetFilter key value ->
            let
                filters =
                    Dict.insert key value model.filters
            in
                ( { model | filters = filters, loadingPavs = True }, getSolrList filters )

        ClearFilter key ->
            let
                filters =
                    Dict.remove key model.filters
            in
                if Dict.isEmpty filters then
                    ( { model | facets = initFacets, filters = filters }, Cmd.none )
                else
                    ( { model | filters = filters, loadingPavs = True }, getSolrList filters )

        SetArchiveName name ->
            ( { model | archiveName = String.trim name }, Cmd.none )

        RunMCPA ->
            ( { model | postStatus = Posted }, runMCPA model.archiveName model.filters )

        GotPostResponse result ->
            case result of
                Ok (Decoder.AtomObject { id }) ->
                    ( { model | postStatus = PostFinished }, checkStatus id )

                Err err ->
                    Debug.log "Post failed" err
                        |> always ( { model | postStatus = PostFailed }, Cmd.none )

        StatusUpdate (Decoder.GridSet { id, matrices }) ->
            let
                (Decoder.GridSetMatrices ms) =
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
                        checkStatus id
            in
                ( { model | postStatus = postStatus }, cmd )


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


checkStatus : Int -> Cmd Msg
checkStatus id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://gad210.nchc.org.tw/api/v2/gridset/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeGridSet
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotGridSet


gotGridSet : Result Http.Error Decoder.GridSet -> Msg
gotGridSet result =
    case result of
        Ok gridSet ->
            StatusUpdate gridSet

        Err err ->
            Debug.crash (toString err)


getShapeGrid : Cmd Msg
getShapeGrid =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://gad210.nchc.org.tw/api/v2/shapegrid/163/geojson"
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


runMCPA : String -> Dict String String -> Cmd Msg
runMCPA archiveName filters =
    let
        query =
            filters
                |> Dict.insert "archiveName" archiveName
                |> Dict.insert "gridSetId" "2"
                |> Dict.insert "user" "public"
                |> Dict.foldr Q.add Q.empty
                |> Q.render
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = "http://gad210.nchc.org.tw/api/v2/globalPam" ++ query
            , body = Http.emptyBody
            , expect = Http.expectJson Decoder.decodeAtomObject
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.send GotPostResponse


getSolrList : Dict String String -> Cmd Msg
getSolrList filters =
    let
        query =
            filters |> Dict.insert "user" "public" |> Dict.foldr Q.add Q.empty |> Q.render
    in
        Http.request
            { method = "GET"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = "http://gad210.nchc.org.tw/api/v2/globalPam" ++ query
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
    , loadingPavs = False
    , archiveName = ""
    , postStatus = NotPosted
    }
        ! [ getShapeGrid ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
