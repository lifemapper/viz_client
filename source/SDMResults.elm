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


module SDMResults exposing (Model, init, update, page, Msg)

import List.Extra as List
import Maybe.Extra as Maybe
import Time
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Decoder
import ProgramFlags exposing (Flags)
import Page exposing (Page)
import MapCardMultiple as MapCard
import Leaflet exposing (BoundingBox)
import Material
import Material.Options as Options
import Material.Typography as Typo
import Material.Spinner as Loading
import Material.Grid as Grid


type alias ProjectionInfo =
    { record : Decoder.ProjectionRecord
    , occurrenceRecord : Decoder.OccurrenceSetRecord
    }


type alias LoadingInfo =
    { toLoad : List Decoder.AtomObjectRecord
    , currentlyLoaded : Dict Int ProjectionInfo
    }


type State
    = WaitingForListToPopulate Int
    | LoadingProjections LoadingInfo
    | DisplaySeparate (List ( ProjectionInfo, MapCard.Model ))
    | DisplayGrouped (List ( List ProjectionInfo, MapCard.Model ))


type alias Model =
    { programFlags : Flags
    , state : State
    , mdl : Material.Model
    }


init : Flags -> Int -> Model
init flags gridsetId =
    { programFlags = flags
    , state = WaitingForListToPopulate gridsetId
    , mdl = Material.model
    }


type Msg
    = LoadProjections Int
    | GotProjectionAtoms Int (List Decoder.AtomObjectRecord)
    | GotProjection Decoder.ProjectionRecord
    | NewProjectionInfo ProjectionInfo
    | SetDisplayGrouped Bool
    | MapCardMsg Int MapCard.Msg
    | Nop
    | Mdl (Material.Msg Msg)


updateMapCard : Int -> MapCard.Msg -> List ( a, MapCard.Model ) -> (List ( a, MapCard.Model ) -> State) -> Model -> ( Model, Cmd Msg )
updateMapCard i msg_ display update model =
    List.getAt i display
        |> Maybe.andThen
            (\( a, mapCard ) ->
                let
                    ( mapCard_, cmd ) =
                        MapCard.update msg_ mapCard
                in
                    List.setAt i ( a, mapCard_ ) display
                        |> Maybe.map
                            (\display ->
                                ( { model | state = update display }
                                , Cmd.map (MapCardMsg i) cmd
                                )
                            )
            )
        |> Maybe.withDefault ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        liftedMapCardUpdate i msg_ model =
            case model.state of
                DisplaySeparate display ->
                    updateMapCard i msg_ display DisplaySeparate model

                DisplayGrouped display ->
                    updateMapCard i msg_ display DisplayGrouped model

                _ ->
                    ( model, Cmd.none )
    in
        case msg of
            Nop ->
                ( model, Cmd.none )

            LoadProjections gridsetId ->
                ( { model | state = WaitingForListToPopulate gridsetId }, loadProjections model.programFlags gridsetId )

            GotProjectionAtoms gridSetId atoms ->
                if List.length atoms == 0 then
                    ( { model | state = WaitingForListToPopulate gridSetId }, Cmd.none )
                else
                    let
                        loadingInfo =
                            { toLoad = atoms, currentlyLoaded = Dict.empty }
                    in
                        ( { model | state = LoadingProjections loadingInfo }
                        , atoms |> List.map (loadMetadata model.programFlags loadingInfo) |> Cmd.batch
                        )

            GotProjection record ->
                ( model, loadOccurrenceSet record )

            NewProjectionInfo newInfo ->
                case model.state of
                    LoadingProjections loadingInfo ->
                        let
                            currentlyLoaded =
                                Dict.insert newInfo.record.id newInfo loadingInfo.currentlyLoaded
                        in
                            if Dict.size currentlyLoaded == List.length loadingInfo.toLoad then
                                ( { model | state = Dict.values currentlyLoaded |> displaySeparate }, Cmd.none )
                            else
                                ( { model | state = LoadingProjections { loadingInfo | currentlyLoaded = currentlyLoaded } }
                                , Cmd.none
                                )

                    _ ->
                        ( model, Cmd.none )

            SetDisplayGrouped True ->
                case model.state of
                    DisplaySeparate display ->
                        ( { model | state = display |> List.map Tuple.first |> displayGrouped }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            SetDisplayGrouped False ->
                case model.state of
                    DisplayGrouped display ->
                        ( { model | state = display |> List.concatMap Tuple.first |> displaySeparate }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            MapCardMsg i msg_ ->
                liftedMapCardUpdate i msg_ model

            Mdl msg_ ->
                Material.update Mdl msg_ model


displaySeparate : List ProjectionInfo -> State
displaySeparate infos =
    infos
        |> List.map (\info -> ( info, makeSeparateMap info ))
        |> DisplaySeparate


makeSeparateMap : ProjectionInfo -> MapCard.Model
makeSeparateMap info =
    -- [ makeBackgroundMap info
    [ makeProjectionMap info |> Maybe.toList
    , makeOccurrenceMap info
    ]
        |> List.concat
        |> MapCard.init (boundingBoxForProjection info)


displayGrouped : List ProjectionInfo -> State
displayGrouped infos =
    infos
        |> List.sortBy (.record >> .squid >> Maybe.withDefault "")
        |> List.groupWhile (\x y -> x.record.squid == y.record.squid)
        |> List.map (\group -> ( group, makeGroupedMap group ))
        |> DisplayGrouped


makeGroupedMap : List ProjectionInfo -> MapCard.Model
makeGroupedMap projections =
    case projections of
        [] ->
            MapCard.init Nothing []

        first :: _ ->
            -- [ makeBackgroundMap first
            [ List.filterMap makeProjectionMap projections
            , makeOccurrenceMap first
            ]
                |> List.concat
                |> MapCard.init (boundingBoxForProjection first)


makeOccurrenceMap : ProjectionInfo -> List MapCard.NamedMap
makeOccurrenceMap { occurrenceRecord } =
    occurrenceRecord.map
        |> Maybe.map
            (\(Decoder.SingleLayerMap { endpoint, mapName, layerName }) ->
                { name = "Occurrences"
                , wmsInfo = { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
                }
            )
        |> Maybe.toList


makeBackgroundMap : ProjectionInfo -> List MapCard.NamedMap
makeBackgroundMap { occurrenceRecord } =
    occurrenceRecord.map
        |> Maybe.map
            (\(Decoder.SingleLayerMap { endpoint, mapName, layerName }) ->
                { name = "Blue Marble Next Generation (NASA)"
                , wmsInfo = { endPoint = endpoint, mapName = mapName, layers = [ "bmng" ] }
                }
            )
        |> Maybe.toList


projectionTitle : Decoder.ProjectionRecord -> String
projectionTitle record =
    record.metadata
        |> Maybe.andThen (\(Decoder.ProjectionMetadata { title }) -> title)
        |> Maybe.withDefault "Projection"


boundingBoxForProjection : ProjectionInfo -> Maybe BoundingBox
boundingBoxForProjection { record } =
    record.spatialRaster
        |> Maybe.map (\(Decoder.SpatialRaster { bbox }) -> bbox)
        |> Maybe.join
        |> Maybe.map
            (\(Decoder.SpatialRasterBbox bbox) ->
                case bbox of
                    [ lng1, lat1, lng2, lat2 ] ->
                        Just (BoundingBox lat1 lng1 lat2 lng2)

                    _ ->
                        Debug.log "bad bounding box" (toString bbox) |> always Nothing
            )
        |> Maybe.join


makeProjectionMap : ProjectionInfo -> Maybe MapCard.NamedMap
makeProjectionMap { record } =
    record.map
        |> Maybe.map
            (\(Decoder.SingleLayerMap { endpoint, mapName, layerName }) ->
                { name = projectionTitle record
                , wmsInfo = { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
                }
            )


loadOccurrenceSet : Decoder.ProjectionRecord -> Cmd Msg
loadOccurrenceSet record =
    case record.occurrenceSet |> Maybe.andThen (\(Decoder.ObjectRef o) -> o.metadataUrl) of
        Just url ->
            Http.request
                { method = "GET"
                , headers = [ Http.header "Accept" "application/json" ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson Decoder.decodeOccurrenceSet
                , timeout = Nothing
                , withCredentials = False
                }
                |> Http.send (gotOccurrenceSet record)

        Nothing ->
            Cmd.none


gotOccurrenceSet : Decoder.ProjectionRecord -> Result Http.Error Decoder.OccurrenceSet -> Msg
gotOccurrenceSet record result =
    case result of
        Ok (Decoder.OccurrenceSet occurrenceRecord) ->
            NewProjectionInfo { record = record, occurrenceRecord = occurrenceRecord }

        Err err ->
            Debug.log "Error fetching occurrence set" (toString err) |> always Nop


loadProjections : Flags -> Int -> Cmd Msg
loadProjections { apiRoot } id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "sdmProject?user=anon&gridsetid=" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (gotProjectionAtoms id)


gotProjectionAtoms : Int -> Result Http.Error Decoder.AtomList -> Msg
gotProjectionAtoms id result =
    case result of
        Ok (Decoder.AtomList atoms) ->
            atoms |> List.map (\(Decoder.AtomObject o) -> o) |> GotProjectionAtoms id

        Err err ->
            Debug.log "Error fetching projections" (toString err) |> always Nop


loadMetadata : Flags -> LoadingInfo -> Decoder.AtomObjectRecord -> Cmd Msg
loadMetadata { apiRoot } loadingInfo { id } =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "sdmProject/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeProjection
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (gotMetadata loadingInfo)


gotMetadata : LoadingInfo -> Result Http.Error Decoder.Projection -> Msg
gotMetadata loadingInfo result =
    case result of
        Ok (Decoder.Projection p) ->
            GotProjection p

        Err err ->
            Debug.log "Failed to load projection" err |> always Nop


view : Model -> Html Msg
view { state } =
    let
        loading message =
            Options.div
                [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                [ Html.text message, Html.p [] [ Loading.spinner [ Loading.active True ] ] ]
    in
        case state of
            WaitingForListToPopulate _ ->
                loading "Waiting for projections..."

            LoadingProjections _ ->
                loading "Loading projections..."

            DisplaySeparate display ->
                display
                    |> List.indexedMap viewSeparate
                    |> Grid.grid []

            DisplayGrouped display ->
                display
                    |> List.indexedMap viewGrouped
                    |> Grid.grid []


viewSeparate : Int -> ( ProjectionInfo, MapCard.Model ) -> Grid.Cell Msg
viewSeparate i ( { record }, mapCard ) =
    Grid.cell []
        [ MapCard.view [ i ] (projectionTitle record) mapCard
            |> Html.map (MapCardMsg i)
        ]


cardSize : Int
cardSize =
    4


viewGrouped : Int -> ( List ProjectionInfo, MapCard.Model ) -> Grid.Cell Msg
viewGrouped i ( projections, mapCard ) =
    case projections of
        [] ->
            Grid.cell [ Grid.size Grid.All cardSize ] []

        { record } :: _ ->
            Grid.cell [ Grid.size Grid.All cardSize ]
                [ MapCard.view [ i ] (record.speciesName |> Maybe.withDefault (toString record.id)) mapCard
                    |> Html.map (MapCardMsg i)
                ]


selectedTab : Model -> Int
selectedTab model =
    case model.state of
        DisplayGrouped _ ->
            1

        _ ->
            0


selectTab : Int -> Msg
selectTab i =
    SetDisplayGrouped (i == 1)


tabTitles : Model -> List (Html Msg)
tabTitles model =
    let
        titles =
            List.map Html.text [ "Ungrouped", "Group by species" ]
    in
        case model.state of
            DisplayGrouped _ ->
                titles

            DisplaySeparate _ ->
                titles

            _ ->
                []


page : Page Model Msg
page =
    { view = view
    , selectedTab = selectedTab
    , selectTab = selectTab
    , tabTitles = tabTitles
    , subscriptions = subscriptions
    , title = "Projection Results"
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        WaitingForListToPopulate gridsetId ->
            Time.every (5 * Time.second) (always <| LoadProjections gridsetId)

        _ ->
            Sub.none
