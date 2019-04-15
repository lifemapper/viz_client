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


module BoomResults exposing (Model, init, update, page, Msg)

import List.Extra as List
import Maybe.Extra as Maybe
import Set
import Time
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Http
import Decoder
import ProgramFlags exposing (Flags)
import Page exposing (Page)
import MapCardMultiple as MapCard
import Leaflet exposing (BoundingBox)
import Material
import Material.Options as Options
import Material.Typography as Typo
import Material.Spinner as Spinner
import Material.Progress as Loading
import Material.Grid as Grid
import Material.Button as Button


cardSize : Int
cardSize =
    4


resultsPerPage : Int
resultsPerPage =
    6


type alias ProjectionInfo =
    { record : Decoder.ProjectionRecord
    , occurrenceRecord : Decoder.OccurrenceSetRecord
    }


type alias LoadingInfo =
    { toLoad : List Decoder.AtomObjectRecord
    , currentlyLoaded : Dict Int ProjectionInfo
    }


type State
    = RequestingStatus Int
    | MonitoringProgress Int Decoder.GridsetProgress
    | GetProjectionsList Int
    | LoadingProjections LoadingInfo
    | NoProjections
    | DisplaySeparate Int String (List ( ProjectionInfo, MapCard.Model ))


type PackageStatus
    = WaitingForPackage Int
    | PackageReady Int


type alias Model =
    { programFlags : Flags
    , state : State
    , packageStatus : PackageStatus
    , mdl : Material.Model
    }


init : Flags -> Int -> ( Model, Cmd Msg )
init flags gridsetId =
    { programFlags = flags
    , state = RequestingStatus gridsetId
    , packageStatus = WaitingForPackage gridsetId
    , mdl = Material.model
    }
        ! [ loadProgress flags gridsetId, checkForPackage flags gridsetId ]


type Msg
    = LoadProgress Int
    | GotProgress Int Decoder.GridsetProgress
    | LoadProjections Int
    | GotProjectionAtoms Int (List Decoder.AtomObjectRecord)
    | GotProjection Decoder.ProjectionRecord
    | NewProjectionInfo ProjectionInfo
    | CheckForPackage Int
    | GotPackageStatus Int Bool
    | SetPage Int
    | SetSpeciesFilter String
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
                DisplaySeparate page speciesFilter display ->
                    updateMapCard i msg_ display (DisplaySeparate page speciesFilter) model

                _ ->
                    ( model, Cmd.none )
    in
        case msg of
            Nop ->
                ( model, Cmd.none )

            LoadProgress gridsetId ->
                ( model, loadProgress model.programFlags gridsetId )

            GotProgress gridsetId gridsetProgress ->
                let
                    (Decoder.GridsetProgress progress) =
                        gridsetProgress
                in
                    if progress.progress == 1 then
                        { model | state = GetProjectionsList gridsetId }
                            ! [ loadProjections model.programFlags gridsetId
                              , checkForPackage model.programFlags gridsetId
                              ]
                    else
                        ( { model | state = MonitoringProgress gridsetId gridsetProgress }, Cmd.none )

            LoadProjections gridsetId ->
                ( model, loadProjections model.programFlags gridsetId )

            CheckForPackage gridsetId ->
                ( model, checkForPackage model.programFlags gridsetId )

            GotProjectionAtoms gridSetId atoms ->
                let
                    loadingInfo =
                        { toLoad = atoms, currentlyLoaded = Dict.empty }
                in
                    if List.length atoms > 0 then
                        ( { model | state = LoadingProjections loadingInfo }
                        , atoms |> List.map (loadMetadata model.programFlags loadingInfo) |> Cmd.batch
                        )
                    else
                        ( { model | state = NoProjections }, Cmd.none )

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

            SetPage p ->
                case model.state of
                    DisplaySeparate page speciesFilter display ->
                        ( { model | state = DisplaySeparate p speciesFilter display }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            SetSpeciesFilter species ->
                case model.state of
                    DisplaySeparate page _ display ->
                        ( { model | state = DisplaySeparate page species display }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            GotPackageStatus id available ->
                if available then
                    { model | packageStatus = PackageReady id } ! []
                else
                    { model | packageStatus = WaitingForPackage id } ! []

            MapCardMsg i msg_ ->
                liftedMapCardUpdate i msg_ model

            Mdl msg_ ->
                Material.update Mdl msg_ model


displaySeparate : List ProjectionInfo -> State
displaySeparate infos =
    infos
        |> List.map (\info -> ( info, makeSeparateMap info ))
        |> DisplaySeparate 0 "all"


makeSeparateMap : ProjectionInfo -> MapCard.Model
makeSeparateMap info =
    [ makeBackgroundMap info
    , makeProjectionMap info |> Maybe.toList
    , makeOccurrenceMap info
    ]
        |> List.concat
        |> MapCard.init (boundingBoxForProjection info)


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


checkForPackage : Flags -> Int -> Cmd Msg
checkForPackage flags id =
    Http.request
        { method = "HEAD"
        , headers = []
        , url = packageUrl flags id
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\{ status } -> Ok (status.code == 200))
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (gotPackageStatus id)


packageUrl : Flags -> Int -> String
packageUrl { apiRoot } id =
    apiRoot ++ "gridset/" ++ (toString id) ++ "/package"


gotPackageStatus : Int -> Result Http.Error Bool -> Msg
gotPackageStatus id result =
    case result of
        Ok available ->
            GotPackageStatus id available

        _ ->
            GotPackageStatus id False


loadProgress : Flags -> Int -> Cmd Msg
loadProgress { apiRoot } id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "gridset/" ++ (toString id) ++ "/progress?detail=1"
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeGridsetProgress
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (gotProgress id)


gotProgress : Int -> Result Http.Error Decoder.GridsetProgress -> Msg
gotProgress id result =
    case result of
        Ok progress ->
            GotProgress id progress

        Err err ->
            Debug.log "Error retrieving gridset progress" err |> always Nop


loadProjections : Flags -> Int -> Cmd Msg
loadProjections { apiRoot } id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "sdmProject?gridsetid=" ++ (toString id)
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
view { state, packageStatus, mdl, programFlags } =
    let
        downloadButton =
            case packageStatus of
                WaitingForPackage id ->
                    Button.render Mdl
                        [ 666 ]
                        mdl
                        [ Button.raised, Button.disabled ]
                        [ Html.text "Download Results Package" ]

                PackageReady id ->
                    Button.render Mdl
                        [ 666 ]
                        mdl
                        [ Button.raised, Button.link <| packageUrl programFlags id ]
                        [ Html.text "Download Results Package" ]
    in
        case state of
            RequestingStatus _ ->
                Options.div []
                    [ Options.div
                        [ Options.css "display" "flex"
                        , Options.css "justify-content" "flex-end"
                        , Options.css "margin" "20px 20px 0 20px"
                        ]
                        [ downloadButton ]
                    , Options.div
                        [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                        [ Html.text "Requesting status...", Html.p [] [ Spinner.spinner [ Spinner.active True ] ] ]
                    ]

            MonitoringProgress _ (Decoder.GridsetProgress progress) ->
                Options.div []
                    [ Options.div
                        [ Options.css "display" "flex"
                        , Options.css "justify-content" "flex-end"
                        , Options.css "margin" "20px 20px 0 20px"
                        ]
                        [ downloadButton ]
                    , if progress.progress == -1 then
                        Options.div
                            [ Options.css "margin" "auto", Options.css "padding-top" "50px", Options.css "width" "400px", Typo.headline ]
                            [ Html.text <| (Maybe.withDefault "Unfortunately, the project failed" progress.message) ++ "." ]
                      else
                        Options.div
                            [ Options.css "margin" "auto", Options.css "padding-top" "50px", Options.css "width" "400px", Typo.headline ]
                            [ Html.text "Waiting for results..."
                            , Html.p [] [ Loading.progress (100 * progress.progress) ]
                            , Html.p [] [ Html.text <| (Maybe.withDefault "" progress.message) ++ "." ]
                            ]
                    ]

            GetProjectionsList _ ->
                Options.div []
                    [ Options.div
                        [ Options.css "display" "flex"
                        , Options.css "justify-content" "flex-end"
                        , Options.css "margin" "20px 20px 0 20px"
                        ]
                        [ downloadButton ]
                    , Options.div
                        [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                        [ Html.text "Loading projections...", Html.p [] [ Spinner.spinner [ Spinner.active True ] ] ]
                    ]

            LoadingProjections _ ->
                Options.div []
                    [ Options.div
                        [ Options.css "display" "flex"
                        , Options.css "justify-content" "flex-end"
                        , Options.css "margin" "20px 20px 0 20px"
                        ]
                        [ downloadButton ]
                    , Options.div
                        [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                        [ Html.text "Loading projections...", Html.p [] [ Spinner.spinner [ Spinner.active True ] ] ]
                    ]

            NoProjections ->
                Options.div
                    [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                    [ Html.text "No projections were returned." ]

            DisplaySeparate page speciesFilter display ->
                let
                    filtered =
                        if speciesFilter == "all" then
                            display
                        else
                            display |> List.filter (Tuple.first >> .record >> .speciesName >> ((==) (Just speciesFilter)))

                    projCount =
                        List.length filtered

                    species =
                        display
                            |> List.filterMap (Tuple.first >> .record >> .speciesName)
                            |> Set.fromList
                in
                    Options.div []
                        [ Options.div
                            [ Options.css "display" "flex"
                            , Options.css "justify-content" "space-between"
                            , Options.css "margin" "20px 20px 0 20px"
                            ]
                            [ Options.div [ Typo.title, Options.css "margin-top" "6px" ]
                                [ Html.text <|
                                    if (List.length display) == 1 then
                                        "Project produced one species model."
                                    else
                                        "Project produced " ++ (display |> List.length |> toString) ++ " species models."
                                ]
                            , if Set.size species > 1 then
                                Options.div []
                                    [ Html.text "Showing: "
                                    , Options.styled Html.select
                                        [ Options.onInput SetSpeciesFilter ]
                                        (species
                                            |> Set.toList
                                            |> List.map
                                                (\species ->
                                                    Html.option [ Html.Attributes.selected (species == speciesFilter) ]
                                                        [ Html.text species ]
                                                )
                                            |> ((::) (Html.option [ Html.Attributes.value "all" ] [ Html.text "All species" ]))
                                        )
                                    ]
                              else
                                Options.div [] []
                            , if projCount > resultsPerPage then
                                Options.div [] <|
                                    List.concat <|
                                        [ [ Html.text <|
                                                "Showing page "
                                                    ++ (toString <| 1 + page)
                                                    ++ " of "
                                                    ++ (toString <| ceiling <| (toFloat projCount) / (toFloat resultsPerPage))
                                                    ++ "."
                                          ]
                                        , if (1 + page) * resultsPerPage < projCount then
                                            [ Options.styled Html.a
                                                [ Options.css "cursor" "pointer", Options.onClick (SetPage <| page + 1) ]
                                                [ Html.text " next" ]
                                            ]
                                          else
                                            []
                                        , if page > 0 then
                                            [ Options.styled Html.a
                                                [ Options.css "cursor" "pointer", Options.onClick (SetPage <| page - 1) ]
                                                [ Html.text " prev" ]
                                            ]
                                          else
                                            []
                                        ]
                              else
                                Options.div [] []
                            , downloadButton
                            ]
                        , filtered
                            |> List.indexedMap viewSeparate
                            |> List.drop (page * resultsPerPage)
                            |> List.take resultsPerPage
                            |> Grid.grid []
                        ]


viewSeparate : Int -> ( ProjectionInfo, MapCard.Model ) -> Grid.Cell Msg
viewSeparate i ( { record }, mapCard ) =
    Grid.cell []
        [ MapCard.view [ i ] (projectionTitle record) mapCard
            |> Html.map (MapCardMsg i)
        ]


page : Page Model Msg
page =
    { view = view
    , selectedTab = always 0
    , selectTab = always Nop
    , tabTitles =
        always []
        --tabTitles
    , subscriptions = subscriptions
    , title = "Species Model Results"
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.state of
            MonitoringProgress gridsetId _ ->
                Time.every (30 * Time.second) (always <| LoadProgress gridsetId)

            _ ->
                Sub.none
        , case model.packageStatus of
            WaitingForPackage gridsetId ->
                Time.every (30 * Time.second) (always <| CheckForPackage gridsetId)

            _ ->
                Sub.none
        ]
