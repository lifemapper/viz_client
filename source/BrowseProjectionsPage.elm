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


module BrowseProjectionsPage exposing (Model, init, update, page, Msg)

import List.Extra as List
import Maybe.Extra as Maybe
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events as Events
import QueryString as Q
import Http
import Decoder
import ProgramFlags exposing (Flags)
import Page exposing (Page)
import MapCardMultiple as MapCard
import Leaflet exposing (BoundingBox)
import OccurrenceSetChooser
import Material
import Material.Options as Options
import Material.Typography as Typo
import Material.Spinner as Loading
import Material.Grid as Grid
import Material.List as L
import Material.Helpers as Helpers
import Helpers exposing (Index, chain)


type alias ProjectionInfo =
    { record : Decoder.ProjectionRecord
    , occurrenceRecord : Decoder.OccurrenceSetRecord
    }


type alias LoadingInfo =
    { toLoad : List Decoder.AtomObjectRecord
    , currentlyLoaded : Dict Int ProjectionInfo
    , occSetsToLoad : List Int
    }


type State
    = Init
    | LoadingProjections LoadingInfo
    | DisplaySeparate (List ( ProjectionInfo, MapCard.Model ))
    | DisplayGrouped (List ( List ProjectionInfo, MapCard.Model ))
    | NothingToShow


type alias Model =
    { programFlags : Flags
    , occurrenceSetChooser : OccurrenceSetChooser.Model
    , occurrenceSets : List Decoder.OccWebListItemRecord
    , scenarios : List Decoder.AtomObjectRecord
    , modelScenarioFilter : Maybe String
    , projScenarioFilter : Maybe String
    , state : State
    , mdl : Material.Model
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { programFlags = flags
    , occurrenceSetChooser = OccurrenceSetChooser.init flags
    , occurrenceSets = []
    , scenarios = []
    , modelScenarioFilter = Nothing
    , projScenarioFilter = Nothing
    , state = Init
    , mdl = Material.model
    }
        ! [ loadScenarios flags ]


type Msg
    = GotProjectionAtoms Int (List Decoder.AtomObjectRecord)
    | GotProjection Decoder.ProjectionRecord
    | GotScenarioAtoms (List Decoder.AtomObjectRecord)
    | SetModelScenarioFilter (Maybe Int)
    | SetProjScenarioFilter (Maybe Int)
    | GotModelScenarioFilterCode (Maybe String)
    | GotProjScenarioFilterCode (Maybe String)
    | NewProjectionInfo ProjectionInfo
    | SetDisplayGrouped Bool
    | ChooserMsg OccurrenceSetChooser.Msg
    | Remove Int
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


addSelected : OccurrenceSetChooser.Msg -> Model -> ( Model, Cmd Msg )
addSelected msg model =
    case msg of
        OccurrenceSetChooser.Select object ->
            loadProjections { model | occurrenceSets = model.occurrenceSets ++ [ object ] }

        msg ->
            ( model, Cmd.none )


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

        liftedChooserUpdate msg_ model =
            Helpers.lift
                .occurrenceSetChooser
                (\m x -> { m | occurrenceSetChooser = x })
                ChooserMsg
                OccurrenceSetChooser.update
                msg_
                model
    in
        case msg of
            Nop ->
                ( model, Cmd.none )

            GotProjectionAtoms occurrence_set_id atoms ->
                case model.state of
                    LoadingProjections loadingInfo ->
                        let
                            loadingInfo_ =
                                { loadingInfo
                                    | toLoad = loadingInfo.toLoad ++ atoms
                                    , occSetsToLoad = loadingInfo.occSetsToLoad |> List.filter ((/=) occurrence_set_id)
                                }
                        in
                            if loadingInfo_.occSetsToLoad == [] && loadingInfo_.toLoad == [] then
                                ( { model | state = NothingToShow }, Cmd.none )
                            else
                                ( { model | state = LoadingProjections loadingInfo_ }
                                , atoms |> List.map (loadMetadata model.programFlags loadingInfo) |> Cmd.batch
                                )

                    _ ->
                        ( model, Cmd.none )

            GotProjection record ->
                ( model, loadOccurrenceSet record )

            GotScenarioAtoms atoms ->
                ( { model | scenarios = atoms }, Cmd.none )

            Remove occSetId ->
                loadProjections { model | occurrenceSets = model.occurrenceSets |> List.filter (.id >> (/=) occSetId) }

            SetModelScenarioFilter id ->
                case id of
                    Just id ->
                        ( model, getScenarioCode model.programFlags GotModelScenarioFilterCode id )

                    Nothing ->
                        loadProjections { model | modelScenarioFilter = Nothing }

            SetProjScenarioFilter id ->
                case id of
                    Just id ->
                        ( model, getScenarioCode model.programFlags GotProjScenarioFilterCode id )

                    Nothing ->
                        loadProjections { model | projScenarioFilter = Nothing }

            GotModelScenarioFilterCode code ->
                loadProjections { model | modelScenarioFilter = code }

            GotProjScenarioFilterCode code ->
                loadProjections { model | projScenarioFilter = code }

            NewProjectionInfo newInfo ->
                case model.state of
                    LoadingProjections loadingInfo ->
                        let
                            currentlyLoaded =
                                Dict.insert newInfo.record.id newInfo loadingInfo.currentlyLoaded
                        in
                            if loadingInfo.occSetsToLoad == [] && Dict.size currentlyLoaded == List.length loadingInfo.toLoad then
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

            ChooserMsg msg_ ->
                chain (addSelected msg_) (liftedChooserUpdate msg_) model

            Mdl msg_ ->
                Material.update Mdl msg_ model


displaySeparate : List ProjectionInfo -> State
displaySeparate infos =
    infos
        |> List.map (\info -> ( info, makeSeparateMap info ))
        |> DisplaySeparate


makeSeparateMap : ProjectionInfo -> MapCard.Model
makeSeparateMap info =
    [ makeBackgroundMap info
    , makeProjectionMap info |> Maybe.toList
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
            [ makeBackgroundMap first
            , List.filterMap makeProjectionMap projections
            , makeOccurrenceMap first
            ]
                |> List.concat
                |> MapCard.init (boundingBoxForProjection first)


makeOccurrenceMap : ProjectionInfo -> List MapCard.NamedMap
makeOccurrenceMap { occurrenceRecord } =
    occurrenceRecord.map
        |> Maybe.map
            (\(Decoder.SingleLayerMap { endpoint, map_name, layer_name }) ->
                { name = "Occurrences"
                , wmsInfo = { endpoint = endpoint, map_name = map_name, layers = [ layer_name ] }
                }
            )
        |> Maybe.toList


makeBackgroundMap : ProjectionInfo -> List MapCard.NamedMap
makeBackgroundMap { occurrenceRecord } =
    occurrenceRecord.map
        |> Maybe.map
            (\(Decoder.SingleLayerMap { endpoint, map_name, layer_name }) ->
                { name = "Blue Marble (Next Generation)"
                , wmsInfo = { endpoint = endpoint, map_name = map_name, layers = [ "bmng" ] }
                }
            )
        |> Maybe.toList


projectionTitle : Decoder.ProjectionRecord -> String
projectionTitle { species_name, algorithm, model_scenario, projection_scenario } =
    (species_name |> Maybe.withDefault "")
        ++ " "
        ++ (algorithm |> Maybe.map (\(Decoder.Algorithm { code }) -> code) |> Maybe.withDefault "")
        ++ " "
        -- take out model scenario
        -- ++ (model_scenario |> Maybe.map (\(Decoder.ScenarioRef { code }) -> code) |> Maybe.join |> Maybe.withDefault "")
        -- ++ " to "
        ++
            (projection_scenario |> Maybe.map (\(Decoder.ScenarioRef { code }) -> code) |> Maybe.join |> Maybe.withDefault "")


boundingBoxForProjection : ProjectionInfo -> Maybe BoundingBox
boundingBoxForProjection { record } =
    record.spatial_raster
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
            (\(Decoder.SingleLayerMap { endpoint, map_name, layer_name }) ->
                { name = projectionTitle record
                , wmsInfo = { endpoint = endpoint, map_name = map_name, layers = [ layer_name ] }
                }
            )


loadOccurrenceSet : Decoder.ProjectionRecord -> Cmd Msg
loadOccurrenceSet record =
    case record.occurrence_set |> Maybe.andThen (\(Decoder.ObjectRef o) -> o.metadata_url) of
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


getScenarioCode : Flags -> (Maybe String -> Msg) -> Int -> Cmd Msg
getScenarioCode { apiRoot } msg id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "scenario/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeScenario
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (gotScenario msg)


gotScenario : (Maybe String -> Msg) -> Result Http.Error Decoder.Scenario -> Msg
gotScenario msg result =
    case result of
        Ok (Decoder.Scenario s) ->
            msg s.code

        Err err ->
            Debug.log "Error fetching scenario" (toString err) |> always (msg Nothing)


loadScenarios : Flags -> Cmd Msg
loadScenarios { apiRoot } =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "scenario"
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotScenarioAtoms


gotScenarioAtoms : Result Http.Error Decoder.AtomList -> Msg
gotScenarioAtoms result =
    case result of
        Ok (Decoder.AtomList atoms) ->
            atoms |> List.map (\(Decoder.AtomObject o) -> o) |> GotScenarioAtoms

        Err err ->
            Debug.log "Error fetching scenarios" (toString err) |> always Nop


loadProjections : Model -> ( Model, Cmd Msg )
loadProjections model =
    let
        query occurrenceSet =
            Q.empty
                |> Q.add "occurrence_set_id" (occurrenceSet.id |> toString)
                |> (model.modelScenarioFilter |> Maybe.map (Q.add "model_scenario_code") |> Maybe.withDefault identity)
                |> (model.projScenarioFilter |> Maybe.map (Q.add "projection_scenario_code") |> Maybe.withDefault identity)
                |> if OccurrenceSetChooser.isPublicData model.occurrenceSetChooser then
                    Q.add "user" "public"
                   else
                    identity

        request occurrenceSet =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Accept" "application/json" ]
                , url = model.programFlags.apiRoot ++ "sdmProject" ++ (Q.render <| query occurrenceSet)
                , body = Http.emptyBody
                , expect = Http.expectJson Decoder.decodeAtomList
                , timeout = Nothing
                , withCredentials = False
                }
                |> Http.send (gotProjectionAtoms occurrenceSet.id)
    in
        if model.occurrenceSets == [] then
            { model | state = NothingToShow } ! []
        else
            { model
                | state =
                    LoadingProjections
                        { toLoad = []
                        , currentlyLoaded = Dict.empty
                        , occSetsToLoad = List.map .id model.occurrenceSets
                        }
            }
                ! (model.occurrenceSets |> List.map request)


gotProjectionAtoms : Int -> Result Http.Error Decoder.AtomList -> Msg
gotProjectionAtoms occurrence_set_id result =
    case result of
        Ok (Decoder.AtomList atoms) ->
            atoms |> List.map (\(Decoder.AtomObject o) -> o) |> (GotProjectionAtoms occurrence_set_id)

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


occurrenceSetLI : Model -> Int -> Decoder.OccWebListItemRecord -> Html Msg
occurrenceSetLI model i o =
    L.li []
        [ L.content [] [ Html.text <| o.name ]
        , L.content2 [ Options.css "flex-flow" "row" ]
            [ L.icon "delete" [ Options.onClick (Remove o.id) ] ]
        ]


occurrenceSetList : Model -> Html Msg
occurrenceSetList model =
    L.ul [ Options.css "margin-left" "20px" ] <|
        List.concat
            [ [ Options.styled Html.p [ Typo.headline ] [ Html.text "Select species" ] ]
            , List.indexedMap (occurrenceSetLI model) model.occurrenceSets
            , [ (OccurrenceSetChooser.view False [ 777 ] model.occurrenceSetChooser |> Html.map ChooserMsg) ]
            ]


view : Model -> Html Msg
view model =
    let
        inner =
            case model.state of
                Init ->
                    Options.div [] []

                LoadingProjections _ ->
                    Options.div
                        [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                        [ Html.text "Loading projections...", Html.p [] [ Loading.spinner [ Loading.active True ] ] ]

                DisplaySeparate display ->
                    display |> List.indexedMap viewSeparate |> Grid.grid []

                DisplayGrouped display ->
                    display |> List.indexedMap viewGrouped |> Grid.grid []

                NothingToShow ->
                    Options.div
                        [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                        [ Html.text "No projections matched." ]

        scenarioOptions onSelect =
            Html.select
                [ Events.onInput <| String.toInt >> Result.toMaybe >> onSelect
                , Html.Attributes.style [ ( "width", "300px" ) ]
                ]
            <|
                (Html.option [] [ Html.text "Any" ])
                    :: (model.scenarios |> List.map (\s -> Html.option [ Html.Attributes.value (toString s.id) ] [ Html.text s.name ]))

        scenarioFilters =
            Options.div [ Options.css "margin-left" "20px", Options.css "margin-top" "20px" ]
                [ Options.styled Html.p [ Typo.headline ] [ Html.text "Filter by scenario" ]
                , Options.styled Html.p
                    [ Options.css "text-align" "right" ]
                    [ Html.label [] [ Html.text "Model " ]
                    , scenarioOptions SetModelScenarioFilter
                    ]
                , Options.styled Html.p
                    [ Options.css "text-align" "right" ]
                    [ Html.label [] [ Html.text "Projection " ]
                    , scenarioOptions SetProjScenarioFilter
                    ]
                ]
    in
        Options.div []
            [ Options.div [ Options.css "display" "flex" ]
                [ occurrenceSetList model
                , scenarioFilters
                ]
            , inner
            ]


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
                [ MapCard.view [ i ] (record.species_name |> Maybe.withDefault (toString record.id)) mapCard
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
    , selectedTab = always 0
    , selectTab = always Nop
    , tabTitles = always []
    , subscriptions = subscriptions
    , title = "Search Species"
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
