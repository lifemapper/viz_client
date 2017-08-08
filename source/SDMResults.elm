module SDMResults exposing (Model, init, update, page, Msg(LoadProjections))

import List.Extra as List
import Time
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Decoder
import ProgramFlags exposing (Flags)
import Page exposing (Page)
import MapCardMultiple as MapCard
import Material
import Material.Options as Options
import Material.Typography as Typo
import Material.Spinner as Loading


type alias ProjectionInfo =
    { record : Decoder.ProjectionRecord
    , mapCard : MapCard.Model
    , occurrenceRecord : Decoder.OccurrenceSetRecord
    }


type alias LoadingInfo =
    { toLoad : List Decoder.AtomObjectRecord
    , currentlyLoaded : Dict Int ProjectionInfo
    }


type State
    = Quiescent
    | WaitingForListToPopulate Int
    | LoadingProjections LoadingInfo
    | AllProjectionsLoaded (List ProjectionInfo)


type alias Model =
    { programFlags : Flags
    , state : State
    , mdl : Material.Model
    }


init : Flags -> Model
init flags =
    { programFlags = flags
    , state = Quiescent
    , mdl = Material.model
    }


type Msg
    = LoadProjections Int
    | GotProjectionAtoms Int (List Decoder.AtomObjectRecord)
    | GotProjection Decoder.ProjectionRecord
    | NewProjectionInfo ProjectionInfo
    | MapCardMsg Int MapCard.Msg
    | Nop
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        liftedMapCardUpdate i msg_ model =
            case model.state of
                AllProjectionsLoaded projections ->
                    List.getAt i projections
                        |> Maybe.andThen
                            (\projection ->
                                let
                                    ( mapCard_, cmd ) =
                                        MapCard.update msg_ projection.mapCard

                                    updated =
                                        { projection | mapCard = mapCard_ }
                                in
                                    List.setAt i updated projections
                                        |> Maybe.map
                                            (\ps ->
                                                ( { model | state = AllProjectionsLoaded ps }
                                                , Cmd.map (MapCardMsg i) cmd
                                                )
                                            )
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

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
                                ( { model | state = AllProjectionsLoaded <| Dict.values currentlyLoaded }, Cmd.none )
                            else
                                ( { model | state = LoadingProjections { loadingInfo | currentlyLoaded = currentlyLoaded } }
                                , Cmd.none
                                )

                    _ ->
                        ( model, Cmd.none )

            MapCardMsg i msg_ ->
                liftedMapCardUpdate i msg_ model

            Mdl msg_ ->
                Material.update Mdl msg_ model


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
            NewProjectionInfo
                { record = record
                , mapCard = getWmsInfo record occurrenceRecord |> MapCard.init
                , occurrenceRecord = occurrenceRecord
                }

        Err err ->
            Debug.log "Error fetching occurrence set" (toString err) |> always Nop


getWmsInfo : Decoder.ProjectionRecord -> Decoder.OccurrenceSetRecord -> List MapCard.NamedMap
getWmsInfo projectionRecord occurrenceRecord =
    List.filterMap identity
        [ projectionRecord.map
            |> Maybe.map
                (\(Decoder.SingleLayerMap { endpoint, mapName, layerName }) ->
                    { name = "Projection"
                    , wmsInfo = { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
                    }
                )
        , occurrenceRecord.map
            |> Maybe.map
                (\(Decoder.SingleLayerMap { endpoint, mapName, layerName }) ->
                    { name = "Occurrences"
                    , wmsInfo = { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
                    }
                )
        ]


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
    case state of
        AllProjectionsLoaded projections ->
            projections
                |> List.indexedMap viewMap
                |> (Options.div
                        [ Options.css "display" "flex"
                        , Options.css "justify-content" "space-around"
                        ]
                   )

        Quiescent ->
            Options.div [] []

        WaitingForListToPopulate _ ->
            Options.div [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                [ Html.text "Waiting for projections..."
                , Html.p [] [ Loading.spinner [ Loading.active True ] ]
                ]

        LoadingProjections _ ->
            Options.div [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                [ Html.text "Loading projections..."
                , Html.p [] [ Loading.spinner [ Loading.active True ] ]
                ]


viewMap : Int -> ProjectionInfo -> Html Msg
viewMap i { record, mapCard } =
    MapCard.view [ i ] (record.speciesName |> Maybe.withDefault (toString record.id)) mapCard
        |> Html.map (MapCardMsg i)


page : Page Model Msg
page =
    { view = view
    , selectedTab = always 0
    , selectTab = always Nop
    , tabTitles = always []
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
