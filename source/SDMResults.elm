module SDMResults exposing (Model, init, update, page, Msg(LoadProjections))

import List.Extra as List
import Html exposing (Html)
import Http
import Decoder
import ProgramFlags exposing (Flags)
import Page exposing (Page)
import MapCardMultiple as MapCard
import Material
import Material.Options as Options


type alias ProjectionInfo =
    { record : Decoder.ProjectionRecord
    , mapCard : MapCard.Model
    , occurrenceRecord : Decoder.OccurrenceSetRecord
    }


type alias Model =
    { programFlags : Flags
    , projectionsToLoad : Maybe Int
    , projections : List ProjectionInfo
    , mdl : Material.Model
    }


init : Flags -> Model
init flags =
    { programFlags = flags
    , projectionsToLoad = Nothing
    , projections = []
    , mdl = Material.model
    }


type Msg
    = LoadProjections Int
    | GotProjectionAtoms (List Decoder.AtomObjectRecord)
    | GotProjection Decoder.ProjectionRecord
    | NewProjectionInfo ProjectionInfo
    | MapCardMsg Int MapCard.Msg
    | Nop
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        liftedMapCardUpdate i msg_ model =
            List.getAt i model.projections
                |> Maybe.andThen
                    (\projection ->
                        let
                            ( mapCard_, cmd ) =
                                MapCard.update msg_ projection.mapCard

                            updated =
                                { projection | mapCard = mapCard_ }
                        in
                            List.setAt i updated model.projections
                                |> Maybe.map (\ps -> ( { model | projections = ps }, Cmd.map (MapCardMsg i) cmd ))
                    )
                |> Maybe.withDefault ( model, Cmd.none )
    in
        case msg of
            Nop ->
                ( model, Cmd.none )

            LoadProjections gridsetId ->
                ( { model | projectionsToLoad = Nothing, projections = [] }
                , loadProjections model.programFlags gridsetId
                )

            GotProjectionAtoms atoms ->
                ( { model | projectionsToLoad = Just (List.length atoms) }
                , atoms |> List.map (loadMetadata model.programFlags) |> Cmd.batch
                )

            GotProjection record ->
                ( model, loadOccurrenceSet record )

            NewProjectionInfo newInfo ->
                ( { model | projections = newInfo :: model.projections }, Cmd.none )

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
        |> Http.send gotProjectionAtoms


gotProjectionAtoms : Result Http.Error Decoder.AtomList -> Msg
gotProjectionAtoms result =
    case result of
        Ok (Decoder.AtomList atoms) ->
            atoms |> List.map (\(Decoder.AtomObject o) -> o) |> GotProjectionAtoms

        Err err ->
            Debug.log "Error fetching projections" (toString err) |> always Nop


loadMetadata : Flags -> Decoder.AtomObjectRecord -> Cmd Msg
loadMetadata { apiRoot } { id } =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "sdmProject/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeProjection
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotMetadata


gotMetadata : Result Http.Error Decoder.Projection -> Msg
gotMetadata result =
    case result of
        Ok (Decoder.Projection p) ->
            GotProjection p

        Err err ->
            Debug.log "Failed to load projection" err |> always Nop


view : Model -> Html Msg
view =
    .projections
        >> List.indexedMap viewMap
        >> (Options.div
                [ Options.css "display" "flex"
                , Options.css "justify-content" "space-around"
                ]
           )


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
    }
