module SDMProjection exposing (Model, update, page, init, Msg(LoadMetadata))

import List.Extra exposing (elemIndex, getAt)
import Material
import Material.Options as Options
import Material.Helpers as Helpers
import Material.Spinner as Loading
import Material.Typography as Typo
import Material.Helpers exposing (lift)
import Http
import Html exposing (Html)
import Page exposing (Page)
import Leaflet
import Decoder
    exposing
        ( ProjectionRecord
        , decodeProjection
        , Projection(..)
        , SingleLayerMap(..)
        , OccurrenceSetRecord
        , ScenarioRecord
        )
import MapCard
import AlgorithmView as Alg
import Helpers exposing (chain)
import ProgramFlags exposing (Flags)


type Tab
    = Map
    | Algorithm
    | OccurrenceSet
    | ModelScenario
    | ProjScenario


tabs : List Tab
tabs =
    [ Map, OccurrenceSet, Algorithm, ModelScenario, ProjScenario ]


tabIndex : Tab -> Int
tabIndex tab =
    tabs |> elemIndex tab |> Maybe.withDefault 0


type State
    = Showing ProjectionRecord
    | Loading


type alias Model =
    { mdl : Material.Model
    , mapCard : MapCard.Model
    , algorithm : Alg.Model
    , occurrenceSet : Maybe OccurrenceSetRecord
    , modelScenario : Maybe ScenarioRecord
    , projectionScenario : Maybe ScenarioRecord
    , state : State
    , selectedTab : Tab
    , programFlags : Flags
    }


init : Flags -> Model
init flags =
    { mdl = Material.model
    , state = Loading
    , selectedTab = Map
    , mapCard = MapCard.init Nothing
    , algorithm = Alg.init Alg.exampleAlgorithm True
    , occurrenceSet = Nothing
    , modelScenario = Nothing
    , projectionScenario = Nothing
    , programFlags = flags
    }


type Msg
    = Mdl (Material.Msg Msg)
    | MapCardMsg MapCard.Msg
    | AlgMsg Alg.Msg
    | SetState State
    | LoadMetadata Int
    | SelectTab Tab
    | SetOccurrenceSet OccurrenceSetRecord
    | SetModelScn ScenarioRecord
    | SetProjScn ScenarioRecord
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        liftedMapCardUpdate =
            Helpers.lift
                .mapCard
                (\m x -> { m | mapCard = x })
                MapCardMsg
                MapCard.update

        liftedAlgUpdate =
            Helpers.lift
                .algorithm
                (\m x -> { m | algorithm = x })
                AlgMsg
                Alg.update
    in
        case msg of
            LoadMetadata id ->
                ( { model | state = Loading }, loadMetadata model.programFlags id )

            SetState state ->
                updateState state model
                    |> chain
                        updateMap
                        loadOccurrenceSetAndScenarios

            SelectTab tab ->
                updateMap { model | selectedTab = tab }

            SetOccurrenceSet o ->
                updateMap { model | occurrenceSet = Just o }

            SetModelScn s ->
                updateMap { model | modelScenario = Just s }

            SetProjScn s ->
                updateMap { model | projectionScenario = Just s }

            Nop ->
                ( model, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ model

            MapCardMsg msg_ ->
                liftedMapCardUpdate msg_ model

            AlgMsg msg_ ->
                liftedAlgUpdate msg_ model


setMap : Maybe Leaflet.WMSInfo -> Model -> ( Model, Cmd Msg )
setMap wmsInfo =
    lift
        .mapCard
        (\m x -> { m | mapCard = x })
        MapCardMsg
        MapCard.update
        (MapCard.SetMap wmsInfo)


updateMap : Model -> ( Model, Cmd Msg )
updateMap model =
    let
        mapInfo projection =
            case model.selectedTab of
                Algorithm ->
                    Nothing

                Map ->
                    projection.map
                        |> Maybe.map
                            (\(SingleLayerMap { endpoint, mapName, layerName }) ->
                                { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
                            )

                OccurrenceSet ->
                    model.occurrenceSet
                        |> Maybe.andThen .map
                        |> Maybe.map
                            (\(SingleLayerMap { endpoint, mapName, layerName }) ->
                                { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
                            )

                ModelScenario ->
                    model.modelScenario
                        |> Maybe.andThen .map
                        |> Maybe.map
                            (\(Decoder.Map { endpoint, mapName, layers }) ->
                                let
                                    (Decoder.MapLayers ls) =
                                        layers

                                    layerNames =
                                        List.map (\(Decoder.MapLayersItem l) -> l.layerName) ls
                                in
                                    { endPoint = endpoint, mapName = mapName, layers = layerNames }
                            )

                ProjScenario ->
                    model.projectionScenario
                        |> Maybe.andThen .map
                        |> Maybe.map
                            (\(Decoder.Map { endpoint, mapName, layers }) ->
                                let
                                    (Decoder.MapLayers ls) =
                                        layers

                                    layerNames =
                                        List.map (\(Decoder.MapLayersItem l) -> l.layerName) ls
                                in
                                    { endPoint = endpoint, mapName = mapName, layers = layerNames }
                            )
    in
        case model.state of
            Showing projection ->
                setMap (mapInfo projection) model

            _ ->
                setMap Nothing model


updateState : State -> Model -> Model
updateState state model =
    let
        algorithm =
            case state of
                Showing projection ->
                    projection.algorithm
                        |> Maybe.map (Alg.fromApi True)
                        |> Maybe.withDefault (Alg.init Alg.exampleAlgorithm True)

                _ ->
                    Alg.init Alg.exampleAlgorithm True
    in
        { model | state = state, algorithm = algorithm }


loadOccurrenceSetAndScenarios : Model -> ( Model, Cmd Msg )
loadOccurrenceSetAndScenarios model =
    let
        commands =
            case model.state of
                Showing projection ->
                    [ projection.occurrenceSet
                        |> Maybe.andThen (\(Decoder.ObjectRef r) -> r.metadataUrl)
                        |> Maybe.map loadOccurrenceSet
                    , projection.modelScenario
                        |> Maybe.andThen (\(Decoder.ScenarioRef r) -> r.metadataUrl)
                        |> Maybe.map (loadScenario SetModelScn)
                    , projection.projectionScenario
                        |> Maybe.andThen (\(Decoder.ScenarioRef r) -> r.metadataUrl)
                        |> Maybe.map (loadScenario SetProjScn)
                    ]

                _ ->
                    []
    in
        model ! List.filterMap identity commands


loadOccurrenceSet : String -> Cmd Msg
loadOccurrenceSet url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeOccurrenceSet
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotOccurrenceSet


loadScenario : (ScenarioRecord -> Msg) -> String -> Cmd Msg
loadScenario andThen url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeScenario
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (gotScenario andThen)


gotOccurrenceSet : Result Http.Error Decoder.OccurrenceSet -> Msg
gotOccurrenceSet result =
    case result of
        Ok (Decoder.OccurrenceSet o) ->
            SetOccurrenceSet o

        Err err ->
            Debug.log "Failed to load occurrence set" err |> always Nop


gotScenario : (ScenarioRecord -> Msg) -> Result Http.Error Decoder.Scenario -> Msg
gotScenario andThen result =
    case result of
        Ok (Decoder.Scenario s) ->
            andThen s

        Err err ->
            Debug.log "Failed to load scenario" err |> always Nop


loadMetadata : Flags -> Int -> Cmd Msg
loadMetadata { apiRoot } id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "sdmProject/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson decodeProjection
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotMetadata


gotMetadata : Result Http.Error Projection -> Msg
gotMetadata result =
    case result of
        Ok (Projection p) ->
            SetState (Showing p)

        Err err ->
            Debug.log "Failed to load projection" err |> always Nop


tabTitle : Tab -> String
tabTitle tab =
    case tab of
        Map ->
            "Projection"

        Algorithm ->
            "Algorithm"

        OccurrenceSet ->
            "Species Data"

        ModelScenario ->
            "Model Input Layers"

        ProjScenario ->
            "Projection Input Layers"


view : Model -> Html Msg
view model =
    case model.state of
        Loading ->
            Options.div [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                [ Html.text "Loading projection..."
                , Html.p [] [ Loading.spinner [ Loading.active True ] ]
                ]

        Showing projection ->
            mainView model projection


mapTitle : Model -> String
mapTitle model =
    case model.selectedTab of
        Algorithm ->
            tabTitle model.selectedTab

        Map ->
            tabTitle model.selectedTab

        OccurrenceSet ->
            model.occurrenceSet
                |> Maybe.andThen .speciesName
                |> Maybe.withDefault (tabTitle model.selectedTab)

        ModelScenario ->
            model.modelScenario
                |> Maybe.andThen .code
                |> Maybe.withDefault (tabTitle model.selectedTab)

        ProjScenario ->
            model.projectionScenario
                |> Maybe.andThen .code
                |> Maybe.withDefault (tabTitle model.selectedTab)


mainView : Model -> ProjectionRecord -> Html Msg
mainView model proj =
    case model.selectedTab of
        Algorithm ->
            Options.div [] [ Alg.view [] model.algorithm |> Html.map AlgMsg ]

        _ ->
            Options.div [] [ MapCard.view [] (mapTitle model) model.mapCard |> Html.map MapCardMsg ]


page : Page Model Msg
page =
    { view = view
    , selectedTab = .selectedTab >> tabIndex
    , selectTab = (\i -> tabs |> getAt i |> Maybe.withDefault Map |> SelectTab)
    , tabTitles = always <| List.map (tabTitle >> Html.text) tabs
    , subscriptions = always Sub.none
    , title = "Completed Projection"
    }
