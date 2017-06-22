module SDMProjection exposing (Model, update, page, init, Msg(LoadMetadata))

import Material
import Material.Options as Options
import Material.Helpers as Helpers
import Http
import Html exposing (Html)
import Page exposing (Page)
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


type Tab
    = Map
    | Algorithm
    | OccurrenceSet
    | ModelScenario
    | ProjScenario


tabs : List Tab
tabs =
    [ Map, Algorithm, OccurrenceSet, ModelScenario, ProjScenario ]


tabIndex : Tab -> Int
tabIndex tab =
    tabs
        |> List.indexedMap (,)
        |> List.filter (\( i, t ) -> t == tab)
        |> List.head
        |> Maybe.map (\( i, _ ) -> i)
        |> Maybe.withDefault 0


type State
    = Showing ProjectionRecord
    | Loading Int
    | Blank


type alias Model =
    { mdl : Material.Model
    , mapCard : MapCard.Model
    , algorithm : Alg.Model
    , occurrenceSet : Maybe OccurrenceSetRecord
    , modelScenario : Maybe ScenarioRecord
    , projectionScenario : Maybe ScenarioRecord
    , state : State
    , selectedTab : Tab
    }


init : Model
init =
    { mdl = Material.model
    , state = Blank
    , selectedTab = Map
    , mapCard = MapCard.init "leaflet-map-projection"
    , algorithm = Alg.init Alg.exampleAlgorithm True
    , occurrenceSet = Nothing
    , modelScenario = Nothing
    , projectionScenario = Nothing
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
                ( { model | state = Loading id }, loadMetadata id )

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


updateMap : Model -> ( Model, Cmd Msg )
updateMap model =
    let
        liftedMapCardUpdate =
            Helpers.lift
                .mapCard
                (\m x -> { m | mapCard = x })
                MapCardMsg
                MapCard.update

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

        mapMsg =
            case model.state of
                Showing projection ->
                    MapCard.SetMap (mapInfo projection)

                _ ->
                    MapCard.SetMap Nothing
    in
        liftedMapCardUpdate mapMsg model


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
            SetState Blank
                |> Debug.log (toString err)


gotScenario : (ScenarioRecord -> Msg) -> Result Http.Error Decoder.Scenario -> Msg
gotScenario andThen result =
    case result of
        Ok (Decoder.Scenario s) ->
            andThen s

        Err err ->
            SetState Blank
                |> Debug.log (toString err)


loadMetadata : Int -> Cmd Msg
loadMetadata id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://notyeti-191.lifemapper.org/api/v2/sdmProject/" ++ (toString id)
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
            SetState Blank
                |> Debug.log (toString err)


tabTitle : Tab -> Html msg
tabTitle tab =
    Html.text <|
        case tab of
            Map ->
                "Map"

            Algorithm ->
                "Algorithm"

            OccurrenceSet ->
                "OccurrenceSet"

            ModelScenario ->
                "Model Scenario"

            ProjScenario ->
                "Projection Scenario"


view : Model -> Html Msg
view model =
    case model.state of
        Blank ->
            Options.div [] []

        Loading _ ->
            Options.div [] [ Html.text "Loading" ]

        Showing projection ->
            mainView model projection


mainView : Model -> ProjectionRecord -> Html Msg
mainView model proj =
    case model.selectedTab of
        Map ->
            Options.div [] [ MapCard.view [] "Projection" model.mapCard |> Html.map MapCardMsg ]

        Algorithm ->
            Options.div [] [ Alg.view [] model.algorithm |> Html.map AlgMsg ]

        OccurrenceSet ->
            Options.div [] [ MapCard.view [] "Map" model.mapCard |> Html.map MapCardMsg ]

        ModelScenario ->
            Options.div [] [ MapCard.view [] "Map" model.mapCard |> Html.map MapCardMsg ]

        ProjScenario ->
            Options.div [] [ MapCard.view [] "Map" model.mapCard |> Html.map MapCardMsg ]


page : Page Model Msg
page =
    { view = view
    , selectedTab = .selectedTab >> tabIndex
    , selectTab = (\i -> List.drop i tabs |> List.head |> Maybe.withDefault Map |> SelectTab)
    , tabTitles = always <| List.map tabTitle tabs
    }
