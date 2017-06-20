module ExistingSDM exposing (Model, update, page, init, Msg(LoadMetadata))

import Material
import Material.Options as Options
import Material.Helpers as Helpers
import Http
import Html exposing (Html)
import Page exposing (Page)
import Decoder exposing (ProjectionRecord, decodeProjection, Projection(..), SingleLayerMap(..))
import MapCard
import AlgorithmView as Alg


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
    , state : State
    , selectedTab : Tab
    }


init : Model
init =
    { mdl = Material.model
    , state = Blank
    , selectedTab = Map
    , mapCard = MapCard.init "leaflet-map-projection"
    , algorithm = Alg.init Alg.exampleAlgorithm
    }


type Msg
    = Mdl (Material.Msg Msg)
    | MapCardMsg MapCard.Msg
    | AlgMsg Alg.Msg
    | SetState State
    | LoadMetadata Int
    | SelectTab Tab
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
                liftedMapCardUpdate (updateMap state) (updateState state model)

            SelectTab tab ->
                ( { model | selectedTab = tab }, Cmd.none )

            Nop ->
                ( model, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ model

            MapCardMsg msg_ ->
                liftedMapCardUpdate msg_ model

            AlgMsg msg_ ->
                liftedAlgUpdate msg_ model


updateState : State -> Model -> Model
updateState state model =
    let
        algorithm =
            case state of
                Showing projection ->
                    projection.algorithm
                        |> Maybe.map Alg.fromApi
                        |> Maybe.withDefault (Alg.init Alg.exampleAlgorithm)

                _ ->
                    Alg.init Alg.exampleAlgorithm
    in
        { model | state = state, algorithm = algorithm }


updateMap : State -> MapCard.Msg
updateMap state =
    case state of
        Showing { map } ->
            map
                |> Maybe.map
                    (\(SingleLayerMap { endpoint, mapName, layerName }) ->
                        { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
                    )
                |> MapCard.SetMap

        _ ->
            MapCard.SetMap Nothing


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
            Options.div [] [ MapCard.view [] "Map" model.mapCard |> Html.map MapCardMsg ]

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
