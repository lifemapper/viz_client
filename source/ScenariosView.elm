module ScenariosView exposing (..)

import Set exposing (Set)
import Decoder
    exposing
        ( AtomObjectRecord
        , AtomList(..)
        , decodeAtomList
        , AtomObject(..)
        , decodeScenario
        , Scenario(..)
        , ScenarioRecord
        , MapLayers(..)
        , MapLayersItem(..)
        )
import Material
import Material.List as L
import Material.Options as Options
import Material.Typography as Typo
import Material.Toggles as Toggles
import Material.Helpers exposing (lift)
import Html exposing (Html)
import Html.Events
import Http
import Helpers exposing (Index)
import ScenariosList as SL
import MapCard


type Mode
    = ModelScenario
    | ProjectionScenarios


type alias Model =
    { mdl : Material.Model
    , mode : Mode
    , mapScenario : Maybe ScenarioRecord
    , mapCard : MapCard.Model
    , selectedScenarios : Set Int
    }


type Msg
    = Mdl (Material.Msg Msg)
    | MapCardMsg MapCard.Msg
    | MapScenario Int
    | SetScenario (Maybe ScenarioRecord)
    | SelectScenario Int
    | UnselectScenario Int


getMetadata : Int -> Cmd Msg
getMetadata id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://notyeti-191.lifemapper.org/api/v2/scenario/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson decodeScenario
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotMetadata


gotMetadata : Result Http.Error Scenario -> Msg
gotMetadata result =
    case result of
        Ok (Scenario s) ->
            SetScenario (Just s)

        Err err ->
            SetScenario Nothing
                |> Debug.log (toString err)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        MapCardMsg msg_ ->
            lift
                .mapCard
                (\m x -> { m | mapCard = x })
                MapCardMsg
                MapCard.update
                msg_
                model

        MapScenario id ->
            ( model, getMetadata id )

        SetScenario s ->
            lift
                .mapCard
                (\m x -> { m | mapCard = x })
                MapCardMsg
                MapCard.update
                (updateMap s)
                ({ model | mapScenario = s })

        SelectScenario id ->
            let
                selectedScenarios =
                    case model.mode of
                        ProjectionScenarios ->
                            Set.insert id model.selectedScenarios

                        ModelScenario ->
                            Set.singleton id
            in
                ( { model | selectedScenarios = selectedScenarios }, Cmd.none )

        UnselectScenario id ->
            ( { model | selectedScenarios = Set.remove id model.selectedScenarios }, Cmd.none )


updateMap : Maybe ScenarioRecord -> MapCard.Msg
updateMap scenario =
    scenario
        |> Maybe.andThen (\{ map } -> map)
        |> Maybe.map
            (\(Decoder.Map { endpoint, mapName, layers }) ->
                let
                    (MapLayers ls) =
                        layers

                    layerNames =
                        List.map (\(MapLayersItem l) -> l.layerName) ls
                in
                    { endPoint = endpoint, mapName = mapName, layers = layerNames }
            )
        |> MapCard.SetMap


scenariosList : Index -> SL.Model -> Model -> Html Msg
scenariosList index availableScenarios model =
    let
        title =
            case model.mode of
                ModelScenario ->
                    "Choose Model Scenario"

                ProjectionScenarios ->
                    "Choose Projection Scenarios"
    in
        Options.div [ Options.css "margin" "20px" ]
            [ Options.styled Html.p [ Typo.title ] [ Html.text title ]
            , L.ul [] <| List.indexedMap (scenarioLI model index) availableScenarios
            ]


scenarioLI : Model -> Index -> Int -> AtomObjectRecord -> Html Msg
scenarioLI model index i s =
    let
        iconName =
            if Just s.id == Maybe.map .id model.mapScenario then
                "visibility"
            else
                "visibility_off"

        icon =
            L.icon iconName [ Options.attribute <| Html.Events.onClick (MapScenario s.id) ]

        selected =
            Set.member s.id model.selectedScenarios

        toggle =
            case model.mode of
                ModelScenario ->
                    Toggles.radio

                ProjectionScenarios ->
                    Toggles.checkbox
    in
        L.li []
            [ L.content
                []
                [ icon, Html.text s.name ]
            , L.content2 []
                [ toggle Mdl
                    (i :: index)
                    model.mdl
                    [ Toggles.value selected
                    , Toggles.group (toString index) |> Options.when (model.mode == ModelScenario)
                    , Options.onToggle (SelectScenario s.id) |> Options.when (not selected)
                    , Options.onToggle (UnselectScenario s.id) |> Options.when (selected && model.mode /= ModelScenario)
                    ]
                    []
                ]
            ]


view : Index -> SL.Model -> Model -> Html Msg
view index availableScenarios model =
    Options.div [ Options.css "display" "flex" ]
        [ scenariosList index availableScenarios model
        , MapCard.view index "Map" model.mapCard |> Html.map MapCardMsg
        ]


init : Mode -> Model
init mode =
    { mdl = Material.model
    , mode = mode
    , mapScenario = Nothing
    , mapCard = MapCard.init ("leaflet-map-scenarios-" ++ (toString mode))
    , selectedScenarios = Set.empty
    }
