module ScenariosView exposing (..)

import List.Extra exposing (remove)
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
import Helpers exposing (Index)
import MapCard


type Mode
    = ModelScenario
    | ProjectionScenarios


type alias Model =
    { mdl : Material.Model
    , mode : Mode
    , mapScenario : Maybe ScenarioRecord
    , mapCard : MapCard.Model
    , selectedScenarios : List ScenarioRecord
    }


toApi :
    ({ scenarioId : Maybe Int, scenarioCode : Maybe String } -> scenarioType)
    -> Model
    -> List scenarioType
toApi toScenarioPOST { selectedScenarios } =
    selectedScenarios
        |> List.map (\s -> toScenarioPOST { scenarioId = Just s.id, scenarioCode = s.code })


type Msg
    = Mdl (Material.Msg Msg)
    | MapCardMsg MapCard.Msg
    | MapScenario ScenarioRecord
    | SelectScenario ScenarioRecord
    | UnselectScenario ScenarioRecord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        liftedMapCardUpdate =
            lift
                .mapCard
                (\m x -> { m | mapCard = x })
                MapCardMsg
                MapCard.update
    in
        case msg of
            Mdl msg_ ->
                Material.update Mdl msg_ model

            MapCardMsg msg_ ->
                liftedMapCardUpdate msg_ model

            MapScenario s ->
                liftedMapCardUpdate (updateMap (Just s)) ({ model | mapScenario = (Just s) })

            SelectScenario s ->
                let
                    selectedScenarios =
                        case model.mode of
                            ProjectionScenarios ->
                                s :: model.selectedScenarios

                            ModelScenario ->
                                [ s ]
                in
                    liftedMapCardUpdate (updateMap (Just s))
                        { model
                            | selectedScenarios = selectedScenarios
                            , mapScenario = (Just s)
                        }

            UnselectScenario id ->
                ( { model | selectedScenarios = remove id model.selectedScenarios }, Cmd.none )


updateMap : Maybe ScenarioRecord -> MapCard.Msg
updateMap =
    Maybe.andThen (\{ map } -> map)
        >> Maybe.map
            (\(Decoder.Map { endpoint, mapName, layers }) ->
                let
                    (MapLayers ls) =
                        layers

                    layerNames =
                        List.map (\(MapLayersItem l) -> l.layerName) ls
                in
                    { endPoint = endpoint, mapName = mapName, layers = layerNames }
            )
        >> MapCard.SetMap


scenariosList : Index -> List ScenarioRecord -> Model -> Html Msg
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


scenarioLI : Model -> Index -> Int -> ScenarioRecord -> Html Msg
scenarioLI model index i s =
    let
        iconName =
            if Just s.id == Maybe.map .id model.mapScenario then
                "visibility"
            else
                "visibility_off"

        selected =
            List.member s model.selectedScenarios

        ( toggle, icon ) =
            case model.mode of
                ModelScenario ->
                    ( Toggles.radio, [] )

                ProjectionScenarios ->
                    ( Toggles.checkbox
                    , if selected then
                        [ L.icon iconName [ Options.attribute <| Html.Events.onClick (MapScenario s) ] ]
                      else
                        [ Options.span [ Options.css "width" "24px" ] [] ]
                    )
    in
        L.li []
            [ L.content
                []
                [ Html.text <| Maybe.withDefault "" s.code ]
            , L.content2 [ Options.css "flex-flow" "row" ]
                (toggle Mdl
                    (i :: index)
                    model.mdl
                    [ Toggles.value selected
                    , Toggles.group (toString index) |> Options.when (model.mode == ModelScenario)
                    , Options.onToggle (SelectScenario s) |> Options.when (not selected)
                    , Options.onToggle (UnselectScenario s) |> Options.when (selected && model.mode /= ModelScenario)
                    ]
                    []
                    :: icon
                )
            ]


view : Index -> List ScenarioRecord -> Model -> Html Msg
view index availableScenarios model =
    let
        mapCardTitle =
            model.mapScenario |> Maybe.andThen .code |> Maybe.withDefault "Map"
    in
        Options.div [ Options.css "display" "flex" ]
            [ scenariosList index availableScenarios model
            , MapCard.view index mapCardTitle model.mapCard |> Html.map MapCardMsg
            ]


complete : Model -> Bool
complete model =
    (List.length model.selectedScenarios) > 0


init : Mode -> Model
init mode =
    { mdl = Material.model
    , mode = mode
    , mapScenario = Nothing
    , mapCard = MapCard.init ("leaflet-map-scenarios-" ++ (toString mode))
    , selectedScenarios = []
    }
