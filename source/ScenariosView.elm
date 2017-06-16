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


toApi :
    ({ scenarioId : Maybe Int, scenarioCode : Maybe String } -> scenarioType)
    -> Model
    -> List scenarioType
toApi toScenarioPOST { selectedScenarios } =
    selectedScenarios
        |> Set.toList
        |> List.map (\id -> toScenarioPOST { scenarioId = Just id, scenarioCode = Nothing })


type Msg
    = Mdl (Material.Msg Msg)
    | MapCardMsg MapCard.Msg
    | MapScenario Int
    | SetMapped (Maybe ScenarioRecord)
    | SelectScenario Int
    | UnselectScenario Int


getMetadataAndMap : Int -> Cmd Msg
getMetadataAndMap id =
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
            SetMapped (Just s)

        Err err ->
            SetMapped Nothing
                |> Debug.log (toString err)


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

            MapScenario id ->
                ( model, getMetadataAndMap id )

            SetMapped s ->
                liftedMapCardUpdate (updateMap s) ({ model | mapScenario = s })

            SelectScenario id ->
                let
                    selectedScenarios =
                        case model.mode of
                            ProjectionScenarios ->
                                Set.insert id model.selectedScenarios

                            ModelScenario ->
                                Set.singleton id
                in
                    ( { model | selectedScenarios = selectedScenarios }, getMetadataAndMap id )

            UnselectScenario id ->
                ( { model | selectedScenarios = Set.remove id model.selectedScenarios }, Cmd.none )


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

        selected =
            Set.member s.id model.selectedScenarios

        ( toggle, icon ) =
            case model.mode of
                ModelScenario ->
                    ( Toggles.radio, [] )

                ProjectionScenarios ->
                    ( Toggles.checkbox
                    , if selected then
                        [ L.icon iconName [ Options.attribute <| Html.Events.onClick (MapScenario s.id) ] ]
                      else
                        [ Options.span [ Options.css "width" "24px" ] [] ]
                    )
    in
        L.li []
            [ L.content
                []
                [ Html.text s.name ]
            , L.content2 [ Options.css "flex-flow" "row" ]
                (toggle Mdl
                    (i :: index)
                    model.mdl
                    [ Toggles.value selected
                    , Toggles.group (toString index) |> Options.when (model.mode == ModelScenario)
                    , Options.onToggle (SelectScenario s.id) |> Options.when (not selected)
                    , Options.onToggle (UnselectScenario s.id) |> Options.when (selected && model.mode /= ModelScenario)
                    ]
                    []
                    :: icon
                )
            ]


view : Index -> SL.Model -> Model -> Html Msg
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
    (Set.size model.selectedScenarios) > 0


init : Mode -> Model
init mode =
    { mdl = Material.model
    , mode = mode
    , mapScenario = Nothing
    , mapCard = MapCard.init ("leaflet-map-scenarios-" ++ (toString mode))
    , selectedScenarios = Set.empty
    }
