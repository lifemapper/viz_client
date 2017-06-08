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
import Material.Scheme
import Material.Menu as Menu
import Material.List as L
import Material.Icon as Icon
import Material.Options as Options
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Typography as Typo
import Material.Toggles as Toggles
import Html exposing (Html)
import Html.Events
import Http
import Helpers exposing (Index)
import Leaflet exposing (setLeafletMap, clearLeafletMap)


type Mode
    = ModelScenario
    | ProjectionScenarios


type alias Model =
    { mdl : Material.Model
    , mode : Mode
    , scenarios : List AtomObjectRecord
    , mapScenario : Maybe ScenarioRecord
    , mapLayer : Int
    , selectedScenarios : Set Int
    }


type Msg
    = Mdl (Material.Msg Msg)
    | GotScenarioList (Result Http.Error AtomList)
    | MapScenario Int
    | MapLayer Int
    | GotScenario (Result Http.Error Scenario)
    | SelectScenario Int
    | UnselectScenario Int


getScenarios : Cmd Msg
getScenarios =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://notyeti-191.lifemapper.org/api/v2/scenario"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send GotScenarioList


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
        |> Http.send GotScenario


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        GotScenarioList (Ok (AtomList atoms)) ->
            ( { model | scenarios = atoms |> List.map (\(AtomObject o) -> o) }, Cmd.none )

        GotScenarioList (Err err) ->
            Debug.log (toString err) ( model, Cmd.none )

        MapScenario id ->
            ( model, getMetadata id )

        MapLayer i ->
            let
                newModel =
                    { model | mapLayer = i }
            in
                ( newModel, updateMap newModel )

        GotScenario (Ok (Scenario s)) ->
            let
                newModel =
                    { model | mapScenario = Just s, mapLayer = 0 }
            in
                ( newModel, updateMap newModel )

        GotScenario (Err err) ->
            Debug.log (toString err) ( model, Cmd.none )

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


updateMap : Model -> Cmd Msg
updateMap model =
    case mapInfo model of
        Nothing ->
            clearLeafletMap (mapContainerId model)

        Just ( endpoint, mapName, layerNames ) ->
            setLeafletMap
                { containerId = (mapContainerId model)
                , endPoint = endpoint
                , mapName = mapName
                , layers = ("bmng" :: (List.take 1 <| List.drop model.mapLayer layerNames))
                }


mapInfo : Model -> Maybe ( String, String, List String )
mapInfo model =
    model.mapScenario
        |> Maybe.andThen (\{ map } -> map)
        |> Maybe.map
            (\(Decoder.Map { endpoint, mapName, layers }) ->
                let
                    (MapLayers ls) =
                        layers

                    layerNames =
                        List.map (\(MapLayersItem l) -> l.layerName) ls
                in
                    ( endpoint, mapName, layerNames )
            )


mapContainerId : Model -> String
mapContainerId model =
    "leaflet-map-" ++ (toString model.mode)


leafletDiv : Model -> Html Msg
leafletDiv model =
    Options.div
        [ Options.id (mapContainerId model)
        , Options.cs "leaflet-map"
        , Options.css "width" "800px"
        , Options.css "height" "600px"
        , Options.css "margin-left" "auto"
        , Options.css "margin-right" "auto"
        ]
        []


mapCard : Index -> Model -> Html Msg
mapCard index model =
    let
        layerNames =
            mapInfo model
                |> Maybe.map (\( _, _, ls ) -> ls)
                |> Maybe.withDefault []

        checkmark x =
            if x then
                Icon.view "check" [ Options.css "width" "40px" ]
            else
                Options.span [ Options.css "width" "40px" ] []

        menuItem i layer =
            Menu.item [ Menu.onSelect (MapLayer i) ]
                [ checkmark (i == model.mapLayer)
                , Html.text layer
                ]
    in
        Card.view
            [ Elevation.e2
            , Options.css "width" "800px"
            , Options.css "margin" "20px"
            ]
            [ Card.title [ Card.border ]
                [ Card.head []
                    [ model.mapScenario
                        |> Maybe.andThen .code
                        |> Maybe.withDefault "Map"
                        |> Html.text
                    ]
                ]
            , Card.menu []
                [ Menu.render Mdl
                    (-1 :: index)
                    model.mdl
                    [ Menu.bottomRight ]
                    (List.indexedMap menuItem layerNames)
                ]
            , Card.text [ Options.css "padding" "0", Options.css "width" "100%" ] [ leafletDiv model ]
            ]


scenariosList : Index -> Model -> Html Msg
scenariosList index model =
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
            , L.ul [] <| List.indexedMap (scenarioLI model index) model.scenarios
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


view : Index -> Model -> Html Msg
view index model =
    Options.div [ Options.css "display" "flex" ]
        [ scenariosList index model
        , mapCard index model
        ]


init : Mode -> Model
init mode =
    { mdl = Material.model
    , mode = mode
    , scenarios = []
    , mapScenario = Nothing
    , mapLayer = 0
    , selectedScenarios = Set.empty
    }


main : Program Never Model Msg
main =
    Html.program
        { init = ( init ProjectionScenarios, Cmd.batch [ Material.init Mdl, getScenarios ] )
        , view = view [] >> Material.Scheme.top
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
