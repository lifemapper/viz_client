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
import Material.List as L
import Material.Options as Options
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Typography as Typo
import Material.Toggles as Toggles
import Material.Helpers exposing (lift)
import Html exposing (Html)
import Html.Events
import Http
import Helpers exposing (Index)
import Leaflet


type alias Model =
    { mdl : Material.Model
    , scenarios : List AtomObjectRecord
    , mapScenario : Maybe ScenarioRecord
    , selectedScenarios : Set Int
    , leafletModel : Leaflet.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | GotScenarioList (Result Http.Error AtomList)
    | MapScenario Int
    | GotScenario (Result Http.Error Scenario)
    | SelectScenario Int
    | UnselectScenario Int
    | LeafletMsg Leaflet.Msg


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

        GotScenario (Ok (Scenario s)) ->
            updateMap ({ model | mapScenario = Just s })

        GotScenario (Err err) ->
            Debug.log (toString err) ( model, Cmd.none )

        SelectScenario id ->
            ( { model | selectedScenarios = Set.insert id model.selectedScenarios }
            , Cmd.none
            )

        UnselectScenario id ->
            ( { model | selectedScenarios = Set.remove id model.selectedScenarios }
            , Cmd.none
            )

        LeafletMsg msg_ ->
            lift
                .leafletModel
                (\m x -> { m | leafletModel = x })
                LeafletMsg
                Leaflet.update
                msg_
                model


updateMap : Model -> ( Model, Cmd Msg )
updateMap model =
    case model.mapScenario of
        Nothing ->
            updateLeaflet "" "" [] model

        Just { map } ->
            case map of
                Nothing ->
                    updateLeaflet "" "" [] model

                Just (Decoder.Map { endpoint, mapName, layers }) ->
                    let
                        (MapLayers ls) =
                            layers

                        layerNames =
                            List.map (\(MapLayersItem l) -> l.layerName) ls
                    in
                        -- updateLeaflet endpoint mapName (List.take 1 layerNames) model
                        updateLeaflet endpoint mapName ("bmng" :: List.take 1 layerNames) model


updateLeaflet : String -> String -> List String -> Model -> ( Model, Cmd Msg )
updateLeaflet endpoint mapName layers model =
    lift
        .leafletModel
        (\m x -> { m | leafletModel = x })
        LeafletMsg
        Leaflet.update
        (Leaflet.SetMap endpoint mapName layers)
        model


leafletDiv : Html Msg
leafletDiv =
    Options.div [ Options.id "leaflet-map", Options.css "width" "800px", Options.css "height" "600px" ] []


mapCard : Model -> Html Msg
mapCard model =
    Card.view
        [ Elevation.e2
        , Options.css "width" "880px"
        , Options.css "height" "660px"
        , Options.css "margin" "20px"
        ]
        [ Card.title []
            [ Card.head []
                [ model.mapScenario
                    |> Maybe.andThen .code
                    |> Maybe.withDefault "Map"
                    |> Html.text
                ]
            ]
        , Card.text [] [ leafletDiv ]
        ]


scenariosList : Index -> Model -> Html Msg
scenariosList index model =
    Options.div [ Options.css "margin" "20px" ]
        [ Options.styled Html.p [ Typo.title ] [ Html.text "Choose Projection Scenarios" ]
        , L.ul [] <| List.indexedMap (scenarioLI model index) model.scenarios
        ]


scenarioLI : Model -> Index -> Int -> AtomObjectRecord -> Html Msg
scenarioLI model index i s =
    let
        icon =
            if Just s.id == Maybe.map .id model.mapScenario then
                L.icon "visibility" []
            else
                L.icon "visibility_off" []

        selected =
            Set.member s.id model.selectedScenarios
    in
        L.li []
            [ L.content
                [ Options.attribute <| Html.Events.onClick (MapScenario s.id) ]
                [ icon, Html.text s.name ]
            , L.content2 []
                [ Toggles.checkbox Mdl
                    (i :: index)
                    model.mdl
                    [ Toggles.value selected
                    , Options.onToggle (SelectScenario s.id) |> Options.when (not selected)
                    , Options.onToggle (UnselectScenario s.id) |> Options.when selected
                    ]
                    []
                ]
            ]


view : Index -> Model -> Html Msg
view index model =
    Options.div [ Options.css "display" "flex" ]
        [ scenariosList index model
        , mapCard model
        ]


init : Model
init =
    { mdl = Material.model
    , scenarios = []
    , mapScenario = Nothing
    , selectedScenarios = Set.empty
    , leafletModel = Leaflet.init
    }


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.batch [ Material.init Mdl, getScenarios ] )
        , view = view [] >> Material.Scheme.top
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
