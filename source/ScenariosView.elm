{-
   Copyright (C) 2018, University of Kansas Center for Research

   Lifemapper Project, lifemapper [at] ku [dot] edu,
   Biodiversity Institute,
   1345 Jayhawk Boulevard, Lawrence, Kansas, 66045, USA

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.
-}


module ScenariosView
    exposing
        ( Model
        , Msg
        , init
        , update
        , view
        , toApi
        , problems
        , subscriptions
        )

import List.Extra exposing (remove)
import Maybe.Extra as Maybe exposing ((?))
import Decoder
    exposing
        ( AtomObjectRecord
        , AtomList(..)
        , decodeAtomList
        , AtomObject(..)
        , decodeScenario
        , Scenario(..)
        , ScenarioRecord
        , ScenarioPackageRecord
        , ScenarioPackageScenarios(..)
        , ScenarioMetadata(..)
        , MapLayers(..)
        , MapLayersItem(..)
        , BoomScenarioPackage(..)
        , BoomScenarioPackageModel_scenario(..)
        , BoomScenarioPackageProjection_scenario(..)
        , BoomScenarioPackageProjection_scenarioItem(..)
        )
import Material
import Material.List as L
import Material.Grid as Grid
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Options as Options
import Material.Typography as Typo
import Material.Toggles as Toggles
import Html exposing (Html)
import Helpers exposing (Index, undefined)
import ScenariosList as SL


type alias Model =
    { mdl : Material.Model
    , package : Maybe ScenarioPackageRecord
    , projectionScenarios : List ScenarioRecord
    , modelScenario : Maybe ScenarioRecord
    , mouseIn : Maybe ScenarioPackageRecord
    }


toApi : Model -> Result String BoomScenarioPackage
toApi { modelScenario, projectionScenarios } =
    case modelScenario of
        Just modelScenario ->
            Ok <|
                BoomScenarioPackage
                    { model_scenario = Just <| BoomScenarioPackageModel_scenario { scenario_code = modelScenario.code }
                    , projection_scenario =
                        projectionScenarios
                            |> List.map (\s -> BoomScenarioPackageProjection_scenarioItem { scenario_code = s.code })
                            |> BoomScenarioPackageProjection_scenario
                            |> Just
                    , scenario_package_filename = Nothing
                    , scenario_package_name = Nothing
                    }

        Nothing ->
            Err "No Model Scenario Selected"


type Msg
    = Mdl (Material.Msg Msg)
    | SelectModelScenario ScenarioPackageRecord ScenarioRecord
    | SelectProjectionScenario ScenarioPackageRecord ScenarioRecord
    | UnselectProjectionScenario ScenarioRecord
    | MouseIn (Maybe ScenarioPackageRecord)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SelectModelScenario p s ->
            if Just p == model.package then
                { model | modelScenario = Just s } ! []
            else
                { model | modelScenario = Just s, projectionScenarios = [], package = Just p } ! []

        SelectProjectionScenario p s ->
            if Just p == model.package then
                { model | projectionScenarios = (s :: model.projectionScenarios) } ! []
            else
                { model | projectionScenarios = [ s ], modelScenario = Nothing, package = Just p } ! []

        UnselectProjectionScenario id ->
            ( { model | projectionScenarios = remove id model.projectionScenarios }, Cmd.none )

        MouseIn package ->
            ( { model | mouseIn = package }, Cmd.none )


type alias ScenarioLiFunc =
    Int -> Scenario -> Html Msg


scenarioTitle : Scenario -> String
scenarioTitle (Scenario scenario) =
    Maybe.or
        (scenario.metadata |> Maybe.andThen (\(ScenarioMetadata md) -> md.title))
        scenario.code
        |> Maybe.withDefault (toString scenario.id)


listScenarios : ScenarioLiFunc -> ScenarioPackageScenarios -> Html Msg
listScenarios liFunc (ScenarioPackageScenarios scenarios) =
    scenarios
        |> List.sortBy scenarioTitle
        |> List.indexedMap liFunc
        |> L.ul []


modelScenarioLI : Index -> Model -> ScenarioPackageRecord -> Maybe ScenarioRecord -> ScenarioLiFunc
modelScenarioLI index model package currentlySelected i (Scenario s) =
    let
        isSelected =
            Just s == currentlySelected

        onToggle =
            if isSelected then
                Options.nop
            else
                Options.onToggle (SelectModelScenario package s)
    in
        L.li []
            [ L.content [] [ Html.text <| scenarioTitle (Scenario s) ]
            , L.content2 []
                [ Toggles.radio Mdl
                    (i :: index)
                    model.mdl
                    [ Toggles.value isSelected
                    , Toggles.group (toString index)
                    , onToggle
                    ]
                    []
                ]
            ]


projectionScenarioLI : Index -> Model -> ScenarioPackageRecord -> List ScenarioRecord -> Maybe ScenarioRecord -> ScenarioLiFunc
projectionScenarioLI index model package currentlySelected modelScenario i (Scenario s) =
    let
        isSelected =
            (List.member s currentlySelected) || (Just s == modelScenario)

        toggle =
            if isSelected then
                UnselectProjectionScenario s
            else
                SelectProjectionScenario package s
    in
        L.li []
            [ L.content [] [ Html.text <| scenarioTitle (Scenario s) ]
            , L.content2 []
                [ Toggles.checkbox Mdl
                    (i :: index)
                    model.mdl
                    [ Toggles.value isSelected
                    , Options.onToggle toggle
                    , Options.when (Just s == modelScenario) Toggles.disabled
                    ]
                    []
                ]
            ]


observedFilter : ScenarioPackageScenarios -> ScenarioPackageScenarios
observedFilter (ScenarioPackageScenarios scenarios) =
    scenarios
        |> List.filter (\(Scenario s) -> Maybe.map (String.startsWith "observed") s.code ? False)
        |> ScenarioPackageScenarios


packageCard : Index -> Model -> ScenarioPackageRecord -> Html Msg
packageCard index model package =
    let
        isSelected =
            model.package == Just package

        modelLI =
            if isSelected then
                modelScenarioLI (0 :: index) model package model.modelScenario
            else
                modelScenarioLI (0 :: index) model package Nothing

        projLI =
            if isSelected then
                projectionScenarioLI (1 :: index) model package model.projectionScenarios model.modelScenario
            else
                projectionScenarioLI (1 :: index) model package [] Nothing
    in
        Card.view
            [ Options.css "width" "100%"
            , if model.mouseIn == Just package then
                Elevation.e16
              else if isSelected then
                Elevation.e8
              else
                Elevation.e2
            , Options.onMouseEnter (MouseIn (Just package))
            , Options.onMouseLeave (MouseIn Nothing)
            ]
            [ Card.title [ Card.border ] [ Card.head [] [ Html.text <| "Package: " ++ (package.name ? "") ] ]
            , Card.text []
                [ Options.div [ Typo.subhead ] [ Html.text "Choose Model Layers" ]
                , listScenarios modelLI package.scenarios
                , Options.div []
                    [ Options.span [ Typo.subhead ] [ Html.text "Choose Projection Layers" ]
                    , Html.text " (The model layer is included in projections automatically.)"
                    ]
                , listScenarios projLI package.scenarios
                ]
            ]


view : Index -> SL.Model -> Model -> Html Msg
view index sl model =
    Options.div []
        [ Options.styled Html.p
            [ Options.css "padding" "20px", Options.css "margin" "0" ]
            [ Html.text <|
                "Note: All layers must be chosen from a single package. "
                    ++ "Previous selections will be cleared when a layer is selected from "
                    ++ "a different package."
            ]
        , sl.packages
            |> List.sortBy .id
            -- just so the packages appear in a consistent order between page loads
            |>
                List.indexedMap (\i package -> Grid.cell [ Grid.size Grid.All 4 ] [ packageCard (i :: index) model package ])
            |> Grid.grid []
        ]


problems : Model -> Maybe String
problems model =
    if model.modelScenario == Nothing then
        Just "No model layers selected."
    else
        Nothing


init : Model
init =
    { mdl = Material.model
    , package = Nothing
    , modelScenario = Nothing
    , projectionScenarios = []
    , mouseIn = Nothing
    }


subscriptions : (Msg -> msg) -> Sub msg
subscriptions liftMsg =
    Sub.none
