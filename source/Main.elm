module Main exposing (..)

import Html exposing (Html)
import Material
import Material.Scheme
import Material.Helpers exposing (lift)
import Material.Layout as Layout
import Material.Options as Options
import ScenariosView as Scns
import AlgorithmsView as Algs


-- MODEL


type Tab
    = Algorithms
    | OccurrenceSets
    | ModelScenario
    | ProjScenarios


tabs : List Tab
tabs =
    [ Algorithms, OccurrenceSets, ModelScenario, ProjScenarios ]


tabIndex : Tab -> Int
tabIndex tab =
    tabs
        |> List.indexedMap (,)
        |> List.filter (\( i, t ) -> t == tab)
        |> List.head
        |> Maybe.map (\( i, _ ) -> i)
        |> Maybe.withDefault 0


type alias Model =
    { mdl : Material.Model
    , selectedTab : Tab
    , scenarioModel : Scns.Model
    , algorithmsModel : Algs.Model
    }


model : Model
model =
    { mdl = Material.model
    , selectedTab = ProjScenarios
    , scenarioModel = Scns.init
    , algorithmsModel = Algs.init
    }


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Tab
    | ScnsMsg Scns.Msg
    | AlgsMsg Algs.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            ( { model | selectedTab = tab }, Cmd.none )

        ScnsMsg msg_ ->
            lift
                .scenarioModel
                (\m x -> { m | scenarioModel = x })
                ScnsMsg
                Scns.update
                msg_
                model

        AlgsMsg msg_ ->
            lift
                .algorithmsModel
                (\m x -> { m | algorithmsModel = x })
                AlgsMsg
                Algs.update
                msg_
                model

        Mdl msg_ ->
            Material.update Mdl msg_ model


tabTitle : Tab -> Html msg
tabTitle tab =
    Html.text <|
        case tab of
            Algorithms ->
                "Algorithms"

            OccurrenceSets ->
                "Occurrence Sets"

            ModelScenario ->
                "Model Scenario"

            ProjScenarios ->
                "Projection Scenarios"


tabView : Tab -> Model -> Html Msg
tabView tab =
    case tab of
        Algorithms ->
            (.algorithmsModel >> Algs.view [] >> Html.map AlgsMsg)

        OccurrenceSets ->
            (\m -> Options.div [] [])

        ModelScenario ->
            (\m -> Options.div [] [])

        ProjScenarios ->
            (.scenarioModel >> Scns.view [] >> Html.map ScnsMsg)


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.fixedDrawer
        , Layout.fixedTabs
        , Layout.selectedTab <| tabIndex model.selectedTab
        , Layout.onSelectTab (\i -> List.drop i tabs |> List.head |> Maybe.withDefault Algorithms |> SelectTab)
        ]
        { header = header model
        , drawer =
            [ Layout.title [] [ Html.text "Drawer" ]
            ]
        , tabs = ( List.map tabTitle tabs, [] )
        , main = [ tabView model.selectedTab model ]
        }


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title [] [ Html.text "LifeMapper SDM" ] ]
    ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.batch [ Material.init Mdl, Cmd.map ScnsMsg Scns.getScenarios ] )
        , view = view >> Material.Scheme.top
        , subscriptions = Material.subscriptions Mdl
        , update = update
        }
