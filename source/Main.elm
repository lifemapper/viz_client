module Main exposing (..)

import Html exposing (Html)
import Material
import Material.Scheme
import Material.Helpers exposing (lift)
import Material.Layout as Layout
import Material.Options as Options
import ScenariosView as Scns
import AlgorithmsView as Algs
import Leaflet


-- MODEL


type Tab
    = Algorithms
    | OccurrenceSets
    | Scenario


tabs : List Tab
tabs =
    [ Algorithms, OccurrenceSets, Scenario ]


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
    , mapModel : Leaflet.Model
    }


model : Model
model =
    { mdl = Material.model
    , selectedTab = Algorithms
    , scenarioModel = Scns.init
    , algorithmsModel = Algs.init
    , mapModel = Leaflet.init
    }


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Tab
    | ScnsMsg Scns.Msg
    | AlgsMsg Algs.Msg
    | MapMsg Leaflet.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            lift
                .mapModel
                (\m x -> { m | mapModel = x })
                MapMsg
                Leaflet.update
                (if tab == Scenario then
                    Leaflet.Draw { lat = 0, lon = 0, zoom = 1 }
                 else
                    Leaflet.Destroy
                )
                ({ model | selectedTab = tab })

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

        MapMsg msg_ ->
            lift
                .mapModel
                (\m x -> { m | mapModel = x })
                MapMsg
                Leaflet.update
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

            Scenario ->
                "Scenario"


tabView : Tab -> Model -> Html Msg
tabView tab =
    case tab of
        Algorithms ->
            (.algorithmsModel >> Algs.view [] >> Html.map AlgsMsg)

        OccurrenceSets ->
            (\m -> Options.div [] [])

        Scenario ->
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
        , main =
            [ Options.div
                [ Options.id "leaflet-map"
                , Options.css "width" "800px"
                , Options.css "height" "600px"
                -- , Options.css "display" "none" |> Options.when (model.selectedTab /= Scenario)
                ]
                []
            , tabView model.selectedTab model
            ]
        }


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title [] [ Html.text "LifeMapper SDM" ] ]
    ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Material.init Mdl )
        , view = view >> Material.Scheme.top
        , subscriptions = Material.subscriptions Mdl
        , update = update
        }
