module NewSDM exposing (Model, page, init, initCmd, update, Msg)

import Html exposing (Html)
import Material
import Material.Helpers exposing (lift)
import ScenariosView as Scns
import AlgorithmsView as Algs
import OccurrenceSetsView as Occs
import Page exposing (Page)
import ScenariosList as SL


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
    , modelScenario : Scns.Model
    , projectionScenarios : Scns.Model
    , algorithmsModel : Algs.Model
    , occurrenceSets : Occs.Model
    , availableScenarios : SL.Model
    }


init : Model
init =
    { mdl = Material.model
    , selectedTab = Algorithms
    , modelScenario = Scns.init Scns.ModelScenario
    , projectionScenarios = Scns.init Scns.ProjectionScenarios
    , algorithmsModel = Algs.init
    , occurrenceSets = Occs.init
    , availableScenarios = SL.init
    }


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Tab
    | ProjScnsMsg Scns.Msg
    | MdlScnMsg Scns.Msg
    | AlgsMsg Algs.Msg
    | OccsMsg Occs.Msg
    | SLMsg SL.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            ( { model | selectedTab = tab }, Cmd.none )

        ProjScnsMsg msg_ ->
            lift
                .projectionScenarios
                (\m x -> { m | projectionScenarios = x })
                ProjScnsMsg
                Scns.update
                msg_
                model

        MdlScnMsg msg_ ->
            lift
                .modelScenario
                (\m x -> { m | modelScenario = x })
                MdlScnMsg
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

        OccsMsg msg_ ->
            lift
                .occurrenceSets
                (\m x -> { m | occurrenceSets = x })
                OccsMsg
                Occs.update
                msg_
                model

        SLMsg msg_ ->
            lift
                .availableScenarios
                (\m x -> { m | availableScenarios = x })
                SLMsg
                SL.update
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


mainView : Model -> Html Msg
mainView model =
    case model.selectedTab of
        Algorithms ->
            model.algorithmsModel |> Algs.view [] |> Html.map AlgsMsg

        OccurrenceSets ->
            model.occurrenceSets |> Occs.view [] |> Html.map OccsMsg

        ModelScenario ->
            model.modelScenario |> Scns.view [ 0 ] (model.availableScenarios) |> Html.map MdlScnMsg

        ProjScenarios ->
            model.projectionScenarios |> Scns.view [ 0 ] (model.availableScenarios) |> Html.map ProjScnsMsg


selectedTab : Model -> Int
selectedTab model =
    tabIndex model.selectedTab


selectTab : Int -> Msg
selectTab i =
    List.drop i tabs |> List.head |> Maybe.withDefault Algorithms |> SelectTab


tabTitles : Model -> List (Html msg)
tabTitles model =
    List.map tabTitle tabs


page : Page Model Msg
page =
    { view = mainView
    , selectedTab = selectedTab
    , selectTab = selectTab
    , tabTitles = tabTitles
    }


initCmd : (Msg -> msg) -> Cmd msg
initCmd map =
    SL.getScenarios SLMsg |> Cmd.map map

