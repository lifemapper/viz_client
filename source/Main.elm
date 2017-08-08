module Main exposing (..)

import Time
import Material
import Material.Layout as Layout
import Material.Typography as Typo
import Material.Options as Options
import Material.Helpers exposing (lift)
import Html exposing (Html)
import Navigation as Nav exposing (Location)
import UrlParser as Url exposing ((</>))
import Http
import Page
import NewSDM
import SDMResults
import ProgramFlags exposing (Flags)
import Decoder
    exposing
        ( AtomObjectRecord
        , AtomList(..)
        , AtomObject(..)
        , decodeAtomList
        )


type SDMPage
    = NewSDM
    | SDMResults Int


route : Url.Parser (SDMPage -> a) a
route =
    Url.oneOf
        [ Url.map NewSDM (Url.s "new")
        , Url.map SDMResults (Url.s "results" </> Url.int)
        ]


type GridSets
    = GridSetsLoading
    | GridSetsList (List AtomObjectRecord)


type alias Model =
    { mdl : Material.Model
    , page : SDMPage
    , gridsets : GridSets
    , newSDM : NewSDM.Model
    , results : SDMResults.Model
    , flags : Flags
    }


init : Flags -> Model
init flags =
    { mdl = Material.model
    , page = NewSDM
    , gridsets = GridSetsLoading
    , newSDM = NewSDM.init flags
    , results = SDMResults.init flags
    , flags = flags
    }


type Msg
    = Mdl (Material.Msg Msg)
    | NewSDMMsg NewSDM.Msg
    | SDMResultsMsg SDMResults.Msg
    | GotGridSets (List AtomObjectRecord)
    | UrlChange Location
    | OpenExisting Int
    | OpenNew
    | Tick Time.Time
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        liftedNewSDMUpdate =
            lift
                .newSDM
                (\m x -> { m | newSDM = x })
                NewSDMMsg
                NewSDM.update

        liftedSDMResultsUpdate =
            lift
                .results
                (\m x -> { m | results = x })
                SDMResultsMsg
                SDMResults.update
    in
        case msg of
            Mdl msg_ ->
                Material.update Mdl msg_ model

            NewSDMMsg msg_ ->
                liftedNewSDMUpdate msg_ model

            SDMResultsMsg msg_ ->
                liftedSDMResultsUpdate msg_ model

            UrlChange loc ->
                case Debug.log "path" (Url.parseHash route loc) of
                    Just NewSDM ->
                        ( { model | page = NewSDM }, Cmd.none )

                    Just (SDMResults id) ->
                        liftedSDMResultsUpdate (SDMResults.LoadProjections id) { model | page = SDMResults id }

                    Nothing ->
                        ( model, Cmd.none )

            OpenExisting id ->
                model ! [ Nav.newUrl ("#results/" ++ toString id) ]

            OpenNew ->
                model ! [ Nav.newUrl "#new" ]

            Tick _ ->
                ( model, getSDMProjections model.flags )

            GotGridSets gridsets ->
                ( { model | gridsets = GridSetsList gridsets }, Cmd.none )

            Nop ->
                ( model, Cmd.none )


getSDMProjections : Flags -> Cmd Msg
getSDMProjections { apiRoot } =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "gridset?user=anon"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotGridSets


gotGridSets : Result Http.Error AtomList -> Msg
gotGridSets result =
    case result of
        Ok (AtomList atoms) ->
            atoms
                |> List.map (\(AtomObject o) -> o)
                |> List.sortBy .modificationTime
                |> List.reverse
                |> GotGridSets

        Err err ->
            Debug.log "Error fetching gridsets" (toString err) |> always Nop


header : String -> List (Html Msg)
header title =
    [ Layout.row []
        [ Layout.title []
            [ Html.text "Lifemapper SDM | "
            , Options.span [ Typo.subhead ] [ Html.text title ]
            ]
        ]
    ]


drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.title [] [ Html.text "Lifemapper SDM" ]
    , Layout.navigation []
        [ Layout.link [ Options.onClick OpenNew ] [ Html.text "New SDM Project" ] ]
    , Layout.title [ Typo.subhead ] [ Html.text "Completed" ]
    , case model.gridsets of
        GridSetsLoading ->
            Html.text "Loading..."

        GridSetsList list ->
            list
                |> List.map (\{ name, id } -> Layout.link [ Options.onClick (OpenExisting id) ] [ Html.text name ])
                |> Layout.navigation []
    ]


pageImplementation : SDMPage -> Page.Page Model Msg
pageImplementation p =
    case p of
        NewSDM ->
            Page.lift NewSDM.page .newSDM NewSDMMsg

        SDMResults id ->
            Page.lift SDMResults.page .results SDMResultsMsg


view : Model -> Html Msg
view model =
    let
        page =
            pageImplementation model.page
    in
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedDrawer
            , Layout.fixedTabs
            , Layout.selectedTab (page.selectedTab model)
            , Layout.onSelectTab page.selectTab
            ]
            { header = header page.title
            , drawer = drawer model
            , tabs = ( page.tabTitles model, [] )
            , main = [ page.view model ]
            }


start : Flags -> Location -> ( Model, Cmd Msg )
start flags loc =
    let
        ( model, msg ) =
            update (UrlChange loc) (init flags)
    in
        model
            ! [ Material.init Mdl
              , NewSDM.initCmd flags NewSDMMsg
              , getSDMProjections flags
              , msg
              ]


subPageSubs : Model -> Sub Msg
subPageSubs model =
    [ NewSDM, SDMResults 0 ]
        |> List.map (pageImplementation >> .subscriptions)
        |> List.map (\subs -> subs model)
        |> Sub.batch


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdl model
        , subPageSubs model
        , case model.flags.completedPollingSeconds of
            Just secs ->
                Time.every (secs * Time.second) Tick

            Nothing ->
                Sub.none
        ]


main : Program Flags Model Msg
main =
    Nav.programWithFlags
        UrlChange
        { init = start
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
