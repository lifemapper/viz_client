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
import SDMProjection
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
    | SDMProjection Int
    | SDMResults Int


route : Url.Parser (SDMPage -> a) a
route =
    Url.oneOf
        [ Url.map NewSDM (Url.s "new")
        , Url.map SDMProjection (Url.s "projection" </> Url.int)
        , Url.map SDMResults (Url.s "results" </> Url.int)
        ]


type alias Model =
    { mdl : Material.Model
    , page : SDMPage
    , sdmProjections : List AtomObjectRecord
    , sdmProjection : SDMProjection.Model
    , newSDM : NewSDM.Model
    , results : SDMResults.Model
    , flags : Flags
    }


init : Flags -> Model
init flags =
    { mdl = Material.model
    , page = NewSDM
    , sdmProjections = []
    , sdmProjection = SDMProjection.init flags
    , newSDM = NewSDM.init flags
    , results = SDMResults.init flags
    , flags = flags
    }


type Msg
    = Mdl (Material.Msg Msg)
    | NewSDMMsg NewSDM.Msg
    | SDMProjectionMsg SDMProjection.Msg
    | SDMResultsMsg SDMResults.Msg
    | GotSDMProjections (List AtomObjectRecord)
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

        liftedSDMProjectionUpdate =
            lift
                .sdmProjection
                (\m x -> { m | sdmProjection = x })
                SDMProjectionMsg
                SDMProjection.update

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

            SDMProjectionMsg msg_ ->
                liftedSDMProjectionUpdate msg_ model

            SDMResultsMsg msg_ ->
                liftedSDMResultsUpdate msg_ model

            UrlChange loc ->
                case Debug.log "path" (Url.parseHash route loc) of
                    Just NewSDM ->
                        ( { model | page = NewSDM }, Cmd.none )

                    Just (SDMProjection id) ->
                        liftedSDMProjectionUpdate (SDMProjection.LoadMetadata id) { model | page = SDMProjection id }

                    Just (SDMResults id) ->
                        liftedSDMResultsUpdate (SDMResults.LoadProjections id) { model | page = SDMResults id }

                    Nothing ->
                        ( model, Cmd.none )

            OpenExisting id ->
                model ! [ Nav.newUrl ("#projection/" ++ toString id) ]

            OpenNew ->
                model ! [ Nav.newUrl "#new" ]

            Tick _ ->
                ( model, getSDMProjections model.flags )

            GotSDMProjections projections ->
                ( { model | sdmProjections = projections }, Cmd.none )

            Nop ->
                ( model, Cmd.none )


getSDMProjections : Flags -> Cmd Msg
getSDMProjections { apiRoot } =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "sdmProject?status=300"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotSDMProjections


gotSDMProjections : Result Http.Error AtomList -> Msg
gotSDMProjections result =
    case result of
        Ok (AtomList atoms) ->
            atoms
                |> List.map (\(AtomObject o) -> o)
                |> List.sortBy .modificationTime
                |> List.reverse
                |> GotSDMProjections

        Err err ->
            Debug.log "Error fetching existing SDMs" (toString err) |> always Nop


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title [] [ Html.text "Lifemapper SDM" ] ]
    ]


drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.title [] [ Html.text "Lifemapper SDM" ]
    , Layout.navigation []
        [ Layout.link [ Options.onClick OpenNew ] [ Html.text "New SDM Project" ] ]
    , Layout.title [ Typo.subhead ] [ Html.text "Completed" ]
    , model.sdmProjections
        |> List.map (\{ name, id } -> Layout.link [ Options.onClick (OpenExisting id) ] [ Html.text name ])
        |> Layout.navigation []
    ]


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                NewSDM ->
                    Page.lift NewSDM.page .newSDM NewSDMMsg

                SDMProjection id ->
                    Page.lift SDMProjection.page .sdmProjection SDMProjectionMsg

                SDMResults id ->
                    Page.lift SDMResults.page .results SDMResultsMsg
    in
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedDrawer
            , Layout.fixedTabs
            , Layout.selectedTab (page.selectedTab model)
            , Layout.onSelectTab page.selectTab
            ]
            { header = header model
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdl model
        , NewSDM.subscriptions NewSDMMsg
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
