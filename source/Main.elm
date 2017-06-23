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


route : Url.Parser (SDMPage -> a) a
route =
    Url.oneOf
        [ Url.map NewSDM (Url.s "new")
        , Url.map SDMProjection (Url.s "projection" </> Url.int)
        ]


type alias Model =
    { mdl : Material.Model
    , page : SDMPage
    , sdmProjections : List AtomObjectRecord
    , sdmProjection : SDMProjection.Model
    , newSDM : NewSDM.Model
    }


init : Model
init =
    { mdl = Material.model
    , page = NewSDM
    , sdmProjections = []
    , sdmProjection = SDMProjection.init
    , newSDM = NewSDM.init
    }


type Msg
    = Mdl (Material.Msg Msg)
    | NewSDMMsg NewSDM.Msg
    | SDMProjectionMsg SDMProjection.Msg
    | GotSDMProjections (List AtomObjectRecord)
    | UrlChange Location
    | OpenExisting Int
    | OpenNew
    | Tick Time.Time


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
    in
        case msg of
            Mdl msg_ ->
                Material.update Mdl msg_ model

            NewSDMMsg msg_ ->
                liftedNewSDMUpdate msg_ model

            SDMProjectionMsg msg_ ->
                liftedSDMProjectionUpdate msg_ model

            UrlChange loc ->
                case Debug.log "path" (Url.parseHash route loc) of
                    Nothing ->
                        ( model, Cmd.none )

                    Just NewSDM ->
                        ( { model | page = NewSDM }, Cmd.none )

                    Just (SDMProjection id) ->
                        liftedSDMProjectionUpdate (SDMProjection.LoadMetadata id) { model | page = SDMProjection id }

            OpenExisting id ->
                model ! [ Nav.newUrl ("#projection/" ++ toString id) ]

            OpenNew ->
                model ! [ Nav.newUrl "#new" ]

            Tick _ ->
                ( model, getSDMProjections )

            GotSDMProjections projections ->
                ( { model | sdmProjections = projections }, Cmd.none )


getSDMProjections : Cmd Msg
getSDMProjections =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://notyeti-191.lifemapper.org/api/v2/sdmProject?status=300"
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
            Debug.log "Error fetching existing SDMs" (toString err) |> always (GotSDMProjections [])


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


start : Location -> ( Model, Cmd Msg )
start loc =
    let
        ( model, msg ) =
            update (UrlChange loc) init
    in
        model ! [ Material.init Mdl, NewSDM.initCmd NewSDMMsg, getSDMProjections, msg ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Material.subscriptions Mdl model, Time.every (5 * Time.second) Tick ]


main : Program Never Model Msg
main =
    Nav.program
        UrlChange
        { init = start
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
