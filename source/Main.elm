module Main exposing (..)

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
import ExistingSDM
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
    | ExistingSDM Int


route : Url.Parser (SDMPage -> a) a
route =
    Url.oneOf
        [ Url.map NewSDM (Url.s "sdm-new")
        , Url.map ExistingSDM (Url.s "sdm-projection" </> Url.int)
        ]


type alias Model =
    { mdl : Material.Model
    , page : SDMPage
    , existingSDMs : List AtomObjectRecord
    , existingSDM : ExistingSDM.Model
    , newSDM : NewSDM.Model
    }


init : Model
init =
    { mdl = Material.model
    , page = NewSDM
    , existingSDMs = []
    , existingSDM = ExistingSDM.init
    , newSDM = NewSDM.init
    }


type Msg
    = Mdl (Material.Msg Msg)
    | NewSDMMsg NewSDM.Msg
    | ExistingSDMMsg ExistingSDM.Msg
    | GotExistingSDMs (Result Http.Error AtomList)
    | UrlChange Location
    | OpenExisting Int
    | OpenNew


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        liftedNewSDMUpdate =
            lift
                .newSDM
                (\m x -> { m | newSDM = x })
                NewSDMMsg
                NewSDM.update

        liftedExistingSDMUpdate =
            lift
                .existingSDM
                (\m x -> { m | existingSDM = x })
                ExistingSDMMsg
                ExistingSDM.update
    in
        case msg of
            Mdl msg_ ->
                Material.update Mdl msg_ model

            NewSDMMsg msg_ ->
                liftedNewSDMUpdate msg_ model

            ExistingSDMMsg msg_ ->
                liftedExistingSDMUpdate msg_ model

            UrlChange loc ->
                case Debug.log "path" (Url.parsePath route loc) of
                    Nothing ->
                        ( model, Cmd.none )

                    Just NewSDM ->
                        ( { model | page = NewSDM }, Cmd.none )

                    Just (ExistingSDM id) ->
                        liftedExistingSDMUpdate (ExistingSDM.LoadMetadata id) { model | page = ExistingSDM id }

            OpenExisting id ->
                model ! [ Nav.newUrl ("/sdm-projection/" ++ toString id) ]

            OpenNew ->
                model ! [ Nav.newUrl "/sdm-new" ]

            GotExistingSDMs (Ok (AtomList atoms)) ->
                ( { model | existingSDMs = atoms |> List.map (\(AtomObject o) -> o) }, Cmd.none )

            GotExistingSDMs (Err err) ->
                Debug.log "Error fetching existing SDMs" (toString err) |> always ( model, Cmd.none )


getExistingSDMs : Cmd Msg
getExistingSDMs =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://notyeti-191.lifemapper.org/api/v2/sdmProject?status=300"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send GotExistingSDMs


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
    , model.existingSDMs
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

                ExistingSDM id ->
                    Page.lift ExistingSDM.page .existingSDM ExistingSDMMsg
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
        model ! [ Material.init Mdl, NewSDM.initCmd NewSDMMsg, getExistingSDMs, msg ]


main : Program Never Model Msg
main =
    Nav.program
        UrlChange
        { init = start
        , view = view
        , subscriptions = Material.subscriptions Mdl
        , update = update
        }
