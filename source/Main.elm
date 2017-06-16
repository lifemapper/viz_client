module Main exposing (..)

import Material
import Material.Layout as Layout
import Material.Scheme
import Material.Helpers exposing (lift)
import Html exposing (Html)
import Page
import NewSDM


type SDMPage
    = NewSDM



-- | CompletedSDM Int


type alias Model =
    { mdl : Material.Model
    , page : SDMPage
    , newSDM : NewSDM.Model
    }


init : Model
init =
    { mdl = Material.model
    , page = NewSDM
    , newSDM = NewSDM.init
    }


type Msg
    = Mdl (Material.Msg Msg)
    | NewSDMMsg NewSDM.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        NewSDMMsg msg_ ->
            lift
                .newSDM
                (\m x -> { m | newSDM = x })
                NewSDMMsg
                NewSDM.update
                msg_
                model


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title [] [ Html.text "Lifemapper SDM" ] ]
    ]


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                NewSDM ->
                    Page.lift NewSDM.page .newSDM NewSDMMsg
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
            , drawer =
                [ Layout.title [] [ Html.text "Lifemapper SDM" ]
                ]
            , tabs = ( page.tabTitles model, [] )
            , main = [ page.view model ]
            }


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( init
            , Cmd.batch
                [ Material.init Mdl
                , NewSDM.initCmd NewSDMMsg
                ]
            )
        , view = view >> Material.Scheme.top
        , subscriptions = Material.subscriptions Mdl
        , update = update
        }
