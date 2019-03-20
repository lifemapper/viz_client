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


module Main exposing (..)

import Time
import Material
import Material.Layout as Layout
import Material.Typography as Typo
import Material.Options as Options
import Material.Helpers exposing (lift)
import Material.Spinner as Loading
import Material.Color as Color
import Html exposing (Html)
import Html.Attributes
import Navigation as Nav exposing (Location)
import UrlParser as Url exposing ((</>))
import Http
import Page
import NewBoom
import BoomResults
import BrowseProjectionsPage
import SignUp
import ProgramFlags exposing (Flags)
import Authentication as Auth
import Decoder
    exposing
        ( AtomObjectRecord
        , AtomList(..)
        , AtomObject(..)
        , decodeAtomList
        )


type AppPage
    = NewBoom NewBoom.Model
    | BoomResults Int BoomResults.Model
    | SignUp SignUp.Model
    | BrowseProjections BrowseProjectionsPage.Model
    | PageNotFound


initResultsPage : Flags -> Int -> ( AppPage, Cmd Msg )
initResultsPage flags gridsetId =
    let
        ( model_, msg_ ) =
            BoomResults.init flags gridsetId
    in
        ( BoomResults gridsetId model_, Cmd.map BoomResultsMsg msg_ )


initNewBoomPage : Flags -> ( AppPage, Cmd Msg )
initNewBoomPage flags =
    let
        ( model_, msg_ ) =
            NewBoom.init flags
    in
        ( NewBoom model_, Cmd.map NewBoomMsg msg_ )


initSignUpPage : Flags -> ( AppPage, Cmd Msg )
initSignUpPage flags =
    let
        ( model_, msg_ ) =
            SignUp.init flags
    in
        ( SignUp model_, Cmd.map SignUpMsg msg_ )


initBrowsePage : Flags -> ( AppPage, Cmd Msg )
initBrowsePage flags =
    let
        ( model_, msg_ ) =
            BrowseProjectionsPage.init flags
    in
        ( BrowseProjections model_, Cmd.map BrowseProjectionsMsg msg_ )


location2Page : Flags -> Location -> Maybe ( AppPage, Cmd Msg )
location2Page flags location =
    let
        route =
            Url.oneOf
                [ Url.map (initNewBoomPage flags) Url.top
                , Url.map (initResultsPage flags) (Url.s "results" </> Url.int)
                , Url.map (initBrowsePage flags) (Url.s "browse-projections")
                , Url.map (initSignUpPage flags) (Url.s "sign-up")
                ]
    in
        Url.parseHash route location


type GridSets
    = GridSetsLoading
    | GridSetsList (List AtomObjectRecord)


type alias Model =
    { mdl : Material.Model
    , page : AppPage
    , gridsets : GridSets
    , flags : Flags
    , login : Auth.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | NewBoomMsg NewBoom.Msg
    | BoomResultsMsg BoomResults.Msg
    | BrowseProjectionsMsg BrowseProjectionsPage.Msg
    | SignUpMsg SignUp.Msg
    | GotGridSets (List AtomObjectRecord)
    | UrlChange Location
    | OpenExisting Int
    | OpenNew
    | OpenBrowse
    | Tick Time.Time
    | AuthMsg Auth.Msg
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        liftedNewBoomUpdate =
            case model.page of
                NewBoom model_ ->
                    lift
                        (always model_)
                        (\m x -> { m | page = NewBoom x })
                        NewBoomMsg
                        NewBoom.update

                _ ->
                    \msg_ model -> ( model, Cmd.none )

        liftedBoomResultsUpdate =
            case model.page of
                BoomResults id results ->
                    lift
                        (always results)
                        (\m x -> { m | page = BoomResults id x })
                        BoomResultsMsg
                        BoomResults.update

                _ ->
                    \msg_ model -> ( model, Cmd.none )

        liftedBrowseProjectionsUpdate =
            case model.page of
                BrowseProjections model_ ->
                    lift
                        (always model_)
                        (\m x -> { m | page = BrowseProjections x })
                        BrowseProjectionsMsg
                        BrowseProjectionsPage.update

                _ ->
                    \msg_ model -> ( model, Cmd.none )

        liftedSignUpUpdate =
            case model.page of
                SignUp model_ ->
                    lift
                        (always model_)
                        (\m x -> { m | page = SignUp x })
                        SignUpMsg
                        SignUp.update

                _ ->
                    \msg_ model -> ( model, Cmd.none )
    in
        case msg of
            Mdl msg_ ->
                Material.update Mdl msg_ model

            NewBoomMsg msg_ ->
                liftedNewBoomUpdate msg_ model

            BoomResultsMsg msg_ ->
                liftedBoomResultsUpdate msg_ model

            BrowseProjectionsMsg msg_ ->
                liftedBrowseProjectionsUpdate msg_ model

            SignUpMsg msg_ ->
                liftedSignUpUpdate msg_ model

            UrlChange location ->
                location2Page model.flags location
                    |> Maybe.withDefault ( PageNotFound, Cmd.none )
                    |> \( page, msg ) -> ( { model | page = page }, msg )

            OpenExisting id ->
                model ! [ Nav.newUrl ("#results/" ++ toString id) ]

            OpenNew ->
                model ! [ Nav.newUrl "#" ]

            OpenBrowse ->
                model ! [ Nav.newUrl "#browse-projections/" ]

            Tick _ ->
                ( model, getGridSets model.flags model.login )

            GotGridSets gridsets ->
                ( { model | gridsets = GridSetsList gridsets }, Cmd.none )

            Nop ->
                ( model, Cmd.none )

            AuthMsg msg_ ->
                let
                    ( login, cmd_ ) =
                        Auth.update model.flags msg_ model.login
                in
                    { model | login = login } ! [ Cmd.map AuthMsg cmd_ ]


getGridSets : Flags -> Auth.Model -> Cmd Msg
getGridSets { apiRoot } login =
    let
        url =
            case Auth.getUserName login of
                Nothing ->
                    apiRoot ++ "gridset?user=anon"

                _ ->
                    apiRoot ++ "gridset"
    in
        Http.request
            { method = "GET"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = url
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
            [ Html.text "BiotaPhy | "
            , Options.span [ Typo.subhead ] [ Html.text title ]
            ]
        ]
    ]


newLink : Model -> Html Msg
newLink model =
    let
        selected =
            case model.page of
                NewBoom _ ->
                    Color.background (Color.color Color.Grey Color.S300)

                _ ->
                    Options.nop
    in
        Layout.link [ Options.onClick OpenNew, Options.css "cursor" "pointer", selected ]
            [ Html.text "New Project" ]


resultsLink : Model -> AtomObjectRecord -> Html Msg
resultsLink model { name, id } =
    let
        selected =
            case model.page of
                BoomResults gridsetId _ ->
                    if gridsetId == id then
                        Color.background (Color.color Color.Grey Color.S300)
                    else
                        Options.nop

                _ ->
                    Options.nop
    in
        Layout.link [ Options.onClick (OpenExisting id), Options.css "cursor" "pointer", selected ]
            [ Html.text name ]


title : Auth.Model -> Html msg
title login =
    Auth.getUserName login
        |> Maybe.map (\userName -> "Welcome, " ++ userName)
        |> Maybe.withDefault "BiotaPhy Platform"
        |> Html.text


drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.title [] [ title model.login ]
    , Layout.navigation [] (Auth.view "#sign-up/" model.login |> List.map (Html.map AuthMsg))
    , Layout.navigation []
        [ Layout.link []
            [ Html.a [ Html.Attributes.href "./subsetpam.html" ]
                [ Html.text "Subset the Global PAM" ]
            ]
        ]
    , Layout.navigation [] [ newLink model ]
      -- , Layout.navigation [] [ newOccurrenceSetLink model ]
    , Layout.title [ Typo.subhead ] [ Html.text "Completed" ]
    , Layout.navigation []
        [ Layout.link [ Options.onClick OpenBrowse, Options.css "cursor" "pointer" ] [ Html.text "Search Species" ]]
    , case model.gridsets of
        GridSetsLoading ->
            Layout.row [] [ Loading.spinner [ Loading.active True ] ]

        GridSetsList list ->
            list |> List.map (resultsLink model) |> Layout.navigation []
    ]


pageImplementation : AppPage -> Page.Page Model Msg
pageImplementation p =
    case p of
        NewBoom model_ ->
            Page.lift NewBoom.page (always model_) NewBoomMsg

        BoomResults _ model_ ->
            Page.lift BoomResults.page (always model_) BoomResultsMsg

        SignUp model_ ->
            Page.lift SignUp.page (always model_) SignUpMsg

        BrowseProjections model_ ->
            Page.lift BrowseProjectionsPage.page (always model_) BrowseProjectionsMsg

        PageNotFound ->
            { view = always <| Options.div [] []
            , selectedTab = always 0
            , selectTab = always Nop
            , tabTitles = always []
            , subscriptions = always Sub.none
            , title = "Page Not Found"
            }


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
start flags location =
    let
        ( login, authInitCmd ) =
            Auth.init flags

        ( page, cmd ) =
            location2Page flags location
                |> Maybe.withDefault ( PageNotFound, Cmd.none )

        model =
            { mdl = Material.model
            , page = page
            , gridsets = GridSetsLoading
            , flags = flags
            , login = login
            }

        cmds =
            [ Material.init Mdl
            , getGridSets flags login
            , Cmd.map AuthMsg authInitCmd
            , cmd
            ]
    in
        model ! cmds


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdl model
        , (pageImplementation model.page).subscriptions model
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
