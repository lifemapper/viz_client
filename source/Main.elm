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
import Navigation as Nav exposing (Location)
import UrlParser as Url exposing ((</>))
import Http
import Page
import NewSDM
import SDMResults
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


type SDMPage
    = NewSDM NewSDM.Model
    | SDMResults Int SDMResults.Model
    | SignUp SignUp.Model
    | BrowseProjections BrowseProjectionsPage.Model
    | PageNotFound


initResultsPage : Flags -> Int -> ( SDMPage, Cmd Msg )
initResultsPage flags gridsetId =
    ( SDMResults.init flags gridsetId |> SDMResults gridsetId, Cmd.none )


initNewSDMPage : Flags -> ( SDMPage, Cmd Msg )
initNewSDMPage flags =
    let
        ( model_, msg_ ) =
            NewSDM.init flags
    in
        ( NewSDM model_, Cmd.map NewSDMMsg msg_ )


initSignUpPage : Flags -> ( SDMPage, Cmd Msg )
initSignUpPage flags =
    let
        ( model_, msg_ ) =
            SignUp.init flags
    in
        ( SignUp model_, Cmd.map SignUpMsg msg_ )


initBrowsePage : Flags -> ( SDMPage, Cmd Msg )
initBrowsePage flags =
    let
        ( model_, msg_ ) =
            BrowseProjectionsPage.init flags
    in
        ( BrowseProjections model_, Cmd.map BrowseProjectionsMsg msg_ )


location2Page : Flags -> Location -> Maybe ( SDMPage, Cmd Msg )
location2Page flags location =
    let
        route =
            Url.oneOf
                [ Url.map (initNewSDMPage flags) Url.top
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
    , page : SDMPage
    , gridsets : GridSets
    , flags : Flags
    , login : Auth.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | NewSDMMsg NewSDM.Msg
    | SDMResultsMsg SDMResults.Msg
    | BrowseProjectionsMsg BrowseProjectionsPage.Msg
    | SignUpMsg SignUp.Msg
    | GotGridSets (List AtomObjectRecord)
    | UrlChange Location
    | OpenExisting Int
    | OpenNew
    | OpenNewOccurrenceSet
    | OpenBrowse
    | Tick Time.Time
    | AuthMsg Auth.Msg
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        liftedNewSDMUpdate =
            case model.page of
                NewSDM model_ ->
                    lift
                        (always model_)
                        (\m x -> { m | page = NewSDM x })
                        NewSDMMsg
                        NewSDM.update

                _ ->
                    \msg_ model -> ( model, Cmd.none )

        liftedSDMResultsUpdate =
            case model.page of
                SDMResults id results ->
                    lift
                        (always results)
                        (\m x -> { m | page = SDMResults id x })
                        SDMResultsMsg
                        SDMResults.update

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

            NewSDMMsg msg_ ->
                liftedNewSDMUpdate msg_ model

            SDMResultsMsg msg_ ->
                liftedSDMResultsUpdate msg_ model

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

            OpenNewOccurrenceSet ->
                model ! [ Nav.newUrl "#new-species-data/" ]

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
            [ Html.text "Lifemapper SDM | "
            , Options.span [ Typo.subhead ] [ Html.text title ]
            ]
        ]
    ]


newLink : Model -> Html Msg
newLink model =
    let
        selected =
            case model.page of
                NewSDM _ ->
                    Color.background (Color.color Color.Grey Color.S300)

                _ ->
                    Options.nop
    in
        Layout.link [ Options.onClick OpenNew, Options.css "cursor" "pointer", selected ]
            [ Html.text "New SDM Project" ]


resultsLink : Model -> AtomObjectRecord -> Html Msg
resultsLink model { modificationTime, id } =
    let
        selected =
            case model.page of
                SDMResults gridsetId _ ->
                    if gridsetId == id then
                        Color.background (Color.color Color.Grey Color.S300)
                    else
                        Options.nop

                _ ->
                    Options.nop
    in
        Layout.link [ Options.onClick (OpenExisting id), Options.css "cursor" "pointer", selected ]
            [ Html.text modificationTime ]


browseProjectionsLink : Model -> Html Msg
browseProjectionsLink model =
    Layout.link [ Options.onClick OpenBrowse, Options.css "cursor" "pointer" ] [ Html.text "Browse by species" ]


title : Auth.Model -> Html msg
title login =
    Auth.getUserName login
        |> Maybe.map (\userName -> "Welcome, " ++ userName)
        |> Maybe.withDefault "Lifemapper SDM"
        |> Html.text


drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.title [] [ title model.login ]
    , Layout.navigation [] (Auth.view "#sign-up/" model.login |> List.map (Html.map AuthMsg))
    , Layout.navigation [] [ newLink model ]
      -- , Layout.navigation [] [ newOccurrenceSetLink model ]
    , Layout.title [ Typo.subhead ] [ Html.text "Completed" ]
    , Layout.navigation [] [ browseProjectionsLink model ]
    , case model.gridsets of
        GridSetsLoading ->
            Layout.row [] [ Loading.spinner [ Loading.active True ] ]

        GridSetsList list ->
            list |> List.map (resultsLink model) |> Layout.navigation []
    ]


pageImplementation : SDMPage -> Page.Page Model Msg
pageImplementation p =
    case p of
        NewSDM model_ ->
            Page.lift NewSDM.page (always model_) NewSDMMsg

        SDMResults _ model_ ->
            Page.lift SDMResults.page (always model_) SDMResultsMsg

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
