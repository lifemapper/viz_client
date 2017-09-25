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
import NewOccurrenceSet
import ProgramFlags exposing (Flags)
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
    | NewOccurrenceSet NewOccurrenceSet.Model
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


initNewOccurrenceSetPage : Flags -> ( SDMPage, Cmd Msg )
initNewOccurrenceSetPage flags =
    let
        ( model_, msg_ ) =
            NewOccurrenceSet.init
    in
        ( NewOccurrenceSet model_, Cmd.none )


location2Page : Flags -> Location -> Maybe ( SDMPage, Cmd Msg )
location2Page flags location =
    let
        route =
            Url.oneOf
                [ Url.map (initNewSDMPage flags) Url.top
                , Url.map (initResultsPage flags) (Url.s "results" </> Url.int)
                , Url.map (initNewOccurrenceSetPage flags) (Url.s "new-species-data")
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
    }


type Msg
    = Mdl (Material.Msg Msg)
    | NewSDMMsg NewSDM.Msg
    | SDMResultsMsg SDMResults.Msg
    | NewOccurrenceSetMsg NewOccurrenceSet.Msg
    | GotGridSets (List AtomObjectRecord)
    | UrlChange Location
    | OpenExisting Int
    | OpenNew
    | OpenNewOccurrenceSet
    | Tick Time.Time
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

        liftedNewOccurrenceSetUpdate =
            case model.page of
                NewOccurrenceSet model_ ->
                    lift
                        (always model_)
                        (\m x -> { m | page = NewOccurrenceSet x })
                        NewOccurrenceSetMsg
                        NewOccurrenceSet.update

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

            NewOccurrenceSetMsg msg_ ->
                liftedNewOccurrenceSetUpdate msg_ model

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

            Tick _ ->
                ( model, getGridSets model.flags )

            GotGridSets gridsets ->
                ( { model | gridsets = GridSetsList gridsets }, Cmd.none )

            Nop ->
                ( model, Cmd.none )


getGridSets : Flags -> Cmd Msg
getGridSets { apiRoot } =
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


newOccurrenceSetLink : Model -> Html Msg
newOccurrenceSetLink model =
    let
        selected =
            case model.page of
                NewOccurrenceSet _ ->
                    Color.background (Color.color Color.Grey Color.S300)

                _ ->
                    Options.nop
    in
        Layout.link [ Options.onClick OpenNewOccurrenceSet, Options.css "cursor" "pointer", selected ]
            [ Html.text "New Species Data" ]


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


drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.title [] [ Html.text "Lifemapper SDM" ]
    , Layout.navigation [] [ newLink model ]
    , Layout.navigation [] [ newOccurrenceSetLink model ]
    , Layout.title [ Typo.subhead ] [ Html.text "Completed" ]
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

        NewOccurrenceSet model_ ->
            Page.lift NewOccurrenceSet.page (always model_) NewOccurrenceSetMsg

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
    location2Page flags location
        |> Maybe.withDefault ( PageNotFound, Cmd.none )
        |> \( page, msg ) ->
            { mdl = Material.model
            , page = page
            , gridsets = GridSetsLoading
            , flags = flags
            }
                ! [ Material.init Mdl
                  , getGridSets flags
                  , msg
                  ]


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
