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


module NewSDM exposing (Model, page, init, update, Msg)

import List.Extra exposing (elemIndex, getAt)
import Html exposing (Html)
import Http
import Material
import Material.Options as Options
import Material.List as Lists
import Material.Icon as Icon
import Material.Typography as Typo
import Material.Button as Button
import Material.Toggles as Toggles
import Material.Textfield as Textfield
import Material.Helpers exposing (lift)
import Material.Spinner as Loading
import ScenariosView as Scns
import AlgorithmsView as Algs
import OccurrenceSetsView as Occs
import Page exposing (Page)
import ScenariosList as SL
import Decoder
import Encoder
import ProgramFlags exposing (Flags)
import Navigation as Nav
import UploadFile


type Tab
    = Algorithms
    | OccurrenceSets
    | Scenarios
    | TreeUpload
    | HypothesisUpload
    | PostProjection


tabs : List Tab
tabs =
    [ OccurrenceSets, Algorithms, Scenarios, TreeUpload, HypothesisUpload, PostProjection ]


tabIndex : Tab -> Int
tabIndex tab =
    tabs |> elemIndex tab |> Maybe.withDefault 0


type WorkFlowState
    = Defining
    | Submitting
    | SubmissionFailed


type alias Model =
    { mdl : Material.Model
    , selectedTab : Tab
    , scenarios : Scns.Model
    , algorithmsModel : Algs.Model
    , occurrenceSets : Occs.Model
    , availableScenarios : SL.Model
    , treeUpload : UploadFile.Model
    , hypoUpload : UploadFile.Model
    , computeStats : Bool
    , archiveName : String
    , workFlowState : WorkFlowState
    , programFlags : Flags
    }


toApi : Model -> Result String Decoder.BoomPOST
toApi { algorithmsModel, occurrenceSets, scenarios, treeUpload, hypoUpload, computeStats, archiveName } =
    Scns.toApi scenarios
        |> Result.map
            (\scenarioPackage ->
                Decoder.BoomPOST
                    { sdm = Just <| Algs.toApi algorithmsModel
                    , occurrence = Just <| Occs.toApi occurrenceSets
                    , scenario_package = Just scenarioPackage
                    , global_pam =
                        Nothing
                    , mcpa =
                        UploadFile.getUploadedFilename hypoUpload
                            |> Maybe.map (\filename -> Decoder.BoomMCPA { hypotheses_package_name = filename })
                    , pam_stats =
                        if computeStats then
                            Just (Decoder.BoomPAMStats { compute_pam_stats = 1 })
                        else
                            Just (Decoder.BoomPAMStats { compute_pam_stats = 0 })
                    , tree =
                        UploadFile.getUploadedFilename treeUpload
                            |> Maybe.map (Just >> Decoder.BoomPOSTTreeRecord >> Decoder.BoomPOSTTree)
                    , archive_name = Just archiveName
                    }
            )


submitJob : Model -> Cmd Msg
submitJob model =
    case toApi model of
        Ok postData ->
            Http.request
                { method = "POST"
                , headers = [ Http.header "Accept" "application/json", Http.header "Content-Type" "text/plain" ]
                , url = model.programFlags.apiRoot ++ "sdmProject"
                , body = Http.jsonBody <| Encoder.encodeBoomPOST <| postData
                , expect = Http.expectJson Decoder.decodeAtomObject
                , timeout = Nothing
                , withCredentials = False
                }
                |> Debug.log "request"
                |> Http.send JobSubmitted

        Err msg ->
            Debug.log "Can't post SDM." msg |> always Cmd.none


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { mdl = Material.model
      , selectedTab = OccurrenceSets
      , scenarios = Scns.init
      , algorithmsModel = Algs.init
      , occurrenceSets = Occs.init flags
      , availableScenarios = SL.init flags
      , treeUpload = UploadFile.init
      , hypoUpload = UploadFile.init
      , computeStats = False
      , archiveName = ""
      , workFlowState = Defining
      , programFlags = flags
      }
    , SL.getPackages flags SLMsg
    )


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Tab
    | SubmitJob
    | JobSubmitted (Result Http.Error Decoder.AtomObject)
    | Restart
    | ScnsMsg Scns.Msg
    | AlgsMsg Algs.Msg
    | OccsMsg Occs.Msg
    | SLMsg SL.Msg
    | ToggleComputeStats
    | UpdateArchiveName String
    | UploadMsg UploadFile.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            ( { model | selectedTab = tab }, Cmd.none )

        SubmitJob ->
            ( { model | workFlowState = Submitting }, submitJob model )

        JobSubmitted (Ok (Decoder.AtomObject results)) ->
            model ! [ Nav.newUrl ("#results/" ++ toString results.id) ]

        JobSubmitted (Err err) ->
            Debug.log "submission failed" (toString err)
                |> always ( { model | workFlowState = SubmissionFailed }, Cmd.none )

        Restart ->
            init model.programFlags

        ScnsMsg msg_ ->
            lift
                .scenarios
                (\m x -> { m | scenarios = x })
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

        OccsMsg msg_ ->
            lift
                .occurrenceSets
                (\m x -> { m | occurrenceSets = x })
                OccsMsg
                (Occs.update [ 1 ])
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

        UploadMsg msg_ ->
            let
                ( treeUpload_, cmd1 ) =
                    UploadFile.update "tree" [ 3 ] model.programFlags msg_ model.treeUpload

                ( hypoUpload_, cmd2 ) =
                    UploadFile.update "biogeo" [ 4 ] model.programFlags msg_ model.hypoUpload
            in
                { model | treeUpload = treeUpload_, hypoUpload = hypoUpload_ } ! [ cmd1, cmd2 ]

        ToggleComputeStats ->
            ( { model | computeStats = not model.computeStats }, Cmd.none )

        UpdateArchiveName name ->
            ( { model | archiveName = name }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


tabTitle : Tab -> Html msg
tabTitle tab =
    Html.text <|
        case tab of
            Algorithms ->
                "SDM Algorithms"

            OccurrenceSets ->
                "Species Data"

            Scenarios ->
                "SDM Layers"

            TreeUpload ->
                "Tree Upload"

            HypothesisUpload ->
                "Hypotheses"

            PostProjection ->
                "Submit Project"


mainView : Model -> Html Msg
mainView model =
    case model.workFlowState of
        SubmissionFailed ->
            Options.div [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                [ Html.text "There was a problem submitting the job."
                , Html.p []
                    [ Button.render Mdl
                        [ 5 ]
                        model.mdl
                        [ Button.raised, Options.onClick SubmitJob ]
                        [ Html.text "Retry" ]
                    ]
                ]

        Submitting ->
            Options.div [ Options.css "text-align" "center", Options.css "padding-top" "50px", Typo.headline ]
                [ Html.text "Submitting job..."
                , Html.p [] [ Loading.spinner [ Loading.active True ] ]
                ]

        Defining ->
            case model.selectedTab of
                Algorithms ->
                    model.algorithmsModel |> Algs.view [ 0 ] |> Html.map AlgsMsg

                OccurrenceSets ->
                    model.occurrenceSets |> Occs.view [ 1 ] |> Html.map OccsMsg

                Scenarios ->
                    model.scenarios |> Scns.view [ 2 ] model.availableScenarios |> Html.map ScnsMsg

                TreeUpload ->
                    Options.div [ Options.css "padding" "20px" ]
                        (UploadFile.view Mdl UploadMsg [ 3 ] model.mdl model.treeUpload)

                HypothesisUpload ->
                    Options.div [ Options.css "padding" "20px" ]
                        (UploadFile.view Mdl UploadMsg [ 4 ] model.mdl model.hypoUpload)

                PostProjection ->
                    Options.div [ Options.css "padding" "20px" ]
                        [ Html.p []
                            [ Html.text """
                                 Once all of the inputs below have been defined the job
                                 can be submitted.
                                 """
                            ]
                        , Lists.ul []
                            (List.map (taskLI model) tasks
                                ++ [ uploadTaskLI "Tree" model.treeUpload ]
                                ++ [ uploadTaskLI "Biogeographic Hypotheses" model.hypoUpload ]
                            )
                        , Toggles.switch Mdl
                            [ 6 ]
                            model.mdl
                            [ Toggles.value model.computeStats
                            , Options.onToggle ToggleComputeStats
                            , Options.css "margin-bottom" "20px"
                            ]
                            [ Html.text "Compute PAM stats" ]
                        , Textfield.render Mdl
                            [ 7 ]
                            model.mdl
                            [ Textfield.label "Job name"
                            , Textfield.floatingLabel
                            , Textfield.value model.archiveName
                            , Options.onInput UpdateArchiveName
                            ]
                            []
                        , Html.p []
                            [ Button.render Mdl
                                [ 5 ]
                                model.mdl
                                [ Button.raised
                                , Button.disabled |> Options.when (not <| complete model)
                                , Options.onClick SubmitJob |> Options.when (complete model)
                                ]
                                [ Html.text "Submit Job" ]
                            ]
                        ]


uploadTaskLI : String -> UploadFile.Model -> Html Msg
uploadTaskLI title upload =
    case UploadFile.getUploadedFilename upload of
        Just filename ->
            Lists.li []
                [ Lists.content []
                    [ Icon.i "check_box"
                    , Html.text title
                    , Options.span [ Options.css "margin-left" "5px", Typo.caption ]
                        [ Html.text <| "(" ++ filename ++ ")" ]
                    ]
                ]

        Nothing ->
            Lists.li []
                [ Lists.content []
                    [ Icon.i "check_box_outline_blank"
                    , Html.text title
                    , Options.span [ Options.css "margin-left" "5px", Typo.caption ]
                        [ Html.text <| "(optional)" ]
                    ]
                ]


taskLI : Model -> ( Tab, Model -> Maybe String ) -> Html Msg
taskLI model ( tab, problemFunc ) =
    let
        problems =
            problemFunc model

        ( icon, hint ) =
            case problems of
                Nothing ->
                    ( Icon.i "check_box"
                    , []
                    )

                Just problem ->
                    ( Icon.i "check_box_outline_blank"
                    , [ Options.span [ Options.css "margin-left" "5px", Typo.caption ]
                            [ Html.text <| "(" ++ problem ++ ")" ]
                      ]
                    )
    in
        Lists.li [] [ Lists.content [] <| [ icon, tabTitle tab ] ++ hint ]


tasks : List ( Tab, Model -> Maybe String )
tasks =
    [ ( OccurrenceSets, (.occurrenceSets >> Occs.problems) )
    , ( Algorithms, (.algorithmsModel >> Algs.problems) )
    , ( Scenarios, (.scenarios >> Scns.problems) )
    ]


complete : Model -> Bool
complete model =
    (model.archiveName /= "") && (List.all (\( _, problems ) -> problems model == Nothing) tasks)


selectedTab : Model -> Int
selectedTab model =
    tabIndex model.selectedTab


selectTab : Int -> Msg
selectTab i =
    tabs |> getAt i |> Maybe.withDefault Algorithms |> SelectTab


tabTitles : Model -> List (Html msg)
tabTitles model =
    List.map tabTitle tabs


page : Page Model Msg
page =
    { view = mainView
    , selectedTab = selectedTab
    , selectTab = selectTab
    , tabTitles = tabTitles
    , subscriptions =
        always <|
            Sub.batch
                [ Scns.subscriptions ScnsMsg
                , UploadFile.subscriptions UploadMsg
                , Sub.map OccsMsg Occs.subscriptions
                ]
    , title = "New Project"
    }
