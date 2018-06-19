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


port module NewSDM exposing (Model, page, init, update, Msg)

import List.Extra exposing (elemIndex, getAt)
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Events
import Json.Decode as Decode
import Http
import Material
import Material.Options as Options
import Material.List as Lists
import Material.Icon as Icon
import Material.Typography as Typo
import Material.Button as Button
import Material.Helpers exposing (lift)
import Material.Spinner as Loading
import Material.Textfield as Textfield
import Material.Progress as Progress
import ScenariosView as Scns
import AlgorithmsView as Algs
import OccurrenceSetsView as Occs
import Page exposing (Page)
import ScenariosList as SL
import Decoder
import Encoder
import ProgramFlags exposing (Flags)
import Navigation as Nav


port fileSelected : String -> Cmd msg


port uploadProgress : ({ id : String, loaded : Int, total : Int } -> msg) -> Sub msg


port uploadComplete : ({ id : String, response : String, status : Int } -> msg) -> Sub msg


port uploadFailed : ({ id : String, response : String } -> msg) -> Sub msg


port uploadCanceled : (String -> msg) -> Sub msg


port uploadCmd : { id : String, url : String } -> Cmd msg


port selectedFileName : ({ id : String, filename : String } -> msg) -> Sub msg


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


type UploadStatus
    = StartingUpload
    | Uploading ( Int, Int )
    | UploadComplete Int String
    | UploadFailed String
    | UploadCanceled


type FileSelectState
    = FileNotSelected
    | FileSelected
    | GotFileName { localFileName : String, uploadAs : String }
    | UploadingFile { localFileName : String, uploadAs : String, status : UploadStatus }
    | FileNameConflict { localFileName : String, uploadAs : String }


type alias Model =
    { mdl : Material.Model
    , selectedTab : Tab
    , scenarios : Scns.Model
    , algorithmsModel : Algs.Model
    , occurrenceSets : Occs.Model
    , availableScenarios : SL.Model
    , tree : FileSelectState
    , workFlowState : WorkFlowState
    , programFlags : Flags
    }


toApi : Model -> Result String Decoder.BoomPOST
toApi { algorithmsModel, occurrenceSets, scenarios } =
    Scns.toApi scenarios
        |> Result.map
            (\scenarioPackage ->
                Decoder.BoomPOST
                    { sdm = Just <| Algs.toApi algorithmsModel
                    , occurrence = Just <| Occs.toApi occurrenceSets
                    , scenario_package = Just scenarioPackage
                    , global_pam = Nothing
                    , mcpa = Nothing
                    , pam_stats = Nothing
                    , tree = Nothing
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
      , tree = FileNotSelected
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
    | FileSelectedMsg String
    | GotFileNameMsg { id : String, filename : String }
    | GotUploadStatus { id : String, status : UploadStatus }
    | UpdateUploadFilename String
    | DoUpload


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

        FileSelectedMsg id ->
            ( { model | tree = FileSelected }, fileSelected id )

        GotFileNameMsg { id, filename } ->
            if id == "tree-file" then
                ( { model | tree = GotFileName { localFileName = filename, uploadAs = filename } }, Cmd.none )
            else
                ( model, Cmd.none )

        UpdateUploadFilename uploadAs ->
            case model.tree of
                GotFileName info ->
                    ( { model | tree = GotFileName { info | uploadAs = uploadAs } }, Cmd.none )

                FileNameConflict info ->
                    ( { model | tree = FileNameConflict { info | uploadAs = uploadAs } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DoUpload ->
            case model.tree of
                GotFileName info ->
                    doUpload info model

                FileNameConflict info ->
                    doUpload info model

                _ ->
                    ( model, Cmd.none )

        GotUploadStatus { id, status } ->
            case ( id, model.tree ) of
                ( "tree-file", UploadingFile ({ localFileName, uploadAs } as info) ) ->
                    case status of
                        UploadComplete 409 _ ->
                            ( { model
                                | tree =
                                    FileNameConflict
                                        { localFileName = localFileName
                                        , uploadAs = uploadAs
                                        }
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | tree = UploadingFile { info | status = status } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


doUpload : { localFileName : String, uploadAs : String } -> Model -> ( Model, Cmd Msg )
doUpload { localFileName, uploadAs } model =
    let
        url =
            model.programFlags.apiRoot ++ "upload?uploadType=tree&fileName=" ++ uploadAs
    in
        ( { model
            | tree =
                UploadingFile
                    { localFileName = localFileName
                    , uploadAs = uploadAs
                    , status = StartingUpload
                    }
          }
        , uploadCmd { id = "tree-file", url = url }
        )


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
                "Hypothesis"

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
                        [ 0 ]
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
                    model.algorithmsModel |> Algs.view [] |> Html.map AlgsMsg

                OccurrenceSets ->
                    model.occurrenceSets |> Occs.view [] |> Html.map OccsMsg

                Scenarios ->
                    model.scenarios |> Scns.view [ 0 ] model.availableScenarios |> Html.map ScnsMsg

                TreeUpload ->
                    Options.div [ Options.css "padding" "20px" ]
                        (case model.tree of
                            FileNameConflict { localFileName, uploadAs } ->
                                [ Html.p [] [ Html.text ("File selected: " ++ localFileName) ]
                                , Html.p [] [ Html.text "Filename already in use. Choose another." ]
                                , Html.p []
                                    [ Textfield.render Mdl
                                        [ 0 ]
                                        model.mdl
                                        [ Textfield.label "Upload as"
                                        , Textfield.floatingLabel
                                        , Textfield.value uploadAs
                                        , Options.onInput UpdateUploadFilename
                                        ]
                                        []
                                    , Button.render Mdl
                                        [ 1 ]
                                        model.mdl
                                        [ Button.raised, Options.onClick DoUpload ]
                                        [ Html.text "Upload" ]
                                    ]
                                ]

                            UploadingFile { localFileName, uploadAs, status } ->
                                case status of
                                    StartingUpload ->
                                        [ Html.text <| "Starting upload of " ++ uploadAs ++ "..."
                                        , Progress.indeterminate
                                        ]

                                    Uploading ( loaded, total ) ->
                                        [ Html.text <| "Uploading " ++ uploadAs ++ "..."
                                        , Progress.progress (toFloat loaded / toFloat total * 100)
                                        ]

                                    UploadComplete status response ->
                                        [ Html.text <| "Finished uploading " ++ uploadAs ++ "." ]

                                    UploadFailed response ->
                                        [ Html.text <| "Failed uploading " ++ uploadAs ++ "." ]

                                    UploadCanceled ->
                                        [ Html.text <| "Failed uploading " ++ uploadAs ++ "." ]

                            GotFileName { localFileName, uploadAs } ->
                                [ Html.p [] [ Html.text ("File selected: " ++ localFileName) ]
                                , Html.p []
                                    [ Textfield.render Mdl
                                        [ 0 ]
                                        model.mdl
                                        [ Textfield.label "Upload as"
                                        , Textfield.floatingLabel
                                        , Textfield.value uploadAs
                                        , Options.onInput UpdateUploadFilename
                                        ]
                                        []
                                    , Button.render Mdl
                                        [ 1 ]
                                        model.mdl
                                        [ Button.raised, Options.onClick DoUpload ]
                                        [ Html.text "Upload" ]
                                    ]
                                ]

                            FileSelected ->
                                [ Html.text "Opening file..." ]

                            FileNotSelected ->
                                [ Html.input
                                    [ Attribute.type_ "file"
                                    , Attribute.id "tree-file"
                                    , Events.on "change" (Decode.succeed <| FileSelectedMsg "tree-file")
                                    ]
                                    []
                                ]
                        )

                HypothesisUpload ->
                    Options.div [ Options.css "padding" "20px" ]
                        [ Html.text "..." ]

                PostProjection ->
                    Options.div [ Options.css "padding" "20px" ]
                        [ Html.p []
                            [ Html.text """
                                 Once all of the inputs below have been defined the job
                                 can be submitted.
                                 """
                            ]
                        , Lists.ul [] <| List.map (taskLI model) tasks
                        , Button.render Mdl
                            [ 0 ]
                            model.mdl
                            [ Button.raised
                            , Button.disabled |> Options.when (not <| complete model)
                            , Options.onClick SubmitJob |> Options.when (complete model)
                            ]
                            [ Html.text "Submit Job" ]
                          -- , Button.render Mdl
                          --     [ 1 ]
                          --     model.mdl
                          --     [ Button.raised
                          --     , Options.onClick Restart
                          --     , Options.css "margin-left" "40px"
                          --     ]
                          --     [ Html.text "Start Over" ]
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
    List.all (\( _, problems ) -> problems model == Nothing) tasks


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
                , selectedFileName GotFileNameMsg
                , uploadProgress
                    (\{ id, total, loaded } ->
                        GotUploadStatus { id = id, status = Uploading ( loaded, total ) }
                    )
                , uploadComplete
                    (\{ id, response, status } ->
                        GotUploadStatus { id = id, status = UploadComplete status response }
                    )
                , uploadFailed
                    (\{ id, response } ->
                        GotUploadStatus { id = id, status = UploadFailed response }
                    )
                , uploadCanceled
                    (\id ->
                        GotUploadStatus { id = id, status = UploadCanceled }
                    )
                ]
    , title = "New Project"
    }
