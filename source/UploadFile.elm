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


port module UploadFile exposing (Model, Msg, init, getUploadedFilename, update, view, subscriptions)

import List.Extra as List
import Ternary exposing ((?))
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Events
import ProgramFlags exposing (Flags)
import Material
import Material.Toggles as Toggles
import Material.Textfield as Textfield
import Material.Progress as Progress
import Material.Options as Options
import Material.Button as Button
import Json.Decode as Decode
import Helpers exposing (Index)


port fileSelected : String -> Cmd msg


port uploadProgress : ({ id : String, loaded : Int, total : Int } -> msg) -> Sub msg


port uploadComplete : ({ id : String, response : String, status : Int } -> msg) -> Sub msg


port uploadFailed : ({ id : String, response : String } -> msg) -> Sub msg


port uploadCanceled : (String -> msg) -> Sub msg


port uploadCmd : { id : String, url : String } -> Cmd msg


port selectedFileName : ({ id : String, filename : String, preview : List (List String) } -> msg) -> Sub msg


type UploadStatus
    = StartingUpload
    | Uploading ( Int, Int )
    | UploadComplete Int String
    | UploadFailed String
    | UploadCanceled


type FileSelectState
    = FileNotSelected
    | FileSelected
    | GotFileName { localFileName : String, uploadAs : String, metadata : Metadata }
    | UploadingFile { localFileName : String, uploadAs : String, status : UploadStatus }
    | FileNameConflict { localFileName : String, uploadAs : String }
    | Finished String


type alias Metadata =
    { fields : List { shortName : String, fieldType : String }
    , roles :
        { groupBy : Maybe Int
        , geopoint : Maybe Int
        , latitude : Maybe Int
        , longitude : Maybe Int
        , taxaName : Maybe Int
        , uniqueId : Maybe Int
        }
    , preview : List (List String)
    }


initMetadata : List (List String) -> Metadata
initMetadata preview =
    { fields =
        preview
            |> List.getAt 0
            |> Maybe.withDefault []
            |> List.map (always { shortName = "", fieldType = "string" })
    , roles =
        { groupBy = Nothing
        , geopoint = Nothing
        , latitude = Nothing
        , longitude = Nothing
        , taxaName = Nothing
        , uniqueId = Nothing
        }
    , preview = preview
    }


type alias Model =
    FileSelectState


init : Model
init =
    FileNotSelected


getUploadedFilename : Model -> Maybe String
getUploadedFilename state =
    case state of
        Finished filename ->
            Just filename

        _ ->
            Nothing


type UploadMsg
    = FileSelectedMsg String
    | GotFileNameMsg { id : String, filename : String, preview : List (List String) }
    | MetadataMsg MetadataMsg
    | GotUploadStatus { id : String, status : UploadStatus }
    | UpdateUploadFilename String
    | DoUpload


type MetadataMsg
    = UpdateFieldName Int String
    | ToggleGroupBy Int
    | ToggleGeopoint Int
    | ToggleLatitude Int
    | ToggleLongitude Int
    | ToggleTaxaName Int
    | ToggleUniqueId Int


type alias Msg =
    UploadMsg


fileSelectId : Index -> String
fileSelectId index =
    "file-select" ++ (index |> List.map toString |> String.join "-")


updateMetadata : MetadataMsg -> Metadata -> Metadata
updateMetadata msg metadata =
    case Debug.log "updateMetadata" msg of
        UpdateFieldName i name ->
            let
                fields =
                    metadata.fields
                        |> List.indexedMap
                            (\j field ->
                                if j == i then
                                    { field | shortName = name }
                                else
                                    field
                            )
            in
                { metadata | fields = fields }

        ToggleGroupBy i ->
            let
                roles =
                    metadata.roles
            in
                { metadata | roles = { roles | groupBy = Just i } }

        ToggleGeopoint i ->
            let
                roles =
                    metadata.roles

                roles_ =
                    if roles.geopoint == Just i then
                        { roles | geopoint = Nothing }
                    else
                        { roles
                            | geopoint = Just i
                            , taxaName = (roles.taxaName == Just i) ? Nothing <| roles.taxaName
                            , latitude = Nothing
                            , longitude = Nothing
                        }
            in
                { metadata | roles = roles_ }

        ToggleLatitude i ->
            let
                roles =
                    metadata.roles

                roles_ =
                    if roles.latitude == Just i then
                        { roles | latitude = Nothing }
                    else
                        { roles
                            | latitude = Just i
                            , geopoint = Nothing
                            , longitude = (roles.longitude == Just i) ? Nothing <| roles.longitude
                            , taxaName = (roles.taxaName == Just i) ? Nothing <| roles.taxaName
                        }
            in
                { metadata | roles = roles_ }

        ToggleLongitude i ->
            let
                roles =
                    metadata.roles

                roles_ =
                    if roles.longitude == Just i then
                        { roles | latitude = Nothing }
                    else
                        { roles
                            | longitude = Just i
                            , geopoint = Nothing
                            , latitude = (roles.latitude == Just i) ? Nothing <| roles.latitude
                            , taxaName = (roles.taxaName == Just i) ? Nothing <| roles.taxaName
                        }
            in
                { metadata | roles = roles_ }

        ToggleTaxaName i ->
            let
                roles =
                    metadata.roles

                roles_ =
                    if roles.taxaName == Just i then
                        { roles | taxaName = Nothing }
                    else
                        { roles
                            | taxaName = Just i
                            , geopoint = (roles.geopoint == Just i) ? Nothing <| roles.geopoint
                            , latitude = (roles.latitude == Just i) ? Nothing <| roles.latitude
                            , longitude = (roles.longitude == Just i) ? Nothing <| roles.longitude
                        }
            in
                { metadata | roles = roles_ }

        ToggleUniqueId i ->
            let
                roles =
                    metadata.roles

                uniqueId =
                    if roles.uniqueId == Just i then
                        Nothing
                    else
                        Just i
            in
                { metadata | roles = { roles | uniqueId = uniqueId } }


update : String -> Index -> Flags -> UploadMsg -> FileSelectState -> ( FileSelectState, Cmd msg )
update uploadType index flags msg state =
    case msg of
        FileSelectedMsg id ->
            if id == fileSelectId index then
                ( FileSelected, fileSelected id )
            else
                ( state, Cmd.none )

        GotFileNameMsg { id, filename, preview } ->
            if id == fileSelectId index then
                ( GotFileName { localFileName = filename, uploadAs = filename, metadata = initMetadata preview }, Cmd.none )
            else
                ( state, Cmd.none )

        MetadataMsg msg_ ->
            case state of
                GotFileName info ->
                    ( GotFileName { info | metadata = updateMetadata msg_ info.metadata }, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        UpdateUploadFilename uploadAs ->
            case state of
                GotFileName info ->
                    ( GotFileName { info | uploadAs = uploadAs }, Cmd.none )

                FileNameConflict info ->
                    ( FileNameConflict { info | uploadAs = uploadAs }, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        DoUpload ->
            case state of
                GotFileName info ->
                    doUpload uploadType index flags info

                FileNameConflict info ->
                    doUpload uploadType index flags info

                _ ->
                    ( state, Cmd.none )

        GotUploadStatus { id, status } ->
            if id == fileSelectId index then
                case state of
                    UploadingFile ({ localFileName, uploadAs } as info) ->
                        case status of
                            UploadComplete 409 _ ->
                                ( FileNameConflict
                                    { localFileName = localFileName
                                    , uploadAs = uploadAs
                                    }
                                , Cmd.none
                                )

                            UploadComplete 200 _ ->
                                ( Finished uploadAs, Cmd.none )

                            _ ->
                                ( UploadingFile { info | status = status }, Cmd.none )

                    _ ->
                        ( state, Cmd.none )
            else
                ( state, Cmd.none )


doUpload : String -> Index -> Flags -> { a | localFileName : String, uploadAs : String } -> ( FileSelectState, Cmd msg )
doUpload uploadType index flags { localFileName, uploadAs } =
    let
        url =
            flags.apiRoot ++ "upload?uploadType=" ++ uploadType ++ "&fileName=" ++ uploadAs
    in
        ( UploadingFile
            { localFileName = localFileName
            , uploadAs = uploadAs
            , status = StartingUpload
            }
        , uploadCmd { id = fileSelectId index, url = url }
        )


view : (Material.Msg msg -> msg) -> (UploadMsg -> msg) -> Index -> Material.Model -> FileSelectState -> List (Html msg)
view mapMdlMsg mapMsg index mdl state =
    case state of
        Finished filename ->
            [ Html.text <| "Uploaded " ++ filename ++ "." ]

        FileNameConflict { localFileName, uploadAs } ->
            [ Html.p [] [ Html.text ("File selected: " ++ localFileName) ]
            , Html.p [] [ Html.text "Filename already in use. Choose another." ]
            , Html.p []
                [ Textfield.render mapMdlMsg
                    (0 :: index)
                    mdl
                    [ Textfield.label "Upload as"
                    , Textfield.floatingLabel
                    , Textfield.value uploadAs
                    , Options.onInput (UpdateUploadFilename >> mapMsg)
                    ]
                    []
                ]
            , Button.render mapMdlMsg
                (1 :: index)
                mdl
                [ Button.raised, Options.onClick (mapMsg DoUpload) ]
                [ Html.text "Upload" ]
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

        GotFileName { localFileName, uploadAs, metadata } ->
            [ Html.p [] [ Html.text ("File selected: " ++ localFileName) ]
            , metadataTable mapMdlMsg mapMsg (2 :: index) mdl metadata
            , Html.p []
                [ Textfield.render mapMdlMsg
                    (0 :: index)
                    mdl
                    [ Textfield.label "Upload as"
                    , Textfield.floatingLabel
                    , Textfield.value uploadAs
                    , Options.onInput (UpdateUploadFilename >> mapMsg)
                    ]
                    []
                ]
            , Button.render mapMdlMsg
                (1 :: index)
                mdl
                [ Button.raised, Options.onClick (mapMsg DoUpload) ]
                [ Html.text "Upload" ]
            ]

        FileSelected ->
            [ Html.text "Opening file..." ]

        FileNotSelected ->
            [ Html.input
                [ Attribute.type_ "file"
                , Attribute.id <| fileSelectId index
                , Events.on "change" (Decode.succeed <| mapMsg <| FileSelectedMsg <| fileSelectId index)
                ]
                []
            ]


metadataTable : (Material.Msg msg -> msg) -> (UploadMsg -> msg) -> Index -> Material.Model -> Metadata -> Html msg
metadataTable mapMdlMsg mapMsg index mdl metadata =
    let
        formColumn i cell =
            [ Options.div [ Options.css "display" "flex", Options.css "flex-direction" "column" ]
                [ Textfield.render mapMdlMsg
                    (0 :: i :: index)
                    mdl
                    [ Textfield.floatingLabel
                    , Textfield.label "Column Name"
                    , Textfield.maxlength 10
                    , Textfield.value <| (metadata.fields |> List.getAt i |> Maybe.map .shortName |> Maybe.withDefault "")
                    , Options.onInput (UpdateFieldName i >> MetadataMsg >> mapMsg)
                    ]
                    []
                , Html.select [] <| List.map (\v -> Html.option [] [ Html.text v ]) [ "string", "integer", "real" ]
                , Toggles.radio mapMdlMsg
                    (0 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleGroupBy i |> MetadataMsg |> mapMsg)
                    , Toggles.value (metadata.roles.groupBy == Just i)
                    ]
                    [ Html.text "Group By" ]
                , Toggles.checkbox mapMdlMsg
                    (1 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleGeopoint i |> MetadataMsg |> mapMsg)
                    , Toggles.value (metadata.roles.geopoint == Just i)
                    ]
                    [ Html.text "Geopoint" ]
                , Toggles.checkbox mapMdlMsg
                    (2 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleLatitude i |> MetadataMsg |> mapMsg)
                    , Toggles.value (metadata.roles.latitude == Just i)
                    ]
                    [ Html.text "Latitude" ]
                , Toggles.checkbox mapMdlMsg
                    (3 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleLongitude i |> MetadataMsg |> mapMsg)
                    , Toggles.value (metadata.roles.longitude == Just i)
                    ]
                    [ Html.text "Longitude" ]
                , Toggles.checkbox mapMdlMsg
                    (4 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleTaxaName i |> MetadataMsg |> mapMsg)
                    , Toggles.value (metadata.roles.taxaName == Just i)
                    ]
                    [ Html.text "Taxon" ]
                , Toggles.checkbox mapMdlMsg
                    (5 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleUniqueId i |> MetadataMsg |> mapMsg)
                    , Toggles.value (metadata.roles.uniqueId == Just i)
                    ]
                    [ Html.text "Unique ID" ]
                ]
            ]

        header =
            metadata.preview
                |> List.getAt 0
                |> Maybe.withDefault []
                |> List.indexedMap formColumn
                |> List.map (Html.td [])
                |> Html.tr []

        previewRows =
            metadata.preview |> List.map (List.map (\cell -> Html.td [] [ Html.text cell ]) >> Html.tr [])
    in
        Html.table [] (header :: previewRows)


subscriptions : (UploadMsg -> msg) -> Sub msg
subscriptions mapMsg =
    Sub.batch
        [ selectedFileName (GotFileNameMsg >> mapMsg)
        , uploadProgress
            (\{ id, total, loaded } ->
                GotUploadStatus { id = id, status = Uploading ( loaded, total ) } |> mapMsg
            )
        , uploadComplete
            (\{ id, response, status } ->
                GotUploadStatus { id = id, status = UploadComplete status response } |> mapMsg
            )
        , uploadFailed
            (\{ id, response } ->
                GotUploadStatus { id = id, status = UploadFailed response } |> mapMsg
            )
        , uploadCanceled
            (\id ->
                GotUploadStatus { id = id, status = UploadCanceled } |> mapMsg
            )
        ]
