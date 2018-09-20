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


module OccurrenceMetadata exposing (Metadata, initMetadata, MetadataMsg, updateMetadata, metadataTable)

import List.Extra as List
import Ternary exposing ((?))
import Html exposing (Html)
import Material
import Material.Toggles as Toggles
import Material.Textfield as Textfield
import Material.Options as Options
import Helpers exposing (Index)


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


type MetadataMsg
    = UpdateFieldName Int String
    | ToggleGroupBy Int
    | ToggleGeopoint Int
    | ToggleLatitude Int
    | ToggleLongitude Int
    | ToggleTaxaName Int
    | ToggleUniqueId Int


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


metadataTable : (Material.Msg msg -> msg) -> (MetadataMsg -> msg) -> Index -> Material.Model -> Metadata -> Html msg
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
                    , Options.onInput (UpdateFieldName i >> mapMsg)
                    ]
                    []
                , Html.select [] <| List.map (\v -> Html.option [] [ Html.text v ]) [ "string", "integer", "real" ]
                , Toggles.radio mapMdlMsg
                    (0 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleGroupBy i |> mapMsg)
                    , Toggles.value (metadata.roles.groupBy == Just i)
                    ]
                    [ Html.text "Group By" ]
                , Toggles.checkbox mapMdlMsg
                    (1 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleGeopoint i |> mapMsg)
                    , Toggles.value (metadata.roles.geopoint == Just i)
                    ]
                    [ Html.text "Geopoint" ]
                , Toggles.checkbox mapMdlMsg
                    (2 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleLatitude i |> mapMsg)
                    , Toggles.value (metadata.roles.latitude == Just i)
                    ]
                    [ Html.text "Latitude" ]
                , Toggles.checkbox mapMdlMsg
                    (3 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleLongitude i |> mapMsg)
                    , Toggles.value (metadata.roles.longitude == Just i)
                    ]
                    [ Html.text "Longitude" ]
                , Toggles.checkbox mapMdlMsg
                    (4 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleTaxaName i |> mapMsg)
                    , Toggles.value (metadata.roles.taxaName == Just i)
                    ]
                    [ Html.text "Taxon" ]
                , Toggles.checkbox mapMdlMsg
                    (5 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleUniqueId i |> mapMsg)
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
