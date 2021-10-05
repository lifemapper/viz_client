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


module OccurrenceMetadata
    exposing
        ( Metadata
        , initMetadata
        , MetadataIssues(..)
        , validateMetadata
        , MetadataMsg
        , updateMetadata
        , metadataTable
        , toJson
        )

import List.Extra as List
import Maybe.Extra as Maybe
import Ternary exposing ((?))
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Material
import Material.Toggles as Toggles
import Material.Textfield as Textfield
import Material.Options as Options
import Helpers exposing (Index)
import Json.Encode exposing (encode)
import Encoder exposing (encodeOccurrenceMetadata)
import Decoder
    exposing
        ( OccurrenceMetadata(..)
        , OccurrenceMetadataField(..)
        , OccurrenceMetadataFieldItem(..)
        , OccurrenceMetadataFieldItemField_type(..)
        , OccurrenceMetadataRole(..)
        )


type alias Metadata =
    { fields : List { short_name : String, field_type : String }
    , roles :
        { group_by : Maybe Int
        , geopoint : Maybe Int
        , latitude : Maybe Int
        , longitude : Maybe Int
        , taxa_name : Maybe Int
        , unique_id : Maybe Int
        }
    , preview : List (List String)
    , delimiter : String
    }


toJson : Metadata -> String
toJson metadata =
    let
        fields =
            metadata.fields
                |> List.indexedMap
                    (\i field ->
                        OccurrenceMetadataFieldItem
                            { key = Just (i |> toString)
                            , short_name = Just field.short_name
                            , field_type =
                                case field.field_type of
                                    "string" ->
                                        Just String

                                    "integer" ->
                                        Just Integer

                                    "real" ->
                                        Just Real

                                    _ ->
                                        Nothing
                            }
                    )

        roles =
            OccurrenceMetadataRole
                { unique_id = metadata.roles.unique_id |> Maybe.map toString
                , taxa_name = metadata.roles.taxa_name |> Maybe.map toString |> Maybe.withDefault ""
                , longitude = metadata.roles.longitude |> Maybe.map toString |> Maybe.withDefault ""
                , latitude = metadata.roles.latitude |> Maybe.map toString |> Maybe.withDefault ""
                , group_by = metadata.roles.group_by |> Maybe.map toString
                }
    in
        OccurrenceMetadata { role = Just roles, field = Just <| OccurrenceMetadataField fields, delimiter = Just metadata.delimiter }
            |> encodeOccurrenceMetadata
            |> encode 0


initMetadata : String -> List (List String) -> Metadata
initMetadata delimiter preview =
    { fields =
        preview
            |> List.getAt 0
            |> Maybe.withDefault []
            |> truncAndDeDup
            |> List.map (\col_name -> { short_name = col_name, field_type = "string" })
    , roles =
        { group_by = Nothing
        , geopoint = Nothing
        , latitude = Nothing
        , longitude = Nothing
        , taxa_name = Nothing
        , unique_id = Nothing
        }
    , preview = preview
    , delimiter = delimiter
    }


truncAndDeDup : List String -> List String
truncAndDeDup strings =
    strings
        |> List.map (String.left 10)
        |> List.foldl (\s result -> result ++ [ replaceTailUntilUnique result s 0 ]) []


replaceTailUntilUnique : List String -> String -> Int -> String
replaceTailUntilUnique used string count =
    let
        new =
            replaceTailWith count string
    in
        if List.member new used then
            replaceTailUntilUnique used string (count + 1)
        else
            new


replaceTailWith : Int -> String -> String
replaceTailWith count string =
    if count < 1 then
        string
    else
        let
            newTail =
                toString count
        in
            (string |> String.dropRight (String.length newTail)) ++ newTail


type MetadataIssues
    = MissingGeo
    | MissingTaxon
    | DuplicatedNames (List String)


validateMetadata : Metadata -> List MetadataIssues
validateMetadata { roles, fields } =
    [ if roles.geopoint == Nothing && (roles.latitude == Nothing || roles.longitude == Nothing) then
        Just MissingGeo
      else
        Nothing
    , if roles.taxa_name == Nothing then
        Just MissingTaxon
      else
        Nothing
    , case fields |> List.map .short_name |> List.sort |> List.group |> List.filter (\vs -> List.length vs > 1) of
        [] ->
            Nothing

        dupedNames ->
            List.filterMap List.head dupedNames |> DuplicatedNames |> Just
    ]
        |> List.concatMap Maybe.toList


type MetadataMsg
    = UpdateFieldName Int String
    | UpdateFieldType Int String
    | ToggleGroupBy Int
    | ToggleGeopoint Int
    | ToggleLatitude Int
    | ToggleLongitude Int
    | ToggleTaxaName Int
    | ToggleUniqueId Int


updateMetadata : MetadataMsg -> Metadata -> Metadata
updateMetadata msg metadata =
    case msg of
        UpdateFieldName i name ->
            let
                fields =
                    metadata.fields
                        |> List.indexedMap
                            (\j field ->
                                if j == i then
                                    { field | short_name = name }
                                else
                                    field
                            )
            in
                { metadata | fields = fields }

        UpdateFieldType i field_type ->
            let
                fields =
                    metadata.fields
                        |> List.indexedMap
                            (\j field ->
                                if j == i then
                                    { field | field_type = field_type }
                                else
                                    field
                            )
            in
                { metadata | fields = fields }

        ToggleGroupBy i ->
            let
                roles =
                    metadata.roles

                roles_ =
                    if roles.group_by == Just i then
                        { roles | group_by = Nothing }
                    else
                        { roles | group_by = Just i }
            in
                { metadata | roles = roles_ }

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
                            , taxa_name = (roles.taxa_name == Just i) ? Nothing <| roles.taxa_name
                            , latitude = Nothing
                            , longitude = Nothing
                        }

                fields =
                    metadata.fields
                        |> List.indexedMap
                            (\j field ->
                                if j == i then
                                    { field | field_type = "string" }
                                else
                                    field
                            )
            in
                { metadata | roles = roles_, fields = fields }

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
                            , taxa_name = (roles.taxa_name == Just i) ? Nothing <| roles.taxa_name
                        }

                fields =
                    metadata.fields
                        |> List.indexedMap
                            (\j field ->
                                if j == i then
                                    { field | field_type = "real" }
                                else
                                    field
                            )
            in
                { metadata | roles = roles_, fields = fields }

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
                            , taxa_name = (roles.taxa_name == Just i) ? Nothing <| roles.taxa_name
                        }

                fields =
                    metadata.fields
                        |> List.indexedMap
                            (\j field ->
                                if j == i then
                                    { field | field_type = "real" }
                                else
                                    field
                            )
            in
                { metadata | roles = roles_, fields = fields }

        ToggleTaxaName i ->
            let
                roles =
                    metadata.roles

                roles_ =
                    if roles.taxa_name == Just i then
                        { roles | taxa_name = Nothing }
                    else
                        { roles
                            | taxa_name = Just i
                            , geopoint = (roles.geopoint == Just i) ? Nothing <| roles.geopoint
                            , latitude = (roles.latitude == Just i) ? Nothing <| roles.latitude
                            , longitude = (roles.longitude == Just i) ? Nothing <| roles.longitude
                        }

                fields =
                    metadata.fields
                        |> List.indexedMap
                            (\j field ->
                                if j == i then
                                    { field | field_type = "string" }
                                else
                                    field
                            )
            in
                { metadata | roles = roles_, fields = fields }

        ToggleUniqueId i ->
            let
                roles =
                    metadata.roles

                unique_id =
                    if roles.unique_id == Just i then
                        Nothing
                    else
                        Just i
            in
                { metadata | roles = { roles | unique_id = unique_id } }


metadataTable : (Material.Msg msg -> msg) -> (MetadataMsg -> msg) -> Index -> Material.Model -> Metadata -> Html msg
metadataTable mapMdlMsg mapMsg index mdl metadata =
    let
        formColumn i cell =
            [ Options.div [ Options.css "display" "flex", Options.css "flex-direction" "column", Options.css "min-width" "120px" ]
                [ Textfield.render mapMdlMsg
                    (0 :: i :: index)
                    mdl
                    [ Textfield.floatingLabel
                    , Textfield.label "Column Name"
                    , Textfield.maxlength 10
                    , Textfield.value (metadata.fields |> List.getAt i |> Maybe.map .short_name |> Maybe.withDefault "")
                    , Options.onInput (UpdateFieldName i >> mapMsg)
                    ]
                    []
                  -- , Toggles.checkbox mapMdlMsg
                  --     (1 :: 1 :: i :: index)
                  --     mdl
                  --     [ Options.onToggle (ToggleGeopoint i |> mapMsg)
                  --     , Toggles.value (metadata.roles.geopoint == Just i)
                  --     , Options.css "display" "none"
                  --     ]
                  --     [ Html.text "Geopoint" ]
                , Toggles.radio mapMdlMsg
                    (2 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleLatitude i |> mapMsg)
                    , Toggles.value (metadata.roles.latitude == Just i)
                    ]
                    [ Html.text "Latitude" ]
                , Toggles.radio mapMdlMsg
                    (3 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleLongitude i |> mapMsg)
                    , Toggles.value (metadata.roles.longitude == Just i)
                    ]
                    [ Html.text "Longitude" ]
                , Toggles.radio mapMdlMsg
                    (4 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleTaxaName i |> mapMsg)
                    , Toggles.value (metadata.roles.taxa_name == Just i)
                    ]
                    [ Html.text "Taxon" ]
                , Toggles.checkbox mapMdlMsg
                    (5 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleUniqueId i |> mapMsg)
                    , Toggles.value (metadata.roles.unique_id == Just i)
                    ]
                    [ Html.text "Unique ID" ]
                , Toggles.checkbox mapMdlMsg
                    (0 :: 1 :: i :: index)
                    mdl
                    [ Options.onToggle (ToggleGroupBy i |> mapMsg)
                    , Toggles.value (metadata.roles.group_by == Just i)
                    ]
                    [ Html.text "Group By" ]
                , Html.select [ Html.Events.onInput (UpdateFieldType i >> mapMsg) ] <|
                    List.map
                        (\v ->
                            Html.option
                                [ Html.Attributes.selected
                                    (metadata.fields
                                        |> List.getAt i
                                        |> Maybe.map .field_type
                                        |> Maybe.withDefault ""
                                        |> (==) v
                                    )
                                ]
                                [ Html.text v ]
                        )
                        [ "string", "integer", "real" ]
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
            metadata.preview
                |> List.map
                    (List.map
                        (\cell ->
                            Html.td [ Html.Attributes.style [ ( "border", "1px solid" ), ( "padding", "5px" ) ] ]
                                [ Html.text cell ]
                        )
                        >> Html.tr []
                    )
    in
        Options.div [ Options.css "overflow-x" "auto" ]
            [ Html.table [] (header :: previewRows) ]
