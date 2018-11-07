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


module OccurrenceSetsView exposing (Model, toApi, Msg, update, view, init, problems, subscriptions)

import ProgramFlags exposing (Flags)
import Decoder
    exposing
        ( AtomObjectRecord
        , decodeOccurrenceSet
        , OccurrenceSetRecord
        , OccurrenceSet(..)
        , SingleLayerMap(..)
        , SpatialVector(..)
        , SpatialVectorBbox(..)
        )
import OccurrenceSetChooser
import Leaflet
import Material
import Material.Options as Options
import Material.Typography as Typo
import Material.List as L
import Material.Helpers as Helpers
import Html exposing (Html)
import Http
import List.Extra exposing (removeAt)
import Helpers exposing (Index, chain)
import MapCard
import UploadFile
import OccurrenceFromTaxonomy


type OccurrenceSource
    = Choose
    | Upload UploadFile.Model
    | Taxonomy OccurrenceFromTaxonomy.Model


type alias Model =
    { occurrenceSets : List AtomObjectRecord
    , chooser : OccurrenceSetChooser.Model
    , mappedSet : Maybe OccurrenceSetRecord
    , mapCard : MapCard.Model
    , source : OccurrenceSource
    , mdl : Material.Model
    , programFlags : Flags
    }


toApi : Model -> Decoder.BoomOccurrenceSet
toApi model =
    case model.source of
        Choose ->
            Decoder.BoomOccurrenceSet
                { occurrence_ids =
                    model.occurrenceSets
                        |> List.map (.id)
                        |> Decoder.BoomOccurrenceSetOccurrence_ids
                        |> Just
                , points_filename = Nothing
                , point_count_min = Nothing
                , taxon_ids = Nothing
                , taxon_names = Nothing
                }

        Upload upload ->
            Decoder.BoomOccurrenceSet
                { occurrence_ids = Nothing
                , points_filename = upload |> UploadFile.getUploadedFilename
                , point_count_min = Nothing
                , taxon_ids = Nothing
                , taxon_names = Nothing
                }

        Taxonomy taxonomy ->
            Decoder.BoomOccurrenceSet
                { occurrence_ids = Nothing
                , points_filename = Nothing
                , point_count_min = Nothing
                , taxon_ids = Just (taxonomy |> OccurrenceFromTaxonomy.getTaxonIds)
                , taxon_names = Nothing
                }


type Msg
    = ChooserMsg OccurrenceSetChooser.Msg
    | Remove Int
    | SetMapped (Maybe OccurrenceSetRecord)
    | MapOccurrences Int
    | MapCardMsg MapCard.Msg
    | UploadMsg UploadFile.Msg
    | TaxonomyMsg OccurrenceFromTaxonomy.Msg
    | ToggleWantToUpload
    | Mdl (Material.Msg Msg)


update : Index -> Msg -> Model -> ( Model, Cmd Msg )
update index msg model =
    let
        liftedChooserUpdate =
            Helpers.lift
                .chooser
                (\m x -> { m | chooser = x })
                ChooserMsg
                OccurrenceSetChooser.update

        liftedMapCardUpdate =
            Helpers.lift
                .mapCard
                (\m x -> { m | mapCard = x })
                MapCardMsg
                MapCard.update
    in
        case msg of
            Mdl msg_ ->
                Material.update Mdl msg_ model

            Remove i ->
                ( { model | occurrenceSets = removeAt i model.occurrenceSets }, Cmd.none )

            MapOccurrences id ->
                ( model, getMetadataAndMap model.programFlags id )

            SetMapped o ->
                updateMap { model | mappedSet = o }

            MapCardMsg msg_ ->
                liftedMapCardUpdate msg_ model

            UploadMsg msg_ ->
                case model.source of
                    Upload upload ->
                        let
                            ( upload_, cmd ) =
                                UploadFile.update UploadFile.Occurrence (1 :: index) model.programFlags msg_ upload
                        in
                            ( { model | source = Upload upload_ }, cmd )

                    _ ->
                        ( model, Cmd.none )

            TaxonomyMsg msg_ ->
                case model.source of
                    Taxonomy tax ->
                        let
                            ( tax_, cmd ) =
                                OccurrenceFromTaxonomy.update msg_ tax
                        in
                            ( { model | source = Taxonomy tax_ }, Cmd.map TaxonomyMsg cmd )

                    _ ->
                        ( model, Cmd.none )

            ToggleWantToUpload ->
                case model.source of
                    Choose ->
                        ( { model | source = Upload UploadFile.init }, Cmd.none )

                    _ ->
                        ( { model | source = Choose }, Cmd.none )

            ChooserMsg msg_ ->
                chain (addSelected msg_) (liftedChooserUpdate msg_) model


addSelected : OccurrenceSetChooser.Msg -> Model -> ( Model, Cmd Msg )
addSelected msg model =
    case msg of
        OccurrenceSetChooser.Select object ->
            ( { model | occurrenceSets = model.occurrenceSets ++ [ object ] }
            , getMetadataAndMap model.programFlags object.id
            )

        msg ->
            ( model, Cmd.none )


setMap : Maybe Leaflet.BoundingBox -> Maybe Leaflet.WMSInfo -> Model -> ( Model, Cmd Msg )
setMap bb wmsInfo =
    Helpers.lift
        .mapCard
        (\m x -> { m | mapCard = x })
        MapCardMsg
        MapCard.update
        (MapCard.SetMap bb wmsInfo)


updateMap : Model -> ( Model, Cmd Msg )
updateMap model =
    let
        bb =
            model.mappedSet
                |> Maybe.andThen .spatialVector
                |> Maybe.andThen (\(SpatialVector { bbox }) -> bbox)
                |> Maybe.andThen
                    (\(SpatialVectorBbox bbox) ->
                        case bbox of
                            [ lng1, lat1, lng2, lat2 ] ->
                                Just (Leaflet.BoundingBox lat1 lng1 lat2 lng2)

                            _ ->
                                Debug.log "bad bounding box" (toString bbox) |> always Nothing
                    )

        mapInfo =
            model.mappedSet
                |> Maybe.andThen .map
                |> Maybe.map
                    (\(SingleLayerMap { endpoint, mapName, layerName }) ->
                        { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
                    )
    in
        setMap bb mapInfo model


getMetadataAndMap : Flags -> Int -> Cmd Msg
getMetadataAndMap flags id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = flags.apiRoot ++ "occurrence/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson decodeOccurrenceSet
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotMetadata


gotMetadata : Result Http.Error OccurrenceSet -> Msg
gotMetadata result =
    case result of
        Ok (OccurrenceSet o) ->
            SetMapped (Just o)

        Err err ->
            SetMapped Nothing
                |> Debug.log (toString err)


init : Flags -> Model
init flags =
    { occurrenceSets = []
    , mappedSet = Nothing
    , chooser = OccurrenceSetChooser.init flags
    , mapCard = MapCard.init Nothing Nothing
    , source = Taxonomy <| OccurrenceFromTaxonomy.init flags
    , mdl = Material.model
    , programFlags = flags
    }


occurrenceSetLI : Model -> Int -> AtomObjectRecord -> Html Msg
occurrenceSetLI model i o =
    let
        iconName =
            if Just o.id == Maybe.map .id model.mappedSet then
                "visibility"
            else
                "visibility_off"

        icon =
            L.icon iconName [ Options.onClick (MapOccurrences o.id) ]
    in
        L.li []
            [ L.content [] [ Html.text o.name ]
            , L.content2 [ Options.css "flex-flow" "row" ]
                [ L.icon "delete" [ Options.onClick (Remove i) ]
                , icon
                ]
            ]


occurrenceSetList : Index -> Model -> Html Msg
occurrenceSetList index model =
    Options.div [ Options.css "margin" "20px" ]
        [ Options.styled Html.p [ Typo.title ] [ Html.text "Choose Occurrence Sets" ]
        , L.ul [] <|
            List.append
                (List.indexedMap (occurrenceSetLI model) model.occurrenceSets)
                [ (OccurrenceSetChooser.view (0 :: index) model.chooser |> Html.map ChooserMsg) ]
        , Options.styled Html.a
            [ Options.onClick ToggleWantToUpload, Options.css "cursor" "pointer" ]
            [ Html.text "or upload data" ]
        ]


view : Index -> Model -> Html Msg
view index model =
    let
        mapCardTitle =
            model.mappedSet |> Maybe.andThen .speciesName |> Maybe.withDefault "Map"
    in
        case model.source of
            Choose ->
                Options.div [ Options.css "display" "flex" ]
                    [ occurrenceSetList index model
                    , MapCard.view index mapCardTitle model.mapCard |> Html.map MapCardMsg
                    ]

            Upload upload ->
                Options.div [ Options.css "display" "flex" ]
                    [ Options.div [ Options.css "margin" "20px" ]
                        [ Options.div [ Options.css "margin-bottom" "10px" ]
                            (UploadFile.view Mdl UploadMsg (1 :: index) model.mdl upload)
                        , Options.styled Html.a
                            [ Options.onClick ToggleWantToUpload, Options.css "cursor" "pointer" ]
                            [ Html.text "or select existing data" ]
                        ]
                    ]

            Taxonomy model_ ->
                Options.div [ Options.css "display" "flex" ]
                    [ Options.div [ Options.css "margin" "20px" ]
                        [ Options.div [ Options.css "margin-bottom" "10px" ]
                            [ OccurrenceFromTaxonomy.view model_ |> Html.map TaxonomyMsg ]
                          -- (UploadFile.view Mdl UploadMsg (1 :: index) model.mdl upload)
                        , Options.styled Html.a
                            [ Options.onClick ToggleWantToUpload, Options.css "cursor" "pointer" ]
                            [ Html.text "or select existing data" ]
                        ]
                    ]


problems : Model -> Maybe String
problems model =
    case model.source of
        Upload upload ->
            if (upload |> UploadFile.getUploadedFilename) == Nothing then
                Just "No species occurrences file uploaded."
            else
                Nothing

        Choose ->
            if model.occurrenceSets == [] then
                Just "No occurrence sets chosen."
            else
                Nothing

        Taxonomy taxonomy ->
            if taxonomy.speciesForOccurrences == [] then
                Just "No species chosen for occurrence points."
            else
                Nothing


subscriptions : Sub Msg
subscriptions =
    UploadFile.subscriptions UploadMsg
