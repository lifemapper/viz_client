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


module OccurrenceSetsView exposing (Model, toApi, Msg, update, view, init, problems)

import ProgramFlags exposing (Flags)
import Decoder
    exposing
        ( AtomObjectRecord
        , decodeOccurrenceSet
        , OccurrenceSetRecord
        , OccurrenceSet(..)
        , SingleLayerMap(..)
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


type alias Model =
    { occurrenceSets : List AtomObjectRecord
    , chooser : OccurrenceSetChooser.Model
    , mappedSet : Maybe OccurrenceSetRecord
    , mapCard : MapCard.Model
    , mdl : Material.Model
    , programFlags : Flags
    }


toApi : Model -> Decoder.BoomOccurrenceSet
toApi =
    .occurrenceSets
        >> List.map (.id)
        >> Decoder.BoomOccurrenceSetOccurrence_ids
        >> (\ids -> { occurrence_ids = Just ids, point_count_min = Nothing, points_filename = Nothing })
        >> Decoder.BoomOccurrenceSet


type Msg
    = ChooserMsg OccurrenceSetChooser.Msg
    | Remove Int
    | SetMapped (Maybe OccurrenceSetRecord)
    | MapOccurrences Int
    | MapCardMsg MapCard.Msg
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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


setMap : Maybe Leaflet.WMSInfo -> Model -> ( Model, Cmd Msg )
setMap wmsInfo =
    Helpers.lift
        .mapCard
        (\m x -> { m | mapCard = x })
        MapCardMsg
        MapCard.update
        (MapCard.SetMap wmsInfo)


updateMap : Model -> ( Model, Cmd Msg )
updateMap model =
    let
        mapInfo =
            model.mappedSet
                |> Maybe.andThen (\{ map } -> map)
                |> Maybe.map
                    (\(SingleLayerMap { endpoint, mapName, layerName }) ->
                        { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
                    )
    in
        setMap mapInfo model


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
    , mapCard = MapCard.init Nothing
    , mdl = Material.model
    , programFlags = flags
    }


occurrenceSetLI : Model -> Index -> Int -> AtomObjectRecord -> Html Msg
occurrenceSetLI model index i o =
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
                (List.indexedMap (occurrenceSetLI model index) model.occurrenceSets)
                [ (OccurrenceSetChooser.view index model.chooser |> Html.map ChooserMsg) ]
        ]


view : Index -> Model -> Html Msg
view index model =
    let
        mapCardTitle =
            model.mappedSet |> Maybe.andThen .speciesName |> Maybe.withDefault "Map"
    in
        Options.div [ Options.css "display" "flex" ]
            [ occurrenceSetList index model
            , MapCard.view index mapCardTitle model.mapCard |> Html.map MapCardMsg
            ]


problems : Model -> Maybe String
problems model =
    case model.occurrenceSets of
        [] ->
            Just "No occurrence sets chosen."

        _ ->
            Nothing
