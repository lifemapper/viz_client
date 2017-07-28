module OccurrenceSetsView exposing (Model, toApi, Msg, update, view, init, subscriptions, problems)

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
import Material
import Material.Options as Options
import Material.Typography as Typo
import Material.List as L
import Material.Helpers as Helpers
import Html exposing (Html)
import Html.Events
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


toApi : Model -> Decoder.BoomPOSTOccurrenceSets
toApi =
    .occurrenceSets
        >> List.map
            (\o ->
                Decoder.BoomPOSTOccurrenceSetsItem
                    { occurrenceSetId = Just o.id
                    , occurrenceData = Nothing
                    , occurrenceMeta = Nothing
                    }
            )
        >> Decoder.BoomPOSTOccurrenceSets


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


setMap : Maybe MapCard.MapInfo -> Model -> ( Model, Cmd Msg )
setMap =
    MapCard.setMap .mapCard (\m x -> { m | mapCard = x }) MapCardMsg


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
    , mapCard = MapCard.init "leaflet-map-occurrence-sets"
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
            L.icon iconName [ Options.attribute <| Html.Events.onClick (MapOccurrences o.id) ]
    in
        L.li []
            [ L.content [] [ Html.text o.name ]
            , L.content2 [ Options.css "flex-flow" "row" ]
                [ L.icon "delete" [ Options.attribute <| Html.Events.onClick (Remove i) ]
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


subscriptions : (Msg -> msg) -> Sub msg
subscriptions liftMsg =
    MapCard.subscriptions (MapCardMsg >> liftMsg)
