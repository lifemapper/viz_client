module OccurrenceSetsView exposing (..)

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
import Helpers exposing (Index)
import MapCard


type alias Model =
    { occurrenceSets : List AtomObjectRecord
    , chooser : OccurrenceSetChooser.Model
    , mappedSet : Maybe OccurrenceSetRecord
    , mapCard : MapCard.Model
    , mdl : Material.Model
    }


type Msg
    = ChooserMsg OccurrenceSetChooser.Msg
    | Remove Int
    | SetMapped (Maybe OccurrenceSetRecord)
    | MapOccurrences Int
    | MapCardMsg MapCard.Msg
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Remove i ->
            ( { model
                | occurrenceSets =
                    List.append
                        (List.take i model.occurrenceSets)
                        (List.drop (i + 1) model.occurrenceSets)
              }
            , Cmd.none
            )

        MapOccurrences id ->
            ( model, getMetadata id )

        SetMapped o ->
            Helpers.lift
                .mapCard
                (\m x -> { m | mapCard = x })
                MapCardMsg
                MapCard.update
                (updateMap o)
                ({ model | mappedSet = o })

        MapCardMsg msg_ ->
            Helpers.lift
                .mapCard
                (\m x -> { m | mapCard = x })
                MapCardMsg
                MapCard.update
                msg_
                model

        ChooserMsg msg_ ->
            Helpers.lift
                .chooser
                (\m x -> { m | chooser = x })
                ChooserMsg
                OccurrenceSetChooser.update
                msg_
                (case msg_ of
                    OccurrenceSetChooser.Select object ->
                        { model | occurrenceSets = model.occurrenceSets ++ [ object ] }

                    msg_ ->
                        model
                )


updateMap : Maybe OccurrenceSetRecord -> MapCard.Msg
updateMap =
    Maybe.andThen (\{ map } -> map)
        >> Maybe.map
            (\(SingleLayerMap { endpoint, mapName, layerName }) ->
                { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
            )
        >> MapCard.SetMap


getMetadata : Int -> Cmd Msg
getMetadata id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://notyeti-191.lifemapper.org/api/v2/occurrence/" ++ (toString id)
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


init : Model
init =
    { occurrenceSets = []
    , mappedSet = Nothing
    , chooser = OccurrenceSetChooser.init
    , mapCard = MapCard.init "leaflet-map-occurrence-sets"
    , mdl = Material.model
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
            , L.content2 []
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
    Options.div [ Options.css "display" "flex" ]
        [ occurrenceSetList index model
        , MapCard.view index "Map" model.mapCard |> Html.map MapCardMsg
        ]
