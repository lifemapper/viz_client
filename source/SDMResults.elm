module SDMResults exposing (Model, init, update, page, Msg(LoadProjections), subscriptions)

import List.Extra as List
import Html exposing (Html)
import Http
import Decoder
import ProgramFlags exposing (Flags)
import Page exposing (Page)
import MapCard
import Material
import Material.Options as Options


type alias Model =
    { programFlags : Flags
    , projectionsToLoad : Maybe Int
    , projections : List Decoder.ProjectionRecord
    , maps : List MapCard.Model
    , mdl : Material.Model
    }


init : Flags -> Model
init flags =
    { programFlags = flags
    , projectionsToLoad = Nothing
    , projections = []
    , maps = []
    , mdl = Material.model
    }


type Msg
    = LoadProjections Int
    | GotProjectionAtoms (List Decoder.AtomObjectRecord)
    | GotProjection Decoder.ProjectionRecord
    | MapCardMsg Int MapCard.Msg
    | Nop
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        LoadProjections gridsetId ->
            ( { model | projectionsToLoad = Nothing, projections = [], maps = [] }
            , loadProjections model.programFlags gridsetId
            )

        GotProjectionAtoms atoms ->
            ( { model | projectionsToLoad = Just (List.length atoms) }
            , atoms |> List.map (loadMetadata model.programFlags) |> Cmd.batch
            )

        GotProjection p ->
            initMap p model

        MapCardMsg i msg_ ->
            List.getAt i model.maps
                |> Maybe.map (MapCard.update msg_)
                |> Maybe.andThen
                    (\( map_, cmd ) ->
                        List.setAt i map_ model.maps
                            |> Maybe.map (\maps -> ( { model | maps = maps }, Cmd.map (MapCardMsg i) cmd ))
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


initMap : Decoder.ProjectionRecord -> Model -> ( Model, Cmd Msg )
initMap projection model =
    let
        map =
            MapCard.init ("projection-" ++ (toString projection.id))

        mapInfo =
            projection.map
                |> Maybe.map
                    (\(Decoder.SingleLayerMap { endpoint, mapName, layerName }) ->
                        { endPoint = endpoint, mapName = mapName, layers = [ layerName ] }
                    )
    in
        MapCard.setMap
            (always map)
            (\m x -> { m | projections = projection :: m.projections, maps = x :: m.maps })
            (MapCardMsg (List.length model.maps))
            mapInfo
            model


loadProjections : Flags -> Int -> Cmd Msg
loadProjections { apiRoot } id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "sdmProject?user=anon&gridsetid=" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotProjectionAtoms


gotProjectionAtoms : Result Http.Error Decoder.AtomList -> Msg
gotProjectionAtoms result =
    case result of
        Ok (Decoder.AtomList atoms) ->
            atoms |> List.map (\(Decoder.AtomObject o) -> o) |> GotProjectionAtoms

        Err err ->
            Debug.log "Error fetching projections" (toString err) |> always Nop


loadMetadata : Flags -> Decoder.AtomObjectRecord -> Cmd Msg
loadMetadata { apiRoot } { id } =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "sdmProject/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeProjection
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotMetadata


gotMetadata : Result Http.Error Decoder.Projection -> Msg
gotMetadata result =
    case result of
        Ok (Decoder.Projection p) ->
            GotProjection p

        Err err ->
            Debug.log "Failed to load projection" err |> always Nop


view : Model -> Html Msg
view =
    .maps
        >> List.indexedMap viewMap
        >> (Options.div
                [ Options.css "display" "flex"
                , Options.css "justify-content" "space-around"
                ]
           )


viewMap : Int -> MapCard.Model -> Html Msg
viewMap i map =
    MapCard.view [ i ] "Map" map |> Html.map (MapCardMsg i)


page : Page Model Msg
page =
    { view = view
    , selectedTab = always 0
    , selectTab = always Nop
    , tabTitles = always []
    }


subscriptions : (Msg -> msg) -> Model -> Sub msg
subscriptions liftMsg model =
    model.maps
        |> List.indexedMap (\i map -> MapCard.subscriptions (MapCardMsg i))
        |> Sub.batch
        |> Sub.map liftMsg
