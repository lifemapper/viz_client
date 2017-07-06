module MapCard exposing (Model, update, Msg, view, init, subscriptions, setMap, MapInfo)

import Maybe.Extra as Maybe
import List.Extra as List
import Material
import Material.Options as Options
import Material.Card as Card
import Material.Menu as Menu
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Helpers exposing (lift)
import Html exposing (Html)
import Helpers exposing (Index)
import Leaflet


type alias MapInfo =
    Leaflet.WMSInfo


type alias Model =
    { mapInfo : Maybe MapInfo
    , mapLayer : Int
    , leaflet : Leaflet.Model
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | LeafletMsg Leaflet.Msg
    | SetLayer Int
    | SetMap (Maybe MapInfo)


setMap : (model -> Model) -> (model -> Model -> model) -> (Msg -> msg) -> Maybe MapInfo -> model -> ( model, Cmd msg )
setMap getter setter liftMsg mapInfo =
    lift getter setter liftMsg update (SetMap mapInfo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        LeafletMsg msg_ ->
            lift
                .leaflet
                (\m l -> { m | leaflet = l })
                LeafletMsg
                Leaflet.update
                msg_
                model

        SetMap mapInfo ->
            updateMap { model | mapInfo = mapInfo, mapLayer = 0 }

        SetLayer layer ->
            updateMap { model | mapLayer = layer }


updateMap : Model -> ( Model, Cmd Msg )
updateMap model =
    setWMS (model.mapInfo |> Maybe.map (selectLayers model.mapLayer)) model


selectLayers : Int -> MapInfo -> MapInfo
selectLayers i mapInfo =
    { mapInfo
        | layers =
            "bmng" :: (mapInfo.layers |> List.getAt i |> Maybe.toList)
    }


setWMS : Maybe MapInfo -> Model -> ( Model, Cmd Msg )
setWMS =
    Leaflet.setWMS .leaflet (\m l -> { m | leaflet = l }) LeafletMsg


view : Index -> String -> Model -> Html Msg
view index title model =
    let
        layerNames =
            model.mapInfo
                |> Maybe.map (\{ layers } -> layers)
                |> Maybe.withDefault []

        checkmark x =
            if x then
                Icon.view "check" [ Options.css "width" "40px" ]
            else
                Options.span [ Options.css "width" "40px" ] []

        menuItem layer layerName =
            Menu.item [ Menu.onSelect (SetLayer layer) ]
                [ checkmark (layer == model.mapLayer)
                , Html.text layerName
                ]

        leafletDiv =
            Options.div
                [ Options.id <| Leaflet.getId <| model.leaflet
                , Options.cs "leaflet-map"
                , Options.css "width" "800px"
                , Options.css "height" "600px"
                , Options.css "margin-left" "auto"
                , Options.css "margin-right" "auto"
                ]
                []
    in
        Card.view
            [ Elevation.e2
            , Options.css "width" "800px"
            , Options.css "margin" "20px"
            ]
            [ Card.title [ Card.border ]
                [ Card.head [] [ Html.text title ]
                ]
            , Card.menu [ Options.cs "map-layers-menu" ]
                [ Menu.render Mdl
                    (-1 :: index)
                    model.mdl
                    [ Menu.bottomRight ]
                    (List.indexedMap menuItem layerNames)
                ]
            , Card.text [ Options.css "padding" "0", Options.css "width" "100%" ] [ leafletDiv ]
            ]


init : String -> Model
init mapContainerId =
    { mapInfo = Nothing
    , mapLayer = 0
    , leaflet = Leaflet.initModel mapContainerId
    , mdl = Material.model
    }


subscriptions : (Msg -> msg) -> Sub msg
subscriptions liftMsg =
    Leaflet.subscriptions (LeafletMsg >> liftMsg)
