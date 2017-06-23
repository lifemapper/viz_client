module MapCard exposing (Model, update, Msg, view, init, setMap, MapInfo)

import Material
import Material.Options as Options
import Material.Card as Card
import Material.Menu as Menu
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Helpers exposing (lift)
import Html exposing (Html)
import Helpers exposing (Index)
import Leaflet exposing (setLeafletMap, clearLeafletMap)


type alias MapInfo =
    { endPoint : String
    , mapName : String
    , layers : List String
    }


type alias Model =
    { mapInfo : Maybe MapInfo
    , mapLayer : Int
    , mapContainerId : String
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
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

        SetMap mapInfo ->
            let
                newModel =
                    { model | mapInfo = mapInfo, mapLayer = 0 }
            in
                ( newModel, updateMap model newModel )

        SetLayer layer ->
            let
                newModel =
                    { model | mapLayer = layer }
            in
                ( newModel, updateMap model newModel )


updateMap : Model -> Model -> Cmd Msg
updateMap oldModel model =
    if model.mapInfo == oldModel.mapInfo then
        Cmd.none
    else
        case model.mapInfo of
            Nothing ->
                clearLeafletMap model.mapContainerId

            Just { endPoint, mapName, layers } ->
                setLeafletMap
                    { containerId = model.mapContainerId
                    , endPoint = endPoint
                    , mapName = mapName
                    , layers = ("bmng" :: (List.take 1 <| List.drop model.mapLayer layers))
                    }


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
                [ Options.id model.mapContainerId
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
    , mapContainerId = mapContainerId
    , mdl = Material.model
    }
