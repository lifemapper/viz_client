module MapCard exposing (Model, update, Msg(SetMap), view, init)

import Maybe.Extra as Maybe
import List.Extra as List
import Material
import Material.Options as Options
import Material.Card as Card
import Material.Menu as Menu
import Material.Elevation as Elevation
import Material.Icon as Icon
import Html exposing (Html)
import Helpers exposing (Index)
import Leaflet exposing (WMSInfo)


type alias Model =
    { mapInfo : Maybe WMSInfo
    , mapLayer : Int
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | SetLayer Int
    | SetMap (Maybe WMSInfo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SetMap mapInfo ->
            ( { model | mapInfo = mapInfo, mapLayer = 0 }, Cmd.none )

        SetLayer layer ->
            ( { model | mapLayer = layer }, Cmd.none )


selectLayers : Int -> WMSInfo -> WMSInfo
selectLayers i mapInfo =
    { mapInfo
        | layers =
            "bmng" :: (mapInfo.layers |> List.getAt i |> Maybe.toList)
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
            model.mapInfo |> Maybe.map (selectLayers model.mapLayer) |> Leaflet.view
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


init : Maybe WMSInfo -> Model
init wmsInfo =
    { mapInfo = wmsInfo
    , mapLayer = 0
    , mdl = Material.model
    }
