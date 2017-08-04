module MapCardMultiple exposing (NamedMap, Model, update, Msg(SetAvailable), view, init)

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


type alias NamedMap =
    { name : String
    , wmsInfo : WMSInfo
    }


type alias Model =
    { available : List NamedMap
    , showing : Maybe NamedMap
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | SetShowing (Maybe NamedMap)
    | SetAvailable (List NamedMap)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SetAvailable available ->
            ( { model | available = available, showing = List.head available }, Cmd.none )

        SetShowing show ->
            ( { model | showing = show }, Cmd.none )


selectLayers : Int -> WMSInfo -> WMSInfo
selectLayers i mapInfo =
    { mapInfo
        | layers =
            "bmng" :: (mapInfo.layers |> List.getAt i |> Maybe.toList)
    }


view : Index -> String -> Model -> Html Msg
view index title model =
    let
        checkmark x =
            if x then
                Icon.view "check" [ Options.css "width" "40px" ]
            else
                Options.span [ Options.css "width" "40px" ] []

        menuItem namedMap =
            Menu.item [ Menu.onSelect (SetShowing <| Just namedMap) ]
                [ checkmark (Just namedMap == model.showing)
                , Html.text namedMap.name
                ]

        leafletDiv =
            model.showing |> Maybe.map .wmsInfo |> Maybe.map (selectLayers 0) |> Leaflet.view
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
                    (model.available |> List.map menuItem)
                ]
            , Card.text [ Options.css "padding" "0", Options.css "width" "100%" ] [ leafletDiv ]
            ]


init : List NamedMap -> Model
init available =
    { available = available
    , showing = List.head available
    , mdl = Material.model
    }
