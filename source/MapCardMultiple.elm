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
    , showing : List NamedMap
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | SetShowing NamedMap
    | SetNotShowing NamedMap
    | SetAvailable (List NamedMap)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SetAvailable available ->
            ( { model | available = available, showing = available }, Cmd.none )

        SetShowing show ->
            ( { model | showing = show :: model.showing }, Cmd.none )

        SetNotShowing notShow ->
            ( { model | showing = List.remove notShow model.showing }, Cmd.none )


selectLayers : Bool -> Int -> WMSInfo -> WMSInfo
selectLayers includeBMNG i mapInfo =
    { mapInfo
        | layers =
            if includeBMNG then
                "bmng" :: (mapInfo.layers |> List.getAt i |> Maybe.toList)
            else
                (mapInfo.layers |> List.getAt i |> Maybe.toList)
    }


view : Index -> String -> Model -> Html Msg
view index title model =
    let
        checkmark x =
            if x then
                Icon.view "check" [ Options.css "width" "40px" ]
            else
                Options.span [ Options.css "width" "40px" ] []

        currentlyShowing namedMap =
            (List.member namedMap model.showing)

        onSelect namedMap =
            if currentlyShowing namedMap then
                SetNotShowing namedMap
            else
                SetShowing namedMap

        menuItem namedMap =
            Menu.item [ Menu.onSelect (onSelect namedMap) ]
                [ checkmark (currentlyShowing namedMap)
                , Html.text namedMap.name
                ]

        leafletDiv =
            model.available
                |> List.filter (\m -> List.member m model.showing)
                |> List.indexedMap (\i m -> m.wmsInfo |> selectLayers (i == 0) 0)
                |> Leaflet.view
    in
        Card.view
            [ Elevation.e2
            , Options.css "width" "100%"
            ]
            [ Card.title [ Card.border ]
                [ Card.head [] [ Html.text title ] ]
            , Card.menu [ Options.cs "map-layers-menu" ]
                [ Menu.render Mdl
                    (-1 :: index)
                    model.mdl
                    [ Menu.bottomRight ]
                    (model.available |> List.map menuItem)
                ]
            , Card.text []
                [ Html.text "Foobar"
                ]
            , Card.text [ Options.css "padding" "0", Options.css "width" "100%" ] [ leafletDiv ]
            ]


init : List NamedMap -> Model
init available =
    { available = available
    , showing = available
    , mdl = Material.model
    }
