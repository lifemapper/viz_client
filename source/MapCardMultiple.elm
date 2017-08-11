module MapCardMultiple exposing (NamedMap, Model, update, Msg(SetAvailable), view, init)

import Maybe.Extra as Maybe
import List.Extra as List
import Material
import Material.Options as Options
import Material.Card as Card
import Material.Elevation as Elevation
import Material.List as L
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
    , expanded : Bool
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | SetShowing NamedMap
    | SetNotShowing NamedMap
    | SetAvailable (List NamedMap)
    | SetExpanded Bool


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

        SetExpanded expanded ->
            ( { model | expanded = expanded }, Cmd.none )


selectLayers : Bool -> Int -> WMSInfo -> WMSInfo
selectLayers includeBMNG i mapInfo =
    { mapInfo
        | layers =
            if includeBMNG then
                "bmng" :: (mapInfo.layers |> List.getAt i |> Maybe.toList)
            else
                (mapInfo.layers |> List.getAt i |> Maybe.toList)
    }


layerLi : Model -> Index -> Int -> NamedMap -> Html Msg
layerLi model index i map =
    let
        ( iconName, onClick ) =
            if List.member map model.showing then
                ( "check_box", SetNotShowing map )
            else
                ( "check_box_outline_blank", SetShowing map )

        icon =
            L.icon iconName [ Options.onClick onClick ]
    in
        L.li [] [ L.content [] [ icon, Html.text map.name ] ]


layersList : Index -> Model -> Html Msg
layersList index model =
    L.ul [] <| List.indexedMap (layerLi model index) <| List.reverse model.available


view : Index -> String -> Model -> Html Msg
view index title model =
    let
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
            [ Card.title [ Options.onClick <| SetExpanded (not model.expanded) ]
                [ Card.head [] [ Html.text title ] ]
            , Card.text [ Options.css "display" "none" |> Options.when (not model.expanded) ]
                [ layersList index model ]
            , Card.text [ Options.css "padding" "0", Options.css "width" "100%" ] [ leafletDiv ]
            ]


init : List NamedMap -> Model
init available =
    { available = available
    , showing = available
    , expanded = False
    , mdl = Material.model
    }
