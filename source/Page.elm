module Page exposing (..)

import Html exposing (Html)


type alias Page model msg =
    { view : model -> Html msg
    , selectedTab : model -> Int
    , selectTab : Int -> msg
    , tabTitles : model -> List (Html msg)
    , subscriptions : model -> Sub msg
    }


lift : Page submodel submsg -> (model -> submodel) -> (submsg -> msg) -> Page model msg
lift subpage model2Submodel submsg2Msg =
    { view = model2Submodel >> subpage.view >> (Html.map submsg2Msg)
    , selectedTab = model2Submodel >> subpage.selectedTab
    , selectTab = subpage.selectTab >> submsg2Msg
    , tabTitles = model2Submodel >> subpage.tabTitles >> List.map (Html.map submsg2Msg)
    , subscriptions = model2Submodel >> subpage.subscriptions >> Sub.map submsg2Msg
    }
