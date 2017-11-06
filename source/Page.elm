{-
Copyright (C) 2017, University of Kansas Center for Research

Lifemapper Project, lifemapper [at] ku [dot] edu,
Biodiversity Institute,
1345 Jayhawk Boulevard, Lawrence, Kansas, 66045, USA

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.
-}
module Page exposing (..)

import Html exposing (Html)


type alias Page model msg =
    { view : model -> Html msg
    , selectedTab : model -> Int
    , selectTab : Int -> msg
    , tabTitles : model -> List (Html msg)
    , subscriptions : model -> Sub msg
    , title : String
    }


lift : Page submodel submsg -> (model -> submodel) -> (submsg -> msg) -> Page model msg
lift subpage model2Submodel submsg2Msg =
    { view = model2Submodel >> subpage.view >> (Html.map submsg2Msg)
    , selectedTab = model2Submodel >> subpage.selectedTab
    , selectTab = subpage.selectTab >> submsg2Msg
    , tabTitles = model2Submodel >> subpage.tabTitles >> List.map (Html.map submsg2Msg)
    , subscriptions = model2Submodel >> subpage.subscriptions >> Sub.map submsg2Msg
    , title = subpage.title
    }
