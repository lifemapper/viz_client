{-
   Copyright (C) 2018, University of Kansas Center for Research

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


module ScenariosList
    exposing
        ( Model
        , Msg
        , getPackages
        , init
        , update
        )

import List.Extra as List
import Http
import ProgramFlags exposing (Flags)
import Decoder
    exposing
        ( AtomObjectRecord
        , AtomList(..)
        , decodeAtomList
        , AtomObject(..)
        , decodeScenario
        , Scenario(..)
        , ScenarioRecord
        , decodeScenarioPackage
        , ScenarioPackage(..)
        , ScenarioPackageRecord
        )


type alias Model =
    { packageList : List AtomObjectRecord
    , packages : List ScenarioPackageRecord
    , programFlags : Flags
    }


type Msg
    = GotPackageList (List AtomObjectRecord)
    | GotPackage ScenarioPackageRecord
    | Nop


init : Flags -> Model
init flags =
    { packageList = []
    , packages = []
    , programFlags = flags
    }


getPackages : Flags -> (Msg -> msg) -> Cmd msg
getPackages { apiRoot } msgMap =
    Cmd.batch
        [ Http.request
            { method = "GET"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = apiRoot ++ "scenpackage"
            , body = Http.emptyBody
            , expect = Http.expectJson decodeAtomList
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.send gotPackageList
            |> Cmd.map msgMap
        , Http.request
            { method = "GET"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = apiRoot ++ "scenpackage?user=public"
            , body = Http.emptyBody
            , expect = Http.expectJson decodeAtomList
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.send gotPackageList
            |> Cmd.map msgMap
        ]


gotPackageList : Result Http.Error AtomList -> Msg
gotPackageList result =
    case result of
        Ok (AtomList atoms) ->
            atoms |> List.map (\(AtomObject o) -> o) |> List.uniqueBy .id |> GotPackageList

        Err err ->
            Debug.log "Loading scenario package list failed" err
                |> always Nop


getPackage : Flags -> Int -> Cmd Msg
getPackage { apiRoot } id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "scenpackage/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson decodeScenarioPackage
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotPackage


gotPackage : Result Http.Error ScenarioPackage -> Msg
gotPackage result =
    case result of
        Ok (ScenarioPackage p) ->
            GotPackage p

        Err err ->
            Debug.log "Loading scenario package failed" err
                |> always Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPackageList atoms ->
            ( { model | packageList = List.uniqueBy .id (model.packageList ++ atoms) }
            , atoms |> List.map (.id >> getPackage model.programFlags) |> Cmd.batch
            )

        GotPackage p ->
            ( { model | packages = List.uniqueBy .id (p :: model.packages) }, Cmd.none )

        Nop ->
            ( model, Cmd.none )
