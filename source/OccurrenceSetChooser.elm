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


module OccurrenceSetChooser exposing (Model, Msg(Select), update, view, init, isPublicData)

import String.Extra as String
import List.Extra as List
import Ternary exposing ((?))
import Decoder exposing (AtomObjectRecord, AtomList(..), decodeAtomList, AtomObject(..))
import ProgramFlags exposing (Flags)
import Json.Decode as Decode
import Helpers exposing (Index)
import Http
import Html exposing (Html)
import Material
import Material.Color as Color
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.List as L
import QueryString as Q
import Dom
import Task


type SearchState
    = WaitingForInput
    | Searching
    | GotResults (List AtomObjectRecord)


type alias Model =
    { searchText : String
    , searchPublicData : Bool
    , searchState : SearchState
    , highlight : Maybe Int
    , mdl : Material.Model
    , programFlags : Flags
    }


isPublicData : Model -> Bool
isPublicData model =
    model.searchPublicData


type Msg
    = Mdl (Material.Msg Msg)
    | UpdateSearchText String
    | SearchPublicData
    | GotOccurrenceSets (Result Http.Error AtomList)
    | Select AtomObjectRecord
    | HighlightUp
    | HighlightDown
    | Nop


updateSearchParameters : Model -> ( Model, Cmd Msg )
updateSearchParameters model =
    let
        searchState =
            if model.searchText == "" then
                WaitingForInput
            else
                Searching

        cmd =
            if model.searchText == "" then
                Cmd.none
            else
                getOccurrenceSets model.programFlags model.searchPublicData model.searchText
    in
        ( { model | searchState = searchState, highlight = Nothing }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SearchPublicData ->
            updateSearchParameters { model | searchPublicData = not model.searchPublicData }

        UpdateSearchText text ->
            updateSearchParameters { model | searchText = String.toSentenceCase text }

        GotOccurrenceSets (Ok (AtomList atoms)) ->
            let
                results =
                    List.map (\(AtomObject o) -> o) atoms
            in
                ( { model | searchState = GotResults results }, Cmd.none )

        GotOccurrenceSets (Err err) ->
            Debug.log (toString err) ( model, Cmd.none )

        Select object ->
            let
                ( model_, cmd ) =
                    updateSearchParameters { model | searchText = "" }
            in
                model_ ! [ cmd, Dom.focus "occurrence-set-search-input" |> Task.attempt (always Nop) ]

        HighlightUp ->
            ( { model | highlight = highlightUp model }, Cmd.none )

        HighlightDown ->
            ( { model | highlight = highlightDown model }, Cmd.none )

        Nop ->
            ( model, Cmd.none )


highlightDown : Model -> Maybe Int
highlightDown model =
    case model.searchState of
        GotResults searchResults ->
            model.highlight
                |> Maybe.map ((+) 1)
                |> Maybe.map (min <| (List.length searchResults) - 1)
                |> Maybe.withDefault 0
                |> Just

        _ ->
            Nothing


highlightUp : Model -> Maybe Int
highlightUp model =
    case model.searchState of
        GotResults searchResults ->
            model.highlight
                |> Maybe.map (flip (-) 1)
                |> Maybe.andThen (\n -> (n < 0) ? Nothing <| (Just n))

        _ ->
            Nothing


getOccurrenceSets : Flags -> Bool -> String -> Cmd Msg
getOccurrenceSets flags publicData searchText =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = searchUrl flags publicData searchText
        , body = Http.emptyBody
        , expect = Http.expectJson decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send GotOccurrenceSets


searchUrl : Flags -> Bool -> String -> String
searchUrl { apiRoot, minimumOccurrencePoints } publicData searchText =
    let
        query_ =
            Q.empty
                |> Q.add "limit" "10"
                |> Q.add "status" "300"
                |> Q.add "minimumNumberOfPoints" (toString minimumOccurrencePoints)
                |> Q.add "displayName" searchText

        query =
            if publicData then
                query_ |> Q.add "user" "public"
            else
                query_
    in
        apiRoot ++ "occurrence" ++ (Q.render query)


viewSearchResultItem : Maybe Int -> Int -> AtomObjectRecord -> Html Msg
viewSearchResultItem highlighted i object =
    L.li []
        [ L.content
            [ Options.onClick <| Select object
            , Color.text Color.accent |> Options.when (Just i == highlighted)
            ]
            [ Html.text object.name ]
        ]


viewSearchResults : Model -> Html Msg
viewSearchResults model =
    case model.searchState of
        GotResults [] ->
            Options.styled Html.p [] [ Html.text "No matches" ]

        GotResults results ->
            L.ul [] <| List.indexedMap (viewSearchResultItem model.highlight) results

        _ ->
            L.ul [] []


onKeyUp : (String -> Msg) -> Options.Property c Msg
onKeyUp msg =
    Options.on "keyup" <| Decode.map msg (Decode.at [ "key" ] Decode.string)


getHighlightedResult : Model -> Maybe AtomObjectRecord
getHighlightedResult model =
    case model.searchState of
        GotResults results ->
            model.highlight |> Maybe.andThen (flip List.getAt results)

        _ ->
            Nothing


keyUp : Model -> String -> Msg
keyUp model s =
    case s of
        "ArrowUp" ->
            HighlightUp

        "ArrowDown" ->
            HighlightDown

        "Enter" ->
            getHighlightedResult model |> Maybe.map Select |> Maybe.withDefault Nop

        _ ->
            Nop


view : Index -> Model -> Html Msg
view index model =
    Options.div []
        [ Textfield.render Mdl
            (0 :: index)
            model.mdl
            [ Textfield.label "Search"
            , Textfield.value model.searchText
            , Options.id "occurrence-set-search-input"
            , Options.onInput UpdateSearchText
            , onKeyUp (keyUp model)
            ]
            []
        , Toggles.switch Mdl
            (1 :: index)
            model.mdl
            [ Toggles.value model.searchPublicData
            , Options.onToggle SearchPublicData
            ]
            [ Html.text "Search public data" ]
        , viewSearchResults model
        ]


init : Flags -> Model
init flags =
    { searchText = ""
    , searchPublicData = True
    , searchState = WaitingForInput
    , highlight = Nothing
    , mdl = Material.model
    , programFlags = flags
    }
