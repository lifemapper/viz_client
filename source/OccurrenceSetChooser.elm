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

import Decoder exposing (AtomObjectRecord, AtomList(..), decodeAtomList, AtomObject(..))
import ProgramFlags exposing (Flags)
import Json.Decode as Decode
import Char
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


type alias Model =
    { searchText : String
    , searchPublicData : Bool
    , searchResults : List AtomObjectRecord
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SearchPublicData ->
            let
                model_ =
                    { model | searchPublicData = not model.searchPublicData, searchResults = [], highlight = Nothing }
            in
                ( model_, getOccurrenceSets model_.programFlags model_.searchPublicData model_.searchText )

        UpdateSearchText text ->
            case String.uncons text of
                Just ( c, rest ) ->
                    let
                        text =
                            String.cons (Char.toUpper c) rest
                    in
                        ( { model | searchText = text }, getOccurrenceSets model.programFlags model.searchPublicData text )

                Nothing ->
                    ( { model | searchText = text }, Cmd.none )

        GotOccurrenceSets (Ok (AtomList atoms)) ->
            let
                results =
                    List.map (\(AtomObject o) -> o) atoms
            in
                ( { model | searchResults = results }, Cmd.none )

        GotOccurrenceSets (Err err) ->
            Debug.log (toString err) ( model, Cmd.none )

        Select object ->
            ( { model | searchText = "", searchResults = [], highlight = Nothing }
            , Dom.focus "occurrence-set-search-input" |> Task.attempt (always Nop)
            )

        HighlightUp ->
            ( { model | highlight = highlightUp model }, Cmd.none )

        HighlightDown ->
            ( { model | highlight = highlightDown model }, Cmd.none )

        Nop ->
            ( model, Cmd.none )


highlightDown : Model -> Maybe Int
highlightDown model =
    model.highlight
        |> Maybe.withDefault (-1)
        |> (+) 1
        |> (min <| (List.length model.searchResults) - 1)
        |> Just


highlightUp : Model -> Maybe Int
highlightUp model =
    model.highlight
        |> Maybe.andThen
            (\n ->
                if n == 0 then
                    Nothing
                else
                    Just (n - 1)
            )
        |> Maybe.map (min <| (List.length model.searchResults) - 1)


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
    if model.searchResults == [] && model.searchText /= "" then
        Options.styled Html.p [] [ Html.text "No matches" ]
    else
        L.ul [] <| List.indexedMap (viewSearchResultItem model.highlight) model.searchResults


onKeyUp : (String -> Msg) -> Options.Property c Msg
onKeyUp msg =
    Options.on "keyup" <| Decode.map msg (Decode.at [ "key" ] Decode.string)


keyUp : Model -> String -> Msg
keyUp model s =
    case s of
        "ArrowUp" ->
            HighlightUp

        "ArrowDown" ->
            HighlightDown

        "Enter" ->
            model.highlight
                |> Maybe.andThen (\i -> List.drop i model.searchResults |> List.head)
                |> Maybe.map Select
                |> Maybe.withDefault Nop

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
    , searchResults = []
    , highlight = Nothing
    , mdl = Material.model
    , programFlags = flags
    }
