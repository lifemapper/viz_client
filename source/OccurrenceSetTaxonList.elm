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


module OccurrenceSetTaxonList exposing (Model, getTaxonIds, init, Msg, view, update)

import Http
import List.Extra as List
import Maybe.Extra exposing ((?))
import Material.Options as Options
import Material.Typography as Typo
import Html exposing (Html)
import Html.Attributes as Attributes
import ProgramFlags exposing (Flags)
import Decoder
import Encoder


type alias Model =
    { state : State
    , programFlags : Flags
    }


type State
    = GetList (List String)
    | RequestingMatches
    | MatchingFailed
    | GotMatches (List Match)


type alias Match =
    { response : Decoder.GbifResponseItemRecord
    , searchName : String
    , use : Bool
    }


getTaxonIds : Model -> List Int
getTaxonIds model =
    case model.state of
        GotMatches matches ->
            matches
                |> List.filter .use
                |> List.filterMap (.response >> .taxon_id)

        _ ->
            []


init : Flags -> Model
init flags =
    { state = GetList []
    , programFlags = flags
    }


type Msg
    = UpdateTaxonList String
    | CheckTaxa
    | GotMatchesMsg (List Match)
    | MatchingFailedMsg
    | UpdateSearchName Int String
    | ToggleUseName Int
    | MatchAgain
    | StartOver


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTaxonList text ->
            case model.state of
                GetList names ->
                    let
                        names_ =
                            text |> String.split "\n" |> List.filter (not << String.isEmpty) |> List.unique
                    in
                        ( { model | state = GetList names_ }, Cmd.none )

                _ ->
                    model ! []

        CheckTaxa ->
            case model.state of
                GetList names ->
                    { model | state = RequestingMatches } ! [ checkTaxa model.programFlags [] names ]

                _ ->
                    model ! []

        GotMatchesMsg matches ->
            case model.state of
                RequestingMatches ->
                    { model | state = GotMatches matches } ! []

                _ ->
                    model ! []

        MatchingFailedMsg ->
            case model.state of
                RequestingMatches ->
                    { model | state = MatchingFailed } ! []

                _ ->
                    model ! []

        UpdateSearchName i name ->
            case model.state of
                GotMatches matches ->
                    let
                        matches_ =
                            matches
                                |> List.indexedMap
                                    (\j match ->
                                        if j == i then
                                            { match | searchName = name }
                                        else
                                            match
                                    )
                    in
                        { model | state = GotMatches matches_ } ! []

                _ ->
                    model ! []

        ToggleUseName i ->
            case model.state of
                GotMatches matches ->
                    let
                        matches_ =
                            matches
                                |> List.indexedMap
                                    (\j match ->
                                        if j == i then
                                            { match | use = not match.use }
                                        else
                                            match
                                    )
                    in
                        { model | state = GotMatches matches_ } ! []

                _ ->
                    model ! []

        MatchAgain ->
            case model.state of
                GotMatches matches ->
                    let
                        names =
                            matches
                                |> List.map (.searchName >> String.trim)
                                |> List.unique
                                |> List.filter (not << String.isEmpty)

                        dontUse =
                            matches
                                |> List.filterMap
                                    (\match ->
                                        if not match.use then
                                            match.response.accepted_name
                                        else
                                            Nothing
                                    )
                    in
                        { model | state = RequestingMatches } ! [ checkTaxa model.programFlags dontUse names ]

                _ ->
                    model ! []

        StartOver ->
            { model | state = GetList [] } ! []


checkTaxa : Flags -> List String -> List String -> Cmd Msg
checkTaxa flags dontUse names =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = flags.apiRoot ++ "gbifparser"
        , body = Http.jsonBody <| Encoder.encodeGbifPost <| Decoder.GbifPost names
        , expect = Http.expectJson Decoder.decodeGbifResponse
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (gotGbifResponse dontUse)


gotGbifResponse : List String -> Result Http.Error Decoder.GbifResponse -> Msg
gotGbifResponse dontUse response =
    case response of
        Ok (Decoder.GbifResponse items) ->
            items
                |> List.map
                    (\(Decoder.GbifResponseItem item) ->
                        { response = item
                        , searchName = item.search_name ? ""
                        , use =
                            case item.accepted_name of
                                Just name ->
                                    (not <| List.member name dontUse) && (Just name) == item.search_name

                                Nothing ->
                                    False
                        }
                    )
                |> List.sortBy .searchName
                |> GotMatchesMsg

        Err err ->
            MatchingFailedMsg


view : Model -> Html Msg
view model =
    case model.state of
        GetList names ->
            Options.div []
                [ Options.styled Html.p [ Typo.title ] [ Html.text "Provide List of Species Names" ]
                , Options.styled Html.textarea
                    [ Options.attribute <|
                        Attributes.placeholder
                            ("Paste species names here, one per line. \n\n"
                                ++ "The names will be matched against the GBIF tree and \n"
                                ++ "the corresponding iDigBio occurrence points downloaded."
                            )
                    , Options.attribute <| Attributes.rows 20
                    , Options.attribute <| Attributes.cols 80
                    , Options.attribute <| Attributes.autocomplete False
                    , Options.attribute <| Attributes.spellcheck False
                    , Options.onInput UpdateTaxonList
                    ]
                    [ names |> String.join "\n" |> Html.text ]
                , Html.p []
                    [ Options.styled Html.button
                        [ Options.onClick CheckTaxa ]
                        [ Html.text "Match" ]
                    ]
                ]

        RequestingMatches ->
            Options.div []
                [ Options.styled Html.p [ Typo.title ] [ Html.text "Matching species names against GBIF..." ] ]

        GotMatches matches ->
            Options.div []
                [ Options.styled Html.p [ Typo.title ] [ Html.text "GBIF species names match results:" ]
                , matches
                    |> List.indexedMap viewMatch
                    |> (++)
                        [ Html.tr []
                            [ Html.th [] [ Html.text "Searched" ]
                            , Html.th [] [ Html.text "Matched" ]
                            , Html.th [] [ Html.text "Use" ]
                            ]
                        ]
                    |> Html.table []
                , Options.styled Html.button
                    [ Options.onClick MatchAgain
                    , Options.attribute <|
                        Attributes.disabled
                            (matches |> List.all (\match -> Just match.searchName == match.response.search_name))
                    ]
                    [ Html.text "Match again" ]
                , Options.styled Html.button [ Options.onClick StartOver ] [ Html.text "Start over" ]
                ]

        MatchingFailed ->
            Options.div []
                [ Options.styled Html.p
                    [ Typo.title ]
                    [ Html.text "There was a problem accessing the GBIF name matching API!" ]
                ]


viewMatch : Int -> Match -> Html Msg
viewMatch i item =
    let
        checkbox =
            Options.styled Html.input
                [ Options.attribute <| Attributes.type_ "checkbox"
                , Options.attribute <| Attributes.checked item.use
                , Options.onClick (ToggleUseName i)
                ]
                []

        nameUpdater =
            Options.styled Html.input
                [ Options.attribute <| Attributes.value <| item.searchName
                , Options.onInput (UpdateSearchName i)
                ]
                []
    in
        case item.response.accepted_name of
            Just name ->
                if item.response.search_name == Just name then
                    Html.tr []
                        [ Html.td [] [ Html.text <| item.response.search_name ? "" ]
                        , Html.td [] [ Html.text name ]
                        , Html.td [] [ checkbox ]
                        ]
                else
                    Html.tr []
                        [ Html.td [] [ nameUpdater ]
                        , Html.td [] [ Html.text name ]
                        , Html.td [] [ checkbox ]
                        ]

            Nothing ->
                Html.tr []
                    [ Html.td [] [ nameUpdater ]
                    , Html.td [] [ Html.text "No match" ]
                    ]
