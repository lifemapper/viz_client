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


module OccurrenceSetTaxonList exposing (Msg, view, update)

import Material.Options as Options
import Material.Typography as Typo
import Html exposing (Html)
import Html.Attributes as Attributes


type alias Model =
    List String


type Msg
    = UpdateTaxonList String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTaxonList text ->
            let
                names =
                    text |> String.split "\n" |> List.filter (not << String.isEmpty)
            in
                ( names, Cmd.none )


view : Model -> Html Msg
view model =
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
            [ model |> String.join "\n" |> Html.text ]
        ]
