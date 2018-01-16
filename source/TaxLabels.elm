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


module TaxLabels exposing (TaxLabel, taxLabels)

import Combine exposing (..)


type alias Name =
    String


type alias Squid =
    String


type alias TaxLabel =
    { name : Name
    , squid : Maybe Squid
    }


taxLabels : Parser s (List TaxLabel)
taxLabels =
    string "TAXLABELS"
        *> whitespace1
        *> sepBy whitespace1 taxLabel
        <* whitespace
        <* string ";"


taxLabel : Parser s TaxLabel
taxLabel =
    name |> andThen (\name -> (maybe (whitespace *> squid)) |> map (\squid -> { name = name, squid = squid }))


name : Parser s Name
name =
    choice
        [ regex "[^']+" |> between (string "'") (string "'")
        , regex "[_a-zA-Z0-9']+"
        ]


squid : Parser s Squid
squid =
    string "[&squid="
        *> regex "[0-9a-fA-F]*"
        <* string "]"
