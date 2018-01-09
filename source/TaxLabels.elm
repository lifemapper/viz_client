module TaxLabels exposing (..)

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
    regex "[_a-zA-Z0-9']+"


squid : Parser s Squid
squid =
    string "[&squid="
        *> regex "[0-9a-fA-F]*"
        <* string "]"
