module DecodeTree exposing (..)

import Json.Decode exposing (Decoder, int, string, float, maybe, list, lazy, field, succeed, fail)


type alias TreeRecord =
    { pathId : Maybe Int
    , length : Maybe Float
    , name : Maybe String
    , mx : Maybe Int
    , children : List Tree
    }


type Tree
    = Tree TreeRecord


treeDecoder : Decoder Tree
treeDecoder =
    (Json.Decode.map5 TreeRecord
        (maybe <| field "pathId" int)
        (maybe <| field "length" float)
        (maybe <| field "name" string)
        (maybe <| field "mx" int)
        (field "children" (lazy (\_ -> (list treeDecoder))))
    )
        |> Json.Decode.andThen checkBinary


checkBinary : TreeRecord -> Decoder Tree
checkBinary treeRecord =
    case treeRecord.children of
        [] ->
            succeed (Tree treeRecord)

        [ _ ] ->
            fail "found node with single child."

        [ _, _ ] ->
            succeed (Tree treeRecord)

        _ ->
            fail "found node with more than two children."
