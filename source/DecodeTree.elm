module DecodeTree exposing (..)

import Json.Decode exposing (Decoder, int, string, float, maybe, list, lazy, field, succeed, fail)


type alias TreeData =
    { pathId : Maybe Int
    , length : Maybe Float
    , name : Maybe String
    , mx : Maybe Int
    }


type alias TreeRecord =
    { data : TreeData
    , children : List Tree
    }


type Tree
    = Node TreeData Tree Tree
    | Leaf TreeData


treeDecoder : Decoder Tree
treeDecoder =
    Json.Decode.map2 TreeRecord
        (Json.Decode.map4 TreeData
            (maybe <| field "pathId" int)
            (maybe <| field "length" float)
            (maybe <| field "name" string)
            (maybe <| field "mx" int)
        )
        (field "children" (lazy (\_ -> (list treeDecoder))))
        |> Json.Decode.andThen checkBinary


checkBinary : TreeRecord -> Decoder Tree
checkBinary { data, children } =
    case children of
        [] ->
            succeed (Leaf data)

        [ _ ] ->
            fail "found node with single child."

        [ left, right ] ->
            succeed (Node data left right)

        _ ->
            fail "found node with more than two children."
