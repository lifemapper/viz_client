module DecodeTree exposing (..)

import Json.Decode exposing (Decoder, int, string, float, maybe, list, lazy, field, succeed, fail)


type alias TreeData =
    { cladeId : Int
    , length : Maybe Float
    , name : String
    , squid : Maybe String
    }


type alias TreeRecord =
    { data : TreeData
    , children : List Tree
    }


type Tree
    = Node TreeData Tree Tree
    | Leaf TreeData


removeUnderscore : Decoder (Maybe String) -> Decoder String
removeUnderscore =
    Json.Decode.map
        (Maybe.withDefault ""
            >> String.map
                (\c ->
                    if c == '_' then
                        ' '
                    else
                        c
                )
        )


treeDecoder : Decoder Tree
treeDecoder =
    Json.Decode.map2 TreeRecord
        (Json.Decode.map4 TreeData
            (field "cladeId" int)
            (maybe <| field "length" float)
            (removeUnderscore <| maybe <| field "name" string)
            (maybe <| field "squid" string)
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
