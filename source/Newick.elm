module Newick exposing (..)

import Combine exposing (..)
import Combine.Num exposing (float, int)


type alias Name =
    String


unparseName : Name -> String
unparseName =
    identity


type alias Length =
    Maybe Float


unparseLength : Length -> String
unparseLength =
    Maybe.map (\l -> ":" ++ (toString l)) >> Maybe.withDefault ""


type Tree
    = SubTree SubTree
    | Branch Branch


unparse : Tree -> String
unparse tree =
    (case tree of
        SubTree st ->
            unparseSubTree st

        Branch b ->
            unparseBranch b
    )
        ++ ";"


type SubTree
    = Leaf Name
    | Branches (List Branch) Name


unparseSubTree : SubTree -> String
unparseSubTree st =
    case st of
        Leaf name ->
            unparseName name

        Branches bs name ->
            "(" ++ (bs |> List.map unparseBranch |> String.join ",") ++ ")" ++ (unparseName name)


type alias Branch =
    ( SubTree, Length )


unparseBranch : Branch -> String
unparseBranch ( st, l ) =
    (unparseSubTree st) ++ (unparseLength l)


tree : Parser s Tree
tree =
    choice
        [ ((lazy subTree) <* string ";") |> map SubTree
        , ((lazy branch) <* string ";") |> map Branch
        ]
        <* end


subTree : () -> Parser s SubTree
subTree () =
    choice
        [ lazy internal
        , leaf
        ]


leaf : Parser s SubTree
leaf =
    name |> map Leaf


name : Parser s Name
name =
    regex "[_a-zA-Z0-9']*"


internal : () -> Parser s SubTree
internal () =
    (branchSet |> lazy |> parens) |> andThen (\branches -> name |> map (Branches branches))


branchSet : () -> Parser s (List Branch)
branchSet () =
    sepBy1 (string ",") (lazy branch)


branch : () -> Parser s Branch
branch () =
    (lazy subTree) |> andThen (\st -> length |> map ((,) st))


length : Parser s Length
length =
    maybe (string ":" *> choice [ float, map toFloat int ])
