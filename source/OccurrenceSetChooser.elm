module OccurrenceSetChooser exposing (..)

import Decoder exposing (AtomObjectRecord, AtomList(..), decodeAtomList, AtomObject(..))
import Json.Decode as Decode
import Char
import Helpers exposing (Index)
import Http
import Html exposing (Html)
import Material
import Material.Scheme
import Material.Color as Color
import Material.Options as Options
import Material.Textfield as Textfield
import Material.List as L
import QueryString as Q
import Dom
import Task


type alias Model =
    { searchText : String
    , searchResults : List AtomObjectRecord
    , highlight : Maybe Int
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | UpdateSearchText String
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

        UpdateSearchText text ->
            case String.uncons text of
                Just ( c, rest ) ->
                    let
                        text =
                            String.cons (Char.toUpper c) rest
                    in
                        ( { model | searchText = text }, getOccurrenceSets text )

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


getOccurrenceSets : String -> Cmd Msg
getOccurrenceSets searchText =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = searchUrl searchText
        , body = Http.emptyBody
        , expect = Http.expectJson decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send GotOccurrenceSets


searchUrl : String -> String
searchUrl searchText =
    "http://notyeti-191.lifemapper.org/api/v2/occurrence"
        ++ (Q.empty
                |> Q.add "limit" "10"
                |> Q.add "status" "300"
                |> Q.add "displayName" searchText
                |> Q.render
           )


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
        , viewSearchResults model
        ]


init : Model
init =
    { searchText = "", searchResults = [], highlight = Nothing, mdl = Material.model }


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Material.init Mdl )
        , view = view [] >> Material.Scheme.top
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
