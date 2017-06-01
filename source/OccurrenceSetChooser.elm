module OccurrenceSetChooser exposing (..)

import Decoder exposing (AtomObjectRecord, AtomList(..), decodeAtomList, AtomObject(..))
import Char
import Helpers exposing (Index)
import Http
import Html exposing (Html)
import Material
import Material.Scheme
import Material.Options as Options
import Material.Textfield as Textfield
import Material.List as L
import QueryString as Q


type alias Model =
    { searchText : String
    , searchResults : List AtomObjectRecord
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | UpdateSearchText String
    | GotOccurrenceSets (Result Http.Error AtomList)


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


viewSearchResultItem : AtomObjectRecord -> Html Msg
viewSearchResultItem object =
    L.li [] [ L.content [] [ Html.text object.name ] ]


viewSearchResults : Model -> Html Msg
viewSearchResults model =
    if model.searchResults == [] && model.searchText /= "" then
        Options.styled Html.p [] [ Html.text "No matches" ]
    else
        L.ul [] <| List.map viewSearchResultItem model.searchResults


view : Index -> Model -> Html Msg
view index model =
    Options.div []
        [ Textfield.render Mdl
            (0 :: index)
            model.mdl
            [ Textfield.label "Search"
            , Textfield.value model.searchText
            , Options.onInput UpdateSearchText
            ]
            []
        , viewSearchResults model
        ]


init : Model
init =
    { searchText = "", searchResults = [], mdl = Material.model }


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Material.init Mdl )
        , view = view [] >> Material.Scheme.top
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
