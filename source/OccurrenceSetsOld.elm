module OccurrenceSets exposing (..)

import Debug
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import List
import Maybe exposing (withDefault)

import Decoder exposing
    ( OccurrenceSet
    , decodeOccurrenceSet
    , OccurrenceSetFeature
    , OccurrenceSetFeatureItem
    )

main : Program Never Model Msg
main =
    Html.program { view = view, update = update, init = init, subscriptions = subscriptions }


type alias Model =
    { occurrenceSets : List OccurrenceSet
    }


init : ( Model, Cmd Msg )
init =
    ( { occurrenceSets = [] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = GetOccurrenceSets
    | GotOccurrenceSets (Result Http.Error (List OccurrenceSet))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetOccurrenceSets ->
            ( model, getOccurrenceSets )

        GotOccurrenceSets (Ok occurrenceSets) ->
            ( { model | occurrenceSets = occurrenceSets }, Cmd.none )

        GotOccurrenceSets (Err err) ->
            ( model, Debug.log (toString err) Cmd.none )


getOccurrenceSets : Cmd Msg
getOccurrenceSets =
        Http.send GotOccurrenceSets
            <| Http.request
                { method = "GET"
                , headers = [Http.header "Accept" "application/json"]
                , url = "http://yeti.lifemapper.org/services/sdm/occurrences"
                , body = Http.emptyBody
                , expect = Http.expectJson decodeOccurrenceSets
                , timeout = Nothing
                , withCredentials = False
                }


decodeOccurrenceSets : Decode.Decoder (List OccurrenceSet)
decodeOccurrenceSets =
    Decode.field "items" (Decode.list decodeOccurrenceSet)


-- decodeOccurrenceSet : Decode.Decoder OccurrenceSet
-- decodeOccurrenceSet =
--     Decode.map5 OccurrenceSet
--         (Decode.field "epsgcode" decodeStringedInt)
--         (Decode.field "id" decodeStringedInt)
--         (Decode.field "modTime" Decode.string)
--         (Decode.field "title" Decode.string)
--         (Decode.field "url" Decode.string)


-- decodeStringedInt : Decode.Decoder Int
-- decodeStringedInt =
--     Decode.string |> Decode.andThen helpStringedInt


-- helpStringedInt : String -> Decode.Decoder Int
-- helpStringedInt value =
--     case String.toInt value of
--         Ok int ->
--             Decode.succeed int

--         Err err ->
--             Decode.fail err


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Occurrence Sets" ]
        , ul [] (List.map viewOccurrenceSet model.occurrenceSets)
        , button [ onClick GetOccurrenceSets ] [ text "Get Occurrence Sets" ]
        ]


viewOccurrenceSet : OccurrenceSet -> Html Msg
viewOccurrenceSet (Decoder.OccurrenceSet r) =
    li [] [ text (withDefault "" r.title) ]
