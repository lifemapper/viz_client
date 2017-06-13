module ScenariosList exposing (..)

import Http
import Decoder
    exposing
        ( AtomObjectRecord
        , AtomList(..)
        , decodeAtomList
        , AtomObject(..)
        , decodeScenario
        , Scenario(..)
        , ScenarioRecord
        )


type alias Model =
    List AtomObjectRecord


type Msg
    = GotScenarioList (Result Http.Error AtomList)

init : Model
init = []


getScenarios : (Msg -> msg) -> Cmd msg
getScenarios msgMap =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://notyeti-191.lifemapper.org/api/v2/scenario"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send GotScenarioList
        |> Cmd.map msgMap


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotScenarioList (Ok (AtomList atoms)) ->
            ( atoms |> List.map (\(AtomObject o) -> o), Cmd.none )

        GotScenarioList (Err err) ->
            Debug.log (toString err) ( model, Cmd.none )
