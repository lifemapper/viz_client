module ScenariosList
    exposing
        ( Model
        , Msg
        , getScenarios
        , init
        , update
        , loading
        , observedScenarios
        , metadataToAtom
        )

import Http
import List.Extra exposing (find)
import Constants exposing (apiRoot)
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
    { atoms : List AtomObjectRecord
    , metadatas : List ScenarioRecord
    }


observedScenarios : Model -> List ScenarioRecord
observedScenarios { metadatas } =
    metadatas |> List.filter (.code >> Maybe.map (String.startsWith "observed") >> Maybe.withDefault False)


loading : Model -> ( Int, Int )
loading { atoms, metadatas } =
    ( List.length metadatas, List.length atoms )


metadataToAtom : Model -> ScenarioRecord -> Maybe AtomObjectRecord
metadataToAtom model metadata =
    model.atoms
        |> find (.id >> (==) metadata.id)


type Msg
    = GotScenarioList (Result Http.Error AtomList)
    | AddMetadata ScenarioRecord
    | Nop


init : Model
init =
    { atoms = []
    , metadatas = []
    }


getScenarios : (Msg -> msg) -> Cmd msg
getScenarios msgMap =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "scenario"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send GotScenarioList
        |> Cmd.map msgMap


getMetadata : Int -> Cmd Msg
getMetadata id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "scenario/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson decodeScenario
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotMetadata


gotMetadata : Result Http.Error Scenario -> Msg
gotMetadata result =
    case result of
        Ok (Scenario s) ->
            AddMetadata s

        Err err ->
            Nop |> Debug.log (toString err)


getMetadatas : List AtomObjectRecord -> Cmd Msg
getMetadatas atoms =
    atoms |> List.map (.id >> getMetadata) |> Cmd.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotScenarioList (Ok (AtomList atoms)) ->
            let
                atomRecords =
                    atoms |> List.map (\(AtomObject o) -> o)
            in
                ( { model | atoms = atomRecords }, getMetadatas atomRecords )

        GotScenarioList (Err err) ->
            Debug.log (toString err) ( model, Cmd.none )

        AddMetadata scenario ->
            ( { model | metadatas = scenario :: model.metadatas }, Cmd.none )

        Nop ->
            ( model, Cmd.none )
