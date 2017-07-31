module SDMResults exposing (Model, init, update, page, Msg(LoadProjections))

import Html exposing (Html)
import Http
import Decoder
import ProgramFlags exposing (Flags)
import Page exposing (Page)


type alias Model =
    { programFlags : Flags
    , projectionsToLoad : Maybe Int
    , projections : List Decoder.ProjectionRecord
    }


init : Flags -> Model
init flags =
    { programFlags = flags
    , projectionsToLoad = Nothing
    , projections = []
    }


type Msg
    = LoadProjections Int
    | GotProjectionAtoms (List Decoder.AtomObjectRecord)
    | GotProjection Decoder.ProjectionRecord
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        LoadProjections gridsetId ->
            ( model, loadProjections model.programFlags gridsetId )

        GotProjectionAtoms atoms ->
            ( { model | projectionsToLoad = Just (List.length atoms) }
            , atoms |> List.map (loadMetadata model.programFlags) |> Cmd.batch
            )

        GotProjection p ->
            ( { model | projections = p :: model.projections }, Cmd.none )


loadProjections : Flags -> Int -> Cmd Msg
loadProjections { apiRoot } id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "sdmProject?user=anon&gridsetid=" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeAtomList
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotProjectionAtoms


gotProjectionAtoms : Result Http.Error Decoder.AtomList -> Msg
gotProjectionAtoms result =
    case result of
        Ok (Decoder.AtomList atoms) ->
            atoms |> List.map (\(Decoder.AtomObject o) -> o) |> GotProjectionAtoms

        Err err ->
            Debug.log "Error fetching projections" (toString err) |> always Nop


loadMetadata : Flags -> Decoder.AtomObjectRecord -> Cmd Msg
loadMetadata { apiRoot } { id } =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiRoot ++ "sdmProject/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.decodeProjection
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotMetadata


gotMetadata : Result Http.Error Decoder.Projection -> Msg
gotMetadata result =
    case result of
        Ok (Decoder.Projection p) ->
            GotProjection p

        Err err ->
            Debug.log "Failed to load projection" err |> always Nop


view : Model -> Html Msg
view model =
    Html.div [] []


page : Page Model Msg
page =
    { view = view
    , selectedTab = always 0
    , selectTab = always Nop
    , tabTitles = always []
    }
