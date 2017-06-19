module ExistingSDM exposing (Model, update, page, init, Msg(LoadMetadata))

import Material
import Material.Options as Options
import Http
import Html exposing (Html)
import Page exposing (Page)
import Decoder exposing (ProjectionRecord, decodeProjection, Projection(..))


type State
    = Showing ProjectionRecord
    | Loading Int
    | Blank


type alias Model =
    { mdl : Material.Model
    , state : State
    }


init : Model
init =
    { mdl = Material.model, state = Blank }


type Msg
    = Mdl (Material.Msg Msg)
    | SetState State
    | LoadMetadata Int
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadMetadata id ->
            ( { model | state = Loading id }, loadMetadata id )

        SetState state ->
            ( { model | state = state }, Cmd.none )

        Nop ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


loadMetadata : Int -> Cmd Msg
loadMetadata id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://notyeti-191.lifemapper.org/api/v2/sdmProject/" ++ (toString id)
        , body = Http.emptyBody
        , expect = Http.expectJson decodeProjection
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send gotMetadata


gotMetadata : Result Http.Error Projection -> Msg
gotMetadata result =
    case result of
        Ok (Projection p) ->
            SetState (Showing p)

        Err err ->
            SetState Blank
                |> Debug.log (toString err)


view : Model -> Html Msg
view model =
    case model.state of
        Blank ->
            Options.div [] []

        Loading _ ->
            Options.div [] [ Html.text "Loading" ]

        Showing p ->
            Options.div [] [ Html.text (toString p) ]


page : Page Model Msg
page =
    { view = view
    , selectedTab = always 0
    , selectTab = always Nop
    , tabTitles = always [ Html.text "SDM Projection" ]
    }
