port module Leaflet exposing (Model, Msg, update, initModel, subscriptions, getId, WMSInfo, setWMS)

import Material.Helpers as Helpers


port setLeafletMap : LeafletModel -> Cmd msg


port leafletViewChanged : (( String, LeafletView ) -> msg) -> Sub msg


port leafletRequestState : (String -> msg) -> Sub msg


type Model
    = Model LeafletModel


type alias LeafletModel =
    { id : String
    , state : LeafletState
    }


initModel : String -> Model
initModel id =
    Model { id = id, state = { view = ( 0, 0, 1 ), wmsLayer = Nothing } }


type alias LeafletView =
    ( Float, Float, Float )


type alias WMSInfo =
    { mapName : String
    , layers : List String
    , endPoint : String
    }


type alias LeafletState =
    { view : LeafletView
    , wmsLayer : Maybe WMSInfo
    }


type Msg
    = SetMap LeafletState
    | SetWMSLayer (Maybe WMSInfo)
    | ViewChanged String LeafletView
    | RequestState String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        SetMap newState ->
            let
                newModel =
                    { model | state = newState }
            in
                ( Model newModel, updateMapIfChanged model newModel )

        SetWMSLayer wmsInfo ->
            let
                state =
                    model.state

                newModel =
                    { model | state = { state | wmsLayer = wmsInfo } }
            in
                ( Model newModel, updateMapIfChanged model newModel )

        ViewChanged changedId view ->
            if changedId == model.id then
                let
                    oldState =
                        model.state
                in
                    ( Model { model | state = { oldState | view = view } }, Cmd.none )
            else
                ( Model model, Cmd.none )

        RequestState requestId ->
            if requestId == model.id then
                ( Model model, setLeafletMap model )
            else
                ( Model model, Cmd.none )


updateMapIfChanged : LeafletModel -> LeafletModel -> Cmd Msg
updateMapIfChanged model newModel =
    if newModel /= model then
        setLeafletMap newModel
    else
        Cmd.none


setWMS : (model -> Model) -> (model -> Model -> model) -> (Msg -> msg) -> Maybe WMSInfo -> model -> ( model, Cmd msg )
setWMS getter setter liftMsg wmsInfo =
    Helpers.lift getter setter liftMsg update (SetWMSLayer wmsInfo)


getId : Model -> String
getId (Model { id }) =
    id


subscriptions : (Msg -> msg) -> Sub msg
subscriptions liftMsg =
    Sub.batch
        [ leafletRequestState RequestState
        , leafletViewChanged (\( id, view ) -> ViewChanged id view)
        ]
        |> Sub.map liftMsg
