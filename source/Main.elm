module Main exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Helpers exposing (lift)
import Material.Layout as Layout
import Material.Button as Button
import Material.Options as Options exposing (css)
import Experiments
import OccurrenceSets


-- MODEL


type alias Model =
    { count : Int
    , mdl : Material.Model
    , selectedTab : Int
    , experiments : Experiments.Model
    , occurrenceSets : OccurrenceSets.Model
    }


model : Model
model =
    { count = 0
    , mdl = Material.model
    , selectedTab = 0
    , experiments = Experiments.model
    , occurrenceSets = OccurrenceSets.model
    }



-- ACTION, UPDATE


type Msg
    = Increase
    | Reset
    | Mdl (Material.Msg Msg)
    | SelectTab Int
    | ExperimentsMsg Experiments.Msg
    | OccurrenceSetsMsg OccurrenceSets.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increase ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        Reset ->
            ( { model | count = 0 }
            , Cmd.none
            )

        SelectTab tab ->
            ( { model | selectedTab = tab }
            , Cmd.none
            )

        ExperimentsMsg msg_ ->
            lift .experiments
                (\m x -> { m | experiments = x })
                ExperimentsMsg
                Experiments.update
                msg_
                model

        OccurrenceSetsMsg msg_ ->
            lift .occurrenceSets
                (\m x -> { m | occurrenceSets = x })
                OccurrenceSetsMsg
                OccurrenceSets.update
                msg_
                model

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


type alias Mdl =
    Material.Model


tabs : List ( String, Model -> Html Msg )
tabs =
    [ ( "Experiments", .experiments >> Experiments.view >> Html.map ExperimentsMsg )
    , ( "Occurrence Sets", .occurrenceSets >> OccurrenceSets.view >> Html.map OccurrenceSetsMsg )
    , ( "Scenarios", kraken )
    , ( "Layers", kraken )
    , ( "Projections", kraken )
    ]


tabTitles : ( List (Html Msg), List (Options.Style Msg) )
tabTitles =
    ( List.map (\( t, _ ) -> text t) tabs
    , []
    )


tabViews : Array.Array (Model -> Html Msg)
tabViews =
    Array.fromList <| List.map (\( _, v ) -> v) tabs


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedTabs
        , Layout.selectedTab model.selectedTab
        , Layout.onSelectTab SelectTab
        ]
        { header = header model
        , drawer = []
        , tabs = tabTitles
        , main = [ (Array.get model.selectedTab tabViews |> Maybe.withDefault kraken) model ]
        }


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title [] [ text "LifeMapper SDM" ] ]
    ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Material.init Mdl )
        , view = view
        , subscriptions =
            \model ->
                Sub.batch
                    [ Sub.map OccurrenceSetsMsg <| OccurrenceSets.subscriptions <| model.occurrenceSets
                    , Material.subscriptions Mdl model
                    ]
        , update = update
        }


kraken : Model -> Html Msg
kraken model =
    div
        [ style [ ( "padding", "2rem" ) ] ]
        [ text ("Attacks: " ++ toString model.count)
        , Button.render Mdl
            [ 0 ]
            model.mdl
            [ Options.onClick Increase
            , Button.raised
            , Button.ripple
            , css "margin" "0 24px"
            ]
            [ text "Attack Again!" ]
        , Button.render Mdl
            [ 1 ]
            model.mdl
            [ Options.onClick Reset
            , Button.raised
            , Button.ripple
            ]
            [ text "Reset" ]
        ]
