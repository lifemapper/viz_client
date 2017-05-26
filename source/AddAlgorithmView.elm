module AddAlgorithmView exposing (..)

import Html exposing (Html)
import Material
import Material.Scheme
import Material.Options as Options
import Material.Grid as Grid exposing (Cell, Device(..), grid, cell)
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.List as L
import Material.Color as Color
import AlgorithmDefinition as D
import Helpers exposing (Index, unsafeGet, removeElem)


type alias Model =
    { raised : Bool
    , expanded : Maybe Int
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | Expand (Maybe Int)
    | Raise Bool
    | Add D.Algorithm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Raise raised ->
            ( { model | raised = raised }, Cmd.none )

        Expand expanded ->
            ( { model | expanded = expanded }, Cmd.none )

        Add _ ->
            ( model, Cmd.none )


setRaised : Bool -> Model -> Model
setRaised raised model =
    { model | raised = raised }


view : Model -> Html Msg
view model =
    Card.view
        [ if model.raised then
            Elevation.e8
          else
            Elevation.e2
        , Options.onMouseEnter (Raise True)
        , Options.onMouseLeave (Raise False)
        , Options.css "width" "100%"
        ]
        [ Card.title []
            [ Card.head [] [ Html.text "Add Algorithm" ]
            , Card.subhead [] [ Html.text "Select a new algorithm to add to the project." ]
            ]
        , Card.text []
            (if model.raised then
                availableAlgorithms model
             else
                []
            )
        ]


availableAlgorithms : Model -> List (Html Msg)
availableAlgorithms model =
    let
        li i def =
            L.li
                [ Options.onMouseOver <| Expand (Just i)
                , Color.text Color.accent |> Options.when (model.expanded == Just i)
                , Options.onClick <| Add def
                ]
                [ L.content [] [ Html.text def.name ] ]
    in
        [ L.ul [] <| List.indexedMap li D.algorithms
        , Options.styled Html.p
            []
            [ model.expanded
                |> Maybe.andThen (\i -> List.drop i D.algorithms |> List.head)
                |> Maybe.map .description
                |> Maybe.withDefault ""
                |> Html.text
            ]
        ]


init : Model
init =
    { raised = False
    , expanded = Nothing
    , mdl = Material.model
    }


main =
    Html.program
        { init = ( init, Material.init Mdl )
        , view = view >> Material.Scheme.top
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Material.subscriptions Mdl model
                    ]
        }
