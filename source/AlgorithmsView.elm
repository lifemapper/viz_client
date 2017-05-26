module AlgorithmsView exposing (..)

import Array
import Html exposing (Html)
import Material
import Material.Scheme
import Material.Options as Options
import Material.Grid as Grid exposing (Cell, Device(..), grid, cell)
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.List as L
import Material.Helpers exposing (lift)
import AlgorithmView
import AlgorithmDefinition as D
import Helpers exposing (Index, unsafeGet, removeElem)


type alias Model =
    { algorithms : Array.Array AlgorithmView.Model
    , addAlgRaised : Bool
    , selectedAlg : Maybe D.Algorithm
    , mdl : Material.Model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | AlgorithmMsg Int AlgorithmView.Msg
    | SelectAddAlg D.Algorithm
    | RaiseAddAlg Bool
    | AddAlg D.Algorithm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        AlgorithmMsg i msg_ ->
            case msg_ of
                AlgorithmView.Remove ->
                    ( { model | algorithms = removeElem i model.algorithms} , Cmd.none)

                _ ->
                    lift
                        (.algorithms >> unsafeGet i)
                        (\m x -> { m | algorithms = Array.set i x m.algorithms })
                        (AlgorithmMsg i)
                        AlgorithmView.update
                        msg_
                        model

        RaiseAddAlg s ->
            ( { model
                | addAlgRaised = s
                , selectedAlg =
                    if s then
                        model.selectedAlg
                    else
                        Nothing
              }
            , Cmd.none
            )

        SelectAddAlg def ->
            ( { model | selectedAlg = Just def }, Cmd.none )

        AddAlg def ->
            ( { model
                | algorithms = Array.push (AlgorithmView.init def) model.algorithms
                , selectedAlg = Nothing
                , addAlgRaised = False
              }
            , Cmd.none
            )


viewAlgorithm : Index -> Int -> AlgorithmView.Model -> Cell Msg
viewAlgorithm index i model =
    cell [ Grid.size All 2 ] [ Html.map (AlgorithmMsg i) <| AlgorithmView.view (i :: index) <| model ]


newAlgorithm : Index -> Model -> Cell Msg
newAlgorithm index model =
    let
        raised =
            model.addAlgRaised
    in
        cell [ Grid.size All 2 ]
            [ Card.view
                [ if raised then
                    Elevation.e8
                  else
                    Elevation.e2
                , Options.onMouseEnter (RaiseAddAlg True)
                , Options.onMouseLeave (RaiseAddAlg False)
                ]
                [ Card.title [] [ Card.head [] [ Html.text "Add Algorithm" ] ]
                , Card.text []
                    [ if raised then
                        case model.selectedAlg of
                            Nothing ->
                                availableAlgorithms model

                            Just def ->
                                Html.text def.description
                      else
                        Html.text "Select a new algorithm to add to the project."
                    ]
                , Card.actions [ Card.border ]
                    [ case model.selectedAlg of
                        Just i ->
                            Button.render Mdl
                                index
                                model.mdl
                                [ Options.onClick (AddAlg i) ]
                                [ Html.text "Add" ]

                        Nothing ->
                            Html.text ""
                    ]
                ]
            ]


availableAlgorithms : Model -> Html Msg
availableAlgorithms model =
    let
        li def =
            L.li [ Options.onClick (SelectAddAlg def) ] [ L.content [] [ Html.text def.name ] ]
    in
        L.ul [] <|
            List.map li D.algorithms


view : Index -> Model -> Html Msg
view index model =
    grid []
        (List.indexedMap (viewAlgorithm index) (Array.toList model.algorithms)
            ++ [ newAlgorithm (Array.length model.algorithms :: index) model ]
        )


init : Model
init =
    { algorithms = Array.empty
    , addAlgRaised = False
    , selectedAlg = Nothing
    , mdl = Material.model
    }


main =
    Html.program
        { init = ( init, Material.init Mdl )
        , view = view [] >> Material.Scheme.top
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Material.subscriptions Mdl model
                    ]
        }
