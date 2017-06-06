module AddAlgorithmView exposing (..)

import Html exposing (Html)
import Material
import Material.Scheme
import Material.Options as Options
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Color as Color
import AlgorithmDefinition as D


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


view : List D.Algorithm -> Model -> Html Msg
view alreadyAdded model =
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
                availableAlgorithms alreadyAdded model
             else
                []
            )
        ]


availableAlgorithms : List D.Algorithm -> Model -> List (Html Msg)
availableAlgorithms alreadyAdded model =
    let
        clickable def =
            not (List.member def alreadyAdded)

        li i def =
            Options.styled Html.li
                [ Options.onMouseOver (Expand (Just i)) |> Options.when (clickable def)
                , Color.text Color.accent |> Options.when (model.expanded == Just i)
                , Options.onClick (Add def) |> Options.when (clickable def)
                , Options.css "cursor" "pointer"
                ]
                [ Html.text def.name ]
    in
        [ List.indexedMap li D.algorithms
            |> Options.styled Html.ul [ Options.css "margin-top" "0", Options.css "padding-left" "20px" ]
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


main : Program Never Model Msg
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
