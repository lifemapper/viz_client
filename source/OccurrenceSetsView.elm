module OccurrenceSetsView exposing (..)

import Decoder exposing (AtomObjectRecord)
import OccurrenceSetChooser
import Material
import Material.Options as Options
import Material.Typography as Typo
import Material.List as L
import Material.Helpers as Helpers
import Html exposing (Html)
import Html.Events
import Helpers exposing (Index)


type alias Model =
    { occurrenceSets : List AtomObjectRecord
    , chooser : OccurrenceSetChooser.Model
    , mdl : Material.Model
    }


type Msg
    = ChooserMsg OccurrenceSetChooser.Msg
    | Remove Int
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Remove i ->
            ( { model
                | occurrenceSets =
                    List.append
                        (List.take i model.occurrenceSets)
                        (List.drop (i + 1) model.occurrenceSets)
              }
            , Cmd.none
            )

        ChooserMsg msg_ ->
            Helpers.lift
                .chooser
                (\m x -> { m | chooser = x })
                ChooserMsg
                OccurrenceSetChooser.update
                msg_
                (case msg_ of
                    OccurrenceSetChooser.Select object ->
                        { model | occurrenceSets = model.occurrenceSets ++ [ object ] }

                    msg_ ->
                        model
                )


init : Model
init =
    { occurrenceSets = []
    , chooser = OccurrenceSetChooser.init
    , mdl = Material.model
    }


occurrenceSetLI : Model -> Index -> Int -> AtomObjectRecord -> Html Msg
occurrenceSetLI model index i o =
    L.li []
        [ L.content [] [ Html.text o.name ]
        , L.content2 []
            [ L.icon "delete" [ Options.attribute <| Html.Events.onClick (Remove i) ] ]
        ]


occurrenceSetList : Index -> Model -> Html Msg
occurrenceSetList index model =
    Options.div [ Options.css "margin" "20px" ]
        [ Options.styled Html.p [ Typo.title ] [ Html.text "Choose Occurrence Sets" ]
        , L.ul [] <|
            List.append
                (List.indexedMap (occurrenceSetLI model index) model.occurrenceSets)
                [ (OccurrenceSetChooser.view index model.chooser |> Html.map ChooserMsg) ]
        ]


view : Index -> Model -> Html Msg
view index model =
    Options.div [ Options.css "display" "flex" ]
        [ occurrenceSetList index model
          -- , mapCard index model
        ]
