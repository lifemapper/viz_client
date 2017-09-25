port module NewOccurrenceSet exposing (..)

import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Events
import Page exposing (Page)
import Json.Decode as Decode
import Csv


type alias Model =
    { selectedFile : Maybe CSVFile
    }


type alias CSVFile =
    { filename : String
    , contents : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (CSVFile -> msg) -> Sub msg


type Msg
    = FileSelected
    | FileRead CSVFile
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileSelected ->
            ( model, fileSelected "foobar" )

        FileRead file ->
            ( { selectedFile = Just file }, Cmd.none )

        Nop ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.selectedFile of
        Nothing ->
            Html.div []
                [ Html.input
                    [ Attribute.type_ "file"
                    , Attribute.id "foobar"
                    , Events.on "change" (Decode.succeed FileSelected)
                    ]
                    []
                ]

        Just csvFile ->
            case Csv.parse csvFile.contents of
                Ok csv ->
                    Html.table []
                        [ Html.thead [] [ Html.tr [] (csv.headers |> List.map (\h -> Html.th [] [ Html.text h ])) ]
                        , Html.tbody [] (csv.records |> List.map (List.map (\d -> Html.td [] [ Html.text d ]) >> Html.tr []))
                        ]

                Err _ ->
                    Html.div [] [ Html.text "Bad CSV" ]


page : Page Model Msg
page =
    { view = view
    , selectedTab = always 0
    , selectTab = always Nop
    , tabTitles = always []
    , subscriptions = always (fileContentRead FileRead)
    , title = "New Species Data"
    }


init : ( Model, Cmd msg )
init =
    ( { selectedFile = Nothing }, Cmd.none )
