{-
Copyright (C) 2018, University of Kansas Center for Research

Lifemapper Project, lifemapper [at] ku [dot] edu,
Biodiversity Institute,
1345 Jayhawk Boulevard, Lawrence, Kansas, 66045, USA

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.
-}
module AlgorithmView
    exposing
        ( Model
        , toApi
        , fromApi
        , Msg(Remove)
        , update
        , init
        , setRaised
        , view
        , exampleAlgorithm
        , validationErrors
        )

import Decoder
import Html exposing (Html)
import Html.Attributes as Attributes
import Material
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Icon as Icon
import Material.Color as Color
import Material.Options as Options
import Material.Card as Card
import ParameterView
import AlgorithmDefinition as D
import Helpers exposing (Index)
import List.Extra exposing (setAt, getAt)


type alias Model =
    { definition : D.Algorithm
    , parameters : List ParameterView.Model
    , truncateDesc : Bool
    , mouseIn : Bool
    , viewOnly : Bool
    , mdl : Material.Model
    }


toApi : Model -> Decoder.Algorithm
toApi { definition, parameters } =
    Decoder.Algorithm
        { code = definition.code
        , parameters = parameters |> List.map ParameterView.toApi
        }


validationErrors : Model -> List ( String, String )
validationErrors model =
    List.filterMap ParameterView.validationError model.parameters


fromApi : Bool -> Decoder.Algorithm -> Model
fromApi viewOnly (Decoder.Algorithm { code, parameters }) =
    let
        definition =
            case D.getAlgorithmByCode code of
                Nothing ->
                    Debug.crash ("unknown algorithm code: " ++ code)

                Just def ->
                    def

        params =
            definition.parameters
                |> List.map (ParameterView.initFromValues parameters)
    in
        { definition = definition
        , parameters = params
        , truncateDesc = True
        , mouseIn = False
        , viewOnly = viewOnly
        , mdl = Material.model
        }


init : D.Algorithm -> Bool -> Model
init def viewOnly =
    { definition = def
    , parameters = List.map ParameterView.init def.parameters
    , truncateDesc = True
    , mouseIn = False
    , viewOnly = viewOnly
    , mdl = Material.model
    }


type Msg
    = Mdl (Material.Msg Msg)
    | ParameterMsg Int ParameterView.Msg
    | ToggleDesc
    | MouseIn Bool
    | Remove


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDesc ->
            ( { model | truncateDesc = not model.truncateDesc }, Cmd.none )

        MouseIn mouseIn ->
            ( { model | mouseIn = mouseIn }, Cmd.none )

        Remove ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        ParameterMsg i msg_ ->
            getAt i model.parameters
                |> Maybe.map (ParameterView.update msg_)
                |> Maybe.andThen
                    (\( param, cmd ) ->
                        setAt i param model.parameters
                            |> Maybe.map (\params -> ( { model | parameters = params }, Cmd.map (ParameterMsg i) cmd ))
                    )
                |> Maybe.withDefault ( model, Cmd.none )


setRaised : Bool -> Model -> Model
setRaised raised model =
    { model | mouseIn = raised }


raised : Model -> Bool
raised model =
    model.viewOnly || model.mouseIn || (model.parameters |> List.filter (\p -> p.focused) |> List.isEmpty |> not)


descriptionView : Model -> Html Msg
descriptionView model =
    let
        desc =
            model.definition.description
    in
        Options.styled Html.p
            [ Options.onClick ToggleDesc ]
            [ if ((not <| raised model) || model.truncateDesc) && String.length desc > 100 then
                Html.text <| (String.left 100 desc) ++ "..."
              else
                Html.text desc
            ]


parameterView : Bool -> Index -> Int -> ParameterView.Model -> Html Msg
parameterView readOnly index i model =
    Html.map (ParameterMsg i) <| ParameterView.view readOnly (i :: index) <| model


parametersView : Index -> Model -> List (Html Msg)
parametersView index model =
    if raised model then
        [ List.indexedMap (parameterView model.viewOnly index) model.parameters
            |> Html.ul [ Attributes.style [ ( "padding", "0" ), ( "list-style", "none" ) ] ]
        ]
    else
        []


removeButton : Index -> Model -> List (Html Msg)
removeButton index model =
    if model.viewOnly then
        []
    else
        [ Button.render Mdl index model.mdl [ Button.icon, Options.onClick Remove ] [ Icon.i "delete" ] ]


header : Model -> List (Html Msg)
header model =
    case validationErrors model of
        [] ->
            [ Html.text model.definition.name ]

        _ ->
            [ Icon.view "warning" [ Color.text Color.accent, Options.css "margin-right" "5px" ]
            , Html.text model.definition.name
            ]


view : Index -> Model -> Html Msg
view index model =
    Card.view
        [ Elevation.e8 |> Options.when (raised model)
        , Elevation.e2 |> Options.when (not <| raised model)
        , Options.onMouseEnter (MouseIn True)
        , Options.onMouseLeave (MouseIn False)
        , Options.css "width" "100%"
        ]
        [ Card.title [ Options.css "padding-right" "48px" ]
            [ Card.head [] <| header model ]
        , Card.menu [] <| removeButton (-1 :: index) model
        , Card.text []
            (descriptionView model :: parametersView index model)
        ]


exampleAlgorithm : D.Algorithm
exampleAlgorithm =
    case List.head (List.drop 4 D.algorithms) of
        Nothing ->
            Debug.crash "No example algorithm def"

        Just a ->
            a
