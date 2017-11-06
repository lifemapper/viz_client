{-
Copyright (C) 2017, University of Kansas Center for Research

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
module ParameterView
    exposing
        ( Model
        , toApi
        , Msg
        , update
        , view
        , validationError
        , init
        , initFromValues
        )

import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Material
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Tooltip as Tooltip
import Material.Toggles as Toggles
import AlgorithmDefinition as D
import List.Extra exposing (find)
import Maybe.Extra exposing (or)
import Helpers exposing (Index)


type alias Model =
    { definition : D.Parameter
    , value : String
    , validationResult : Result String String
    , focused : Bool
    , mdl : Material.Model
    }


toApi : Model -> ( String, String )
toApi model =
    ( model.definition.name, model.value )


validationError : Model -> Maybe ( String, String )
validationError model =
    case model.validationResult of
        Ok _ ->
            Nothing

        Err msg ->
            Just ( model.definition.name, msg )


init : D.Parameter -> Model
init def =
    let
        initValue =
            Maybe.withDefault "" def.default
    in
        { definition = def
        , value = initValue
        , validationResult = validate def initValue
        , focused = False
        , mdl = Material.model
        }


initFromValues : List ( String, String ) -> D.Parameter -> Model
initFromValues values def =
    let
        givenValue =
            values
                |> find (Tuple.first >> ((==) def.name))
                |> Maybe.map Tuple.second

        initValue =
            or givenValue def.default |> Maybe.withDefault ""
    in
        { definition = def
        , value = initValue
        , validationResult = validate def initValue
        , focused = False
        , mdl = Material.model
        }


type Msg
    = Mdl (Material.Msg Msg)
    | Update String
    | Focused Bool


boundErrMsg : Order -> comparable -> String
boundErrMsg reject bound =
    (case reject of
        LT ->
            "Value must not be less than "

        GT ->
            "Value must not be greater than "

        EQ ->
            "Value must not equal "
    )
        ++ (toString bound)
        ++ "."


checkBound : Maybe comparable -> Order -> comparable -> Result String comparable
checkBound bound reject value =
    let
        compareWith bound =
            if compare value bound == reject then
                Err (boundErrMsg reject bound)
            else
                Ok value
    in
        bound |> Maybe.map compareWith |> Maybe.withDefault (Ok value)


checkBounds :
    (String -> Result String comparable)
    -> Maybe String
    -> Maybe String
    -> String
    -> Result String String
checkBounds parse min max value =
    let
        maybeParse =
            (parse >> Result.toMaybe) |> Maybe.andThen
    in
        parse value
            |> Result.andThen (checkBound (maybeParse min) LT)
            |> Result.andThen (checkBound (maybeParse max) GT)
            |> Result.map (always value)


validate : D.Parameter -> String -> Result String String
validate definition value =
    case definition.dataType of
        D.IntegerParam ->
            checkBounds String.toInt definition.min definition.max value

        D.FloatParam ->
            checkBounds String.toFloat definition.min definition.max value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Update value ->
            ( { model | value = value, validationResult = validate model.definition value }, Cmd.none )

        Focused focused ->
            ( { model | focused = focused }, Cmd.none )


renderValidation : Model -> Textfield.Property m
renderValidation model =
    case model.validationResult of
        Ok _ ->
            Options.nop

        Err error ->
            Textfield.error error


view : Bool -> Index -> Model -> Html Msg
view readOnly idx model =
    case model.definition.options of
        [] ->
            viewField readOnly idx model

        options ->
            if options |> List.all (\{ name } -> name == "Yes" || name == "No") then
                viewSwitch readOnly idx model
            else
                viewOptions readOnly idx model


viewField : Bool -> Index -> Model -> Html Msg
viewField readOnly idx model =
    Html.li [ Attributes.style [ ( "padding", "4px" ) ] ]
        [ Textfield.render Mdl
            (0 :: idx)
            model.mdl
            [ Textfield.label model.definition.displayName
            , Textfield.value model.value
            , Options.attribute <| Attributes.readonly readOnly
            , Options.onInput Update
            , Textfield.floatingLabel
            , renderValidation model
            , Tooltip.attach Mdl (1 :: idx)
            , Options.onFocus (Focused True)
            , Options.onBlur (Focused False)
            ]
            []
        , Tooltip.render Mdl
            (1 :: idx)
            model.mdl
            []
            [ Html.text model.definition.doc ]
        ]


viewOptions : Bool -> Index -> Model -> Html Msg
viewOptions readOnly idx model =
    Html.li [ Attributes.style [ ( "padding", "4px" ) ] ]
        [ Html.p [] [ Html.text model.definition.displayName ]
        , Html.ul [ Attributes.style [ ( "margin", "0" ), ( "padding", "0" ), ( "list-style", "none" ) ] ]
            (List.indexedMap (optionView readOnly idx model) model.definition.options)
        ]


optionView : Bool -> Index -> Model -> Int -> D.ParameterOption -> Html Msg
optionView readOnly idx model i option =
    Html.li []
        [ Toggles.radio Mdl
            (i :: idx)
            model.mdl
            [ Toggles.group (toString idx)
            , Toggles.value (toString option.value == model.value)
            , Toggles.disabled |> Options.when readOnly
            , Options.onToggle (Update <| toString option.value)
            , Options.onFocus (Focused True)
            , Options.onBlur (Focused False)
            ]
            [ Html.text option.name ]
        ]


viewSwitch : Bool -> Index -> Model -> Html Msg
viewSwitch readOnly idx model =
    let
        options =
            model.definition.options
                |> List.map (\{ name, value } -> ( name, value ))
                |> Dict.fromList

        yes =
            options |> Dict.get "Yes" |> Maybe.map toString |> Maybe.withDefault ""

        no =
            options |> Dict.get "No" |> Maybe.map toString |> Maybe.withDefault ""

        toggle =
            if model.value == yes then
                Update no
            else
                Update yes
    in
        Html.li [ Attributes.style [ ( "padding", "4px" ) ] ]
            [ Toggles.switch Mdl
                idx
                model.mdl
                [ Options.onToggle toggle
                , Toggles.value (model.value == yes)
                , Toggles.disabled |> Options.when readOnly
                , Options.onFocus (Focused True)
                , Options.onBlur (Focused False)
                ]
                [ Html.text model.definition.displayName ]
            ]
