module SignUp exposing (..)

import Regex exposing (regex, caseInsensitive)
import List.Extra as List
import Maybe.Extra as Maybe
import Html exposing (Html)
import Html.Attributes as Attributes
import Navigation as Nav
import Http
import Material
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Options as Options
import Material.Card as Card
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typo
import ProgramFlags exposing (Flags)
import Page exposing (Page)


type alias Model =
    { mdl : Material.Model
    , username : String
    , email : String
    , firstName : String
    , lastName : String
    , institution : String
    , address1 : String
    , address2 : String
    , address3 : String
    , phoneNumber : String
    , password : String
    , confirmPassword : String
    , terms : Bool
    , fieldErrors : List (Maybe String)
    , posting : Bool
    , conflicted : Bool
    , failed : Bool
    , flags : Flags
    }


initModel : Flags -> Model
initModel flags =
    { mdl = Material.model
    , username = ""
    , email = ""
    , firstName = ""
    , lastName = ""
    , institution = ""
    , address1 = ""
    , address2 = ""
    , address3 = ""
    , phoneNumber = ""
    , password = ""
    , confirmPassword = ""
    , terms = False
    , fieldErrors = []
    , posting = False
    , conflicted = False
    , failed = False
    , flags = flags
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags, Cmd.none )


type Msg
    = Nop
    | UpdateField Int String
    | SetTerms
    | SignUp
    | Conflicted
    | SignUpComplete
    | SignUpFailed
    | Mdl (Material.Msg Msg)


setAt : Int -> a -> List a -> List a
setAt i value list =
    list
        |> List.indexedMap
            (\j v ->
                if j == i then
                    value
                else
                    v
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        UpdateField i value ->
            let
                field =
                    List.getAt i fields

                model_ =
                    field
                        |> Maybe.map (\( _, updater, _, _ ) -> updater model value)
                        |> Maybe.withDefault model

                errors =
                    model_.fieldErrors
                        |> setAt i (field |> Maybe.map (validateField model_) |> Maybe.join)
            in
                ( { model_ | fieldErrors = errors, conflicted = False }, Cmd.none )

        SetTerms ->
            let
                terms =
                    not model.terms
            in
                ( { model | terms = terms }, Cmd.none )

        SignUp ->
            let
                errors =
                    fields |> List.map (validateField model)

                valid =
                    errors |> List.all ((==) Nothing)

                model_ =
                    { model | fieldErrors = errors, posting = valid }

                cmd =
                    if valid then
                        doPost model
                    else
                        Cmd.none
            in
                ( model_, cmd )

        Conflicted ->
            ( { model | conflicted = True, posting = False }, Cmd.none )

        SignUpFailed ->
            ( { model | failed = True, posting = False }, Cmd.none )

        SignUpComplete ->
            model ! [ Nav.load "#", Nav.reload ]

        Nop ->
            ( model, Cmd.none )


doPost : Model -> Cmd Msg
doPost model =
    Http.request
        { method = "POST"
        , headers = []
        , url = Regex.replace Regex.All (Regex.regex "v2/$") (\_ -> "signup") model.flags.apiRoot
        , body =
            Http.multipartBody
                [ Http.stringPart "userId" model.username
                , Http.stringPart "email" model.email
                , Http.stringPart "firstName" model.firstName
                , Http.stringPart "lastName" model.lastName
                , Http.stringPart "institution" model.institution
                , Http.stringPart "address1" model.address1
                , Http.stringPart "address2" model.address2
                , Http.stringPart "address3" model.address2
                , Http.stringPart "phone" model.phoneNumber
                , Http.stringPart "pword1" model.password
                , Http.stringPart "pword2" model.confirmPassword
                , Http.stringPart "tos" "on"
                ]
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = True
        }
        |> Http.send gotSignUpResult


gotSignUpResult : Result Http.Error String -> Msg
gotSignUpResult result =
    case result of
        Ok string ->
            SignUpComplete

        Err (Http.BadStatus err) ->
            if err.status.code == 409 then
                Conflicted
            else
                Debug.log "Error signing up" (toString err) |> always SignUpFailed

        Err err ->
            Debug.log "Error signing up" (toString err) |> always SignUpFailed


type alias FieldInfo =
    ( Model -> String, Model -> String -> Model, String, List (Model -> String -> Maybe String) )


fields : List FieldInfo
fields =
    [ ( .username, (\m x -> { m | username = x }), "User Name *", [ requireField, maxLength 20, minLength 5 ] )
    , ( .email, (\m x -> { m | email = x }), "Email Address *", [ requireField, maxLength 64, minLength 9 ] )
    , ( .firstName, (\m x -> { m | firstName = x }), "First Name *", [ requireField, maxLength 50, minLength 2 ] )
    , ( .lastName, (\m x -> { m | lastName = x }), "Last Name *", [ requireField, maxLength 50, minLength 2 ] )
    , ( .institution, (\m x -> { m | institution = x }), "Institution", [] )
    , ( .address1, (\m x -> { m | address1 = x }), "Address", [] )
    , ( .address2, (\m x -> { m | address2 = x }), "Address", [] )
    , ( .address3, (\m x -> { m | address3 = x }), "Address", [] )
    , ( .phoneNumber, (\m x -> { m | phoneNumber = x }), "Phone Number", [ maxLength 20, minLength 10 ] )
    , ( .password, (\m x -> { m | password = x }), "Password *", [ requireField, maxLength 32, minLength 8 ] )
    , ( .confirmPassword, (\m x -> { m | confirmPassword = x }), "Confirm Password *", [ requireField, checkPasswords ] )
    ]


errorToOption : Maybe String -> Textfield.Property m
errorToOption error =
    case error of
        Just error ->
            Textfield.error error

        Nothing ->
            Options.nop


validateField : Model -> FieldInfo -> Maybe String
validateField model ( deref, updater, name, validaters ) =
    let
        value =
            deref model
    in
        List.foldl Maybe.or Nothing (List.map (\validate -> validate model value) validaters)


checkPasswords : Model -> String -> Maybe String
checkPasswords model value =
    if model.password /= model.confirmPassword then
        Just "Passwords must match."
    else
        Nothing


requireField : Model -> String -> Maybe String
requireField model value =
    if "" == value then
        Just "Field is required."
    else
        Nothing


maxLength : Int -> Model -> String -> Maybe String
maxLength l model value =
    if String.length value > l then
        Just ("Value must be " ++ (toString l) ++ " characters or less.")
    else
        Nothing


minLength : Int -> Model -> String -> Maybe String
minLength l model value =
    if value /= "" && String.length value < l then
        Just ("Value must be " ++ (toString l) ++ " characters or more.")
    else
        Nothing


textfield : Model -> Int -> FieldInfo -> Html Msg
textfield model i ( deref, updater, name, required ) =
    Textfield.render Mdl
        [ 0, i ]
        model.mdl
        [ Textfield.label name
        , Textfield.floatingLabel
        , Textfield.value (deref model)
        , Options.onInput (UpdateField i)
        , model.fieldErrors |> List.getAt i |> Maybe.join |> errorToOption
        , if Regex.contains (regex "password" |> caseInsensitive) name then
            Textfield.password
          else
            Options.nop
        ]
        []


checkbox : Model -> Html Msg
checkbox model =
    Toggles.checkbox Mdl
        [ 1 ]
        model.mdl
        [ Toggles.value model.terms
        , Options.onToggle SetTerms
        ]
        [ Html.text "I have read and agree to the "
        , Html.a [ Attributes.href "http://lifemapper.org/?page_id=1096", Attributes.target "_blank" ]
            [ Html.text "terms of service" ]
        , Html.text "."
        ]


button : Model -> Html Msg
button model =
    Button.render Mdl
        [ 2 ]
        model.mdl
        [ Button.colored
        , Button.raised
        , Options.onClick SignUp
        , if (not model.terms) || model.posting || model.conflicted then
            Button.disabled
          else
            Options.nop
        ]
        [ Html.text "Sign Up" ]


errorMessage : Model -> Html Msg
errorMessage model =
    if model.conflicted then
        Options.span [ Color.text Color.accent, Options.css "margin-left" "5px" ]
            [ Html.text "Username or email already in use." ]
    else if model.failed then
        Options.span [ Color.text Color.accent, Options.css "margin-left" "5px" ]
            [ Html.text "Error. Try again." ]
    else
        Options.span [] []


requiredMessage : Html msg
requiredMessage =
    Options.styled Html.p [ Typo.caption ] [ Html.text "* required field." ]


view : Model -> Html Msg
view model =
    Card.view
        [ Elevation.e8
        , Options.css "margin" "20px"
        , Options.css "width" "400px"
        ]
        [ Card.title []
            [ Card.head [] [ Html.text "New User Information" ] ]
        , Card.text [] ((fields |> List.indexedMap (textfield model)) ++ [ requiredMessage, checkbox model ])
        , Card.actions [ Card.border ]
            [ button model, errorMessage model ]
        ]


page : Page Model Msg
page =
    { view = view
    , selectedTab = always 0
    , selectTab = always Nop
    , tabTitles = always []
    , subscriptions = always Sub.none
    , title = "New User Sign Up"
    }
