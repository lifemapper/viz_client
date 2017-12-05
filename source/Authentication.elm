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


module Authentication exposing (..)

import Regex
import ProgramFlags exposing (Flags)
import Http
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Material.Layout as Layout
import Material.Options as Options
import Navigation as Nav


type Login
    = Unknown
    | LoggingIn
    | NotLoggedIn
    | LoggedIn String
    | BadLogin


type alias LoginInfo =
    { username : String
    , password : String
    , login : Login
    }


initLoginInfo : LoginInfo
initLoginInfo =
    { username = "", password = "", login = Unknown }


type AuthMsg
    = SetUser Login
    | UpdateUserName String
    | UpdatePassword String
    | DoLogin
    | DoLogOut
    | DoReload


authUpdate : Flags -> AuthMsg -> LoginInfo -> ( LoginInfo, Cmd AuthMsg )
authUpdate flags msg loginInfo =
    case msg of
        SetUser user ->
            ( { loginInfo | login = user }, Cmd.none )

        UpdateUserName username ->
            { loginInfo | username = username } ! []

        UpdatePassword password ->
            { loginInfo | password = password } ! []

        DoLogin ->
            loginInfo ! [ doLogin flags loginInfo ]

        DoLogOut ->
            loginInfo ! [ doLogOut flags ]

        DoReload ->
            loginInfo ! [ Nav.reload ]


doLogin : Flags -> LoginInfo -> Cmd AuthMsg
doLogin { apiRoot } { username, password } =
    Http.request
        { method = "POST"
        , headers = []
        , url = Regex.replace Regex.All (Regex.regex "v2/$") (\_ -> "login") apiRoot
        , body =
            Http.multipartBody
                [ Http.stringPart "userid" username
                , Http.stringPart "pword" password
                ]
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = True
        }
        |> Http.send gotLoginResult


doLogOut : Flags -> Cmd AuthMsg
doLogOut { apiRoot } =
    Http.request
        { method = "GET"
        , headers = []
        , url = Regex.replace Regex.All (Regex.regex "v2/$") (\_ -> "logout") apiRoot
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = True
        }
        |> Http.send gotLoginResult


gotLoginResult : Result Http.Error String -> AuthMsg
gotLoginResult result =
    case result of
        Ok string ->
            DoReload

        Err (Http.BadStatus bad) ->
            if bad.status.code == 403 then
                SetUser BadLogin
            else
                Debug.log "Error checking logged in user" (toString bad) |> always (SetUser BadLogin)

        Err err ->
            Debug.log "Error checking logged in user" (toString err) |> always (SetUser BadLogin)


getUser : Flags -> Cmd AuthMsg
getUser { apiRoot } =
    Http.request
        { method = "GET"
        , headers = []
        , url = Regex.replace Regex.All (Regex.regex "v2/$") (\_ -> "login") apiRoot
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = True
        }
        |> Http.send gotUser


gotUser : Result Http.Error String -> AuthMsg
gotUser result =
    case result of
        Ok string ->
            if String.startsWith "Welcome " string then
                string
                    |> String.dropLeft (String.length "Welcome ")
                    |> LoggedIn
                    |> SetUser
            else
                SetUser NotLoggedIn

        Err err ->
            Debug.log "Error checking logged in user" (toString err) |> always (SetUser NotLoggedIn)


userLink : LoginInfo -> List (Html AuthMsg)
userLink loginInfo =
    let
        style =
            Html.Attributes.style [ ( "margin", "2px 5px" ) ]

        loginForm =
            [ Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "Username"
                , Html.Attributes.value loginInfo.username
                , style
                , Html.Events.onInput UpdateUserName
                ]
                []
            , Html.input
                [ Html.Attributes.type_ "password"
                , Html.Attributes.placeholder "Password"
                , Html.Attributes.value loginInfo.password
                , style
                , Html.Events.onInput UpdatePassword
                ]
                []
            , Html.button
                [ Html.Events.onClick DoLogin
                , Html.Attributes.disabled (loginInfo.login == LoggingIn)
                , style
                ]
                [ Html.text "Login" ]
            ]
    in
        case loginInfo.login of
            LoggedIn userName ->
                [ Layout.link [ Options.onClick DoLogOut, Options.css "cursor" "pointer" ] [ Html.text "Logout" ] ]

            BadLogin ->
                (Html.p [ style ] [ Html.text "Invalid username or password." ] :: loginForm)

            Unknown ->
                []

            NotLoggedIn ->
                loginForm

            LoggingIn ->
                loginForm
