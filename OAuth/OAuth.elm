port module OAuth.OAuth exposing
    ( Msg
    , AuthState(..)
    , Model
    , init
    , subscriptions
    , getState
    , getToken
    , authenticate
    , update
    )

import String exposing (..)

type alias Token = String

type Msg
  = OAuthToken String
  | OAuthError String
  | OAuthInitialized

type AuthState
    = Success Token
    | InProgress
    | Ready
    | Uninitialized
    | Failure String

type alias Model =
    { baseAuthUrl : String
    , clientId : String
    , redirectUrl : String
    , state : AuthState
    }

subscriptions : Model -> Sub Msg
subscriptions { state } =
    case state of
        InProgress ->
            authComplete resultToMsg
        Uninitialized ->
            initialized (always OAuthInitialized)
        _ ->
            Sub.none

init : String -> String -> String -> (Model, Cmd Msg)
init baseAuthUrl clientId baseUrl =
    let
        redirectUrl = String.concat
            [ baseUrl
            , if endsWith "/" baseUrl
                then ""
                else "/"
            , "OAuth/oauth_success.html"
            ]
        model =
            { baseAuthUrl = baseAuthUrl
            , redirectUrl = redirectUrl
            , clientId = clientId
            , state = Uninitialized
            }
    in
        (model, initAuth ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OAuthInitialized ->
            ({ model | state = Ready }, Cmd.none)
        OAuthToken token ->
            ({ model | state = Success token }, Cmd.none)
        OAuthError error ->
            ({ model | state = Failure error }, Cmd.none)

getState : Model -> AuthState
getState = .state

getToken : Model -> Maybe String
getToken { state } =
    case state of
        Success token ->
            Just token
        _ ->
            Nothing

authenticate : Model -> (Model, Cmd Msg)
authenticate model =
    let
        model' = { model | state = InProgress }
        url = buildAuthUrl model
    in
        if model.state == Ready
            then (model', startAuth url)
            else (model, Cmd.none)

--------------------------------------------------------------------------------
-- Private

-- Sets up the listener
port initAuth : () -> Cmd msg

-- Sets up the listener
port initialized : (Bool -> msg) -> Sub msg

-- Starts the auth flow
port startAuth : String -> Cmd msg

-- Receives the token
port authComplete : ((Bool, String) -> msg) -> Sub msg

resultToMsg : (Bool, String) -> Msg
resultToMsg (success, data) =
    if success
        then
            OAuthToken data
        else
            OAuthError data

buildAuthUrl : Model -> String
buildAuthUrl { baseAuthUrl, redirectUrl, clientId } =
    let
        query =
            join "&" <| List.map (join "=")
                [ [ "response_type", "token" ]
                , [ "client_id", clientId ]
                , [ "redirect_uri", redirectUrl ]
                ]
    in
        concat [ baseAuthUrl, "?", query]

