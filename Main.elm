module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation
import String

import Base
import Components.Asana as Asana
import Components.OAuthBoundary as OAuthBoundary

type Msg
    = OAuthMsg (OAuthBoundary.Msg Asana.Msg)
    | LocationChange Navigation.Location
type alias Model =
    OAuthBoundary.Instance Asana.Data Asana.Msg

init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let
        (clientId, baseRedirectUrl) =
            Maybe.withDefault ("", "")
            <| List.head
            <| List.filter (Tuple.second >> String.contains location.origin)
                [ ("192968333753040", "https://localhost:8000")
                , ("217803124707970", "https://periodic.github.io/CSVana")
                , ("226996294984037", "https://asana.github.io/CSVana")
                ]
        oauthProps =
                { baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
                , clientId = clientId
                , baseRedirectUrl = baseRedirectUrl
                , child = \token ->
                    Asana.create { token = token }
                }
    in
        OAuthBoundary.create oauthProps |> Base.mapCmd OAuthMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model=
    case msg of
        OAuthMsg msg_ ->
            Base.updateWith OAuthMsg msg_ model
        _ ->
            (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions = Base.subscriptionsWith OAuthMsg

view : Model -> Html Msg
view model =
    div [ class "Main" ]
        [ header [ class "Main-header" ]
            [ h1 [] [ text "CSVana" ] ]
        , div [ class "Main-form" ]
            [ Base.viewWith OAuthMsg model ]
        ]

main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
