module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation
import String

import Base
import Components.Asana as Asana
import Components.OAuthBoundary as OAuthBoundary

type alias Msg = OAuthBoundary.Msg Asana.Msg
type alias Model =
    OAuthBoundary.Instance Asana.Data Asana.Msg

init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let
        (clientId, baseRedirectUrl) =
            Maybe.withDefault ("", "")
            <| List.head
            <| List.filter (snd >> String.contains location.origin)
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
        OAuthBoundary.create oauthProps

update : Msg -> Model -> (Model, Cmd Msg)
update = Base.update

subscriptions : Model -> Sub Msg
subscriptions = Base.subscriptions

urlUpdate : Navigation.Location -> Model -> (Model, Cmd Msg)
urlUpdate location model =
    (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "Main" ]
        [ header [ class "Main-header" ]
            [ h1 [] [ text "CSVana" ] ]
        , div [ class "Main-form" ]
            [ Base.view model ]
        ]

main : Program Never
main =
    Navigation.program
        (Navigation.makeParser identity)
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        , view = view
        }
