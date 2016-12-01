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
    OAuthBoundary.Component Asana.Model Asana.Msg

init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let
        (clientId, baseRedirectUrl) =
            if String.contains "localhost" location.origin
                then ("192968333753040", "https://localhost:8000")
                else ("217803124707970", "https://periodic.github.io/CSVana")
        oauthProps =
                { baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
                , clientId = clientId
                , baseRedirectUrl = baseRedirectUrl
                , childSpec = \token ->
                    Asana.spec { token = token }
                }
        oauthSpec = OAuthBoundary.spec oauthProps
    in
        Base.initC oauthSpec

update : Msg -> Model -> (Model, Cmd Msg)
update = Base.updateC 

subscriptions : Model -> Sub Msg
subscriptions = Base.subscriptionsC

urlUpdate : Navigation.Location -> Model -> (Model, Cmd Msg)
urlUpdate location model =
    (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "Main" ]
        [ header [ class "Main-header" ]
            [ h1 [] [ text "CSVana" ] ]
        , div [ class "Main-form" ]
            [ Base.viewC model ]
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
