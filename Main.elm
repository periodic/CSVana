module Main exposing (main)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Navigation
import String

import Base exposing (..)
import Components.Asana as Asana
import Components.OAuthBoundary as OAuthBoundary

type alias Msg = OAuthBoundary.Msg Asana.Msg
type alias Model =
    { oauthBoundary : OAuthBoundary.Model Asana.Model
    , oauthComponent : Component (OAuthBoundary.Model Asana.Model) (OAuthBoundary.Msg Asana.Msg)
    }

init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let
        asanaComponent = \token ->
            Asana.component { token = token }
        (clientId, baseRedirectUrl) =
            if String.contains "localhost" location.origin
                then ("192968333753040", "https://localhost:8000")
                else ("217803124707970", "https://periodic.github.io/CSVana")
        oauthProps =
                { baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
                , clientId = clientId
                , baseRedirectUrl = baseRedirectUrl
                , childComponent = asanaComponent
                }
        oauthComponent = OAuthBoundary.component oauthProps
        (boundary, boundaryCmd) = oauthComponent.init
    in
        ({ oauthBoundary = boundary, oauthComponent = oauthComponent }, boundaryCmd)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        (oauthBoundary', cmd) = model.oauthComponent.update msg model.oauthBoundary
    in
        ({ model | oauthBoundary = oauthBoundary' }, cmd)

subscriptions : Model -> Sub Msg
subscriptions {oauthComponent, oauthBoundary} =
    oauthComponent.subscriptions oauthBoundary

urlUpdate : Navigation.Location -> Model -> (Model, Cmd Msg)
urlUpdate location model =
    (model, Cmd.none)

view model =
    div [ class "Main" ]
        [ div [ class "Main-header" ]
            [ h1 [] [ text "CSVana : CSV → Asana" ] ]
        , div [ class "Main-form" ]
            [ model.oauthComponent.view model.oauthBoundary ]
        ]

main =
    Navigation.program
        (Navigation.makeParser identity)
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        , view = view
        }
