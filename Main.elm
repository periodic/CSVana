module Main exposing (main)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Navigation

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
        baseUrl = location.href
        oauthProps =
                { baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
                , clientId = "217803124707970"
                , baseRedirectUrl = baseUrl
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
    div [ class "Asana" ]
        [ div [ class "Asana-form" ]
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
