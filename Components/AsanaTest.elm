module Components.AsanaTest exposing (main)

import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)

import CommonViews
import Components.Asana as Asana
import Components.OAuthBoundary as OAuthBoundary

main =
    let
        spec = OAuthBoundary.spec
            { baseRedirectUrl =  "https://localhost:8000"
            , baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
            , clientId = "192968333753040"
            , childSpec = \token ->
                Asana.spec {
                    token = token
                }
            }
    in
        program { spec | view = CommonViews.debugView spec.view}
