module Components.AsanaTest exposing (main)

import Html.App exposing (..)

import Base
import CommonViews
import Components.Asana as Asana
import Components.OAuthBoundary as OAuthBoundary

main : Program Never
main =
    program
    <| CommonViews.withDebug
    <| Base.asRoot
    <| OAuthBoundary.create
        { baseRedirectUrl =  "https://localhost:8000"
        , baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
        , clientId = "192968333753040"
        , child = \token ->
            Asana.create {
                token = token
            }
        }
