module Components.Configs.UserConfigTest exposing (main)

import Html.App

import Base
import CommonViews
import Components.Configs.UserConfig as UserConfig
import Components.OAuthBoundary as OAuthBoundary

main =
    Html.App.program
    <| CommonViews.withDebug
    <| Base.asRoot
    <| OAuthBoundary.create
        { baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
        , clientId = "192968333753040"
        , baseRedirectUrl = "https://localhost:8000"
        , child =
            \token -> UserConfig.create
                { selectedUser = Nothing
                , token = token
                , workspaceId = "15793206719"
                }
        }
