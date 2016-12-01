module Components.TypeaheadTest exposing (main)

import Html.App exposing (program)

import Base
import Asana.Api as Api
import Components.Typeahead as Typeahead
import CommonViews
import Components.OAuthBoundary as OAuthBoundary

main : Program Never
main =
    program <| CommonViews.withDebug <| Base.asRoot <|
        OAuthBoundary.create 
            { baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
            , clientId = "192968333753040"
            , baseRedirectUrl = "https://localhost:8000"
            , child =
                \token -> Typeahead.create
                    { fetcher = \fragment -> Api.projectTypeahead "15793206719" fragment token
                    }
            }
