module Components.UploaderTest exposing (main)

import Html.App exposing (program)

import Components.Uploader as Uploader
import Components.OAuthBoundary as OAuthBoundary
import Components.FieldOptions as FieldOptions

fieldTargets =
    [ FieldOptions.NameTarget
    , FieldOptions.DescriptionTarget
    , FieldOptions.DueDateTarget
    ]

records =
    [ [ "Task 1", "Desc 1", "2016-11-24:00:00.000Z" ]
    , [ "Task 2", "Desc 2", "2016-11-25:00:00.000Z" ]
    , [ "Task 3", "Desc 3", "2016-11-26:00:00.000Z" ]
    ]

props =
    { baseRedirectUrl =  "https://localhost:8000"
    , baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
    , clientId = "192968333753040"
    , childSpec = \token ->
        Uploader.spec
            { token = token
            , projectId = "216637505526884"
            , records = records
            , fieldTargets = fieldTargets
            }
    }

main : Program Never
main =
    program <| OAuthBoundary.spec props
