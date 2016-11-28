module Components.UploaderTest exposing (main)

import Html.App exposing (program)

import Asana.Model as Asana
import Asana.Target as Target
import Components.Uploader as Uploader
import Components.OAuthBoundary as OAuthBoundary

fieldTargets : List Target.Target
fieldTargets =
    [ Target.Name
    , Target.Description
    , Target.DueDate
    , Target.DueTime
    , Target.CustomField { id = "216726495168048", fieldType = Asana.CustomText, name = "String" }
    ]

records : List (List String)
records =
    [ [ "Task 1 - Due on 11/24", "Desc 1", "2016-11-24T08:00:00Z", "", "CS1" ] -- Just date
    , [ "Task 2 - Due on 11/24", "Desc 2", "2016-11-24", "", "CS2" ] -- Just date
    , [ "Task 3 - Due on 11/24", "Desc 3", "11/24/2016 16:00:00", "", "CS3" ] -- Just date
    , [ "Task 4 - Due on 11/24 12a", "Desc 4", "", "2016-11-24", "CS4" ] -- Date + Time
    , [ "Task 5 - Due on 11/24 12p", "Desc 5", "", "2016-11-24T08:00:00Z", "CS5" ] -- Date + Time
    , [ "Task 6 - Due on 11/24 12p", "Desc 6", "", "11/24/2016 12:00:00", "CS6" ] -- Date + Time
    ]

main : Program Never
main =
    program <| OAuthBoundary.spec
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

