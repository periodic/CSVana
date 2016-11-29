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
    , Target.CustomField { id = "216726495168054", fieldType = Asana.CustomNumber, name = "Drew's Number" }
    , Target.CustomField { id = "216726495168050", fieldType = Asana.CustomEnum, name = "Drew's Enum" }
    ]

records : List (List String)
records =
    [ [ "Task 1"
      , "Desc 1"
      , "2016-11-24T08:00:00Z" -- Due 11/24
      , ""
      , "String1"
      , "1"  -- 1.00
      , "Option1"
      ]
    , [ "Task 2"
      , "Desc 2"
      , "2016-11-24" -- Due 11/24
      , ""
      , "String2"
      , "2.00" -- 2.00
      , "Option2"
      ]
    , [ "Task 3"
      , "Desc 3"
      , "11/24/2016 16:00:00" -- Due 11/24
      , ""
      , "String3"
      , "3.0001" -- 3.00
      , "Invalid"
      ]
    , [ "Task 4"
      , "Desc 4"
      , ""
      , "2016-11-24" -- Due 11/24 12a
      , "String4"
      , "-4" -- -4.00
      , ""
      ]
    , [ "Task 5"
      , "Desc 5"
      , ""
      , "2016-11-24T08:00:00Z" -- Due 11/24 12a
      , "String5"
      , "" -- null
      , ""
      ]
    , [ "Task 6"
      , "Desc 6"
      , ""
      , "11/24/2016 12:00:00" -- Due 11/24 12p
      , "String6"
      , "1e5" -- null
      , ""
      ]
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

