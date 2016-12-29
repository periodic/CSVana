module Components.UploaderTest exposing (main)

import Html.App exposing (program)

import Base
import CommonViews
import Asana.Model as Asana
import Asana.Target as Target
import Components.Uploader as Uploader
import Components.OAuthBoundary as OAuthBoundary

-- TODO: All the parsing logic can really be handled in a test on Asana.Parser.
fieldTargets : List (Maybe Target.Target)
fieldTargets =
    [ Just Target.Name
    , Just Target.Description
    , Just Target.DueDate
    , Just Target.DueTime
    , Just <| Target.CustomField <| Asana.CustomTextFieldInfo "216726495168048" "String"
    , Just <| Target.CustomField <| Asana.CustomNumberFieldInfo "216726495168054" "Drew's Number" 2
    , Just <| Target.CustomField <| Asana.CustomEnumFieldInfo "216726495168050" "Drew's Enum"
        [ { id = "216726495168051", name = "Option 1" }
        , { id = "216726495168052", name = "Option 2" }
        ]
    ]

records : List (List String)
records =
    [ [ "Task 1"
      , "Desc 1"
      , "2016-11-24T08:00:00Z" -- Due 11/24
      , ""
      , "String1"
      , "1"  -- 1.00
      , "Option 1"
      ]
    , [ "Task 2"
      , "Desc 2"
      , "2016-11-24" -- Due 11/24
      , ""
      , "String2"
      , "2.00" -- 2.00
      , "Option 2"
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
    program <| CommonViews.withDebug <| Base.asRoot <| OAuthBoundary.create
        { baseRedirectUrl =  "https://localhost:8000"
        , baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
        , clientId = "192968333753040"
        , child = \token ->
            Uploader.create
                { token = token
                , projectId = "216637505526884"
                , records = records
                , fieldTargets = fieldTargets
                }
        }

