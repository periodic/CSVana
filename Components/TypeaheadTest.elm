module Components.TypeaheadTest exposing (main)

import Html.App exposing (program)

import Asana.Api as Api
import Asana.Model as Asana
import Components.Typeahead as Typeahead


main : Program Never
main =
    program
        { init = Typeahead.init
            "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJhdXRob3JpemF0aW9uIjoxOTI5Njk3NjgzOTEzMzEsInNjb3BlIjoiZGVmYXVsdCIsImlhdCI6MTQ3Nzg4NTk1OCwiZXhwIjoxNDc3ODg5NTU4fQ.OjoamBpJDRgyiU--rO-EvV1MpKyR_nxTLHNPcxmeVh0"
            "16202385315778"
            Api.projectTypeahead
        , update = Typeahead.update
        , subscriptions = always Sub.none
        , view = Typeahead.view
        }
