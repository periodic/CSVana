module Components.TypeaheadTest exposing (main)

import Html.App exposing (program)

import Asana.Api as Api
import Components.Typeahead as Typeahead


main : Program Never
main =
    program <| Typeahead.spec
        { fetcher = Api.projectTypeahead
                "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJhdXRob3JpemF0aW9uIjoxOTI5Njk3NjgzOTEzMzEsInNjb3BlIjoiZGVmYXVsdCIsImlhdCI6MTQ3Nzg4NTk1OCwiZXhwIjoxNDc3ODg5NTU4fQ.OjoamBpJDRgyiU--rO-EvV1MpKyR_nxTLHNPcxmeVh0"
                "16202385315778"
        }
