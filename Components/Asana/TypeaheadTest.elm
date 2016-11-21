import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.App exposing (program)

import Components.Asana.Typeahead as Typeahead
import Components.Asana.Model as Asana
import Components.Asana.Api as Api


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
