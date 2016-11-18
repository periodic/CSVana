module AsanaTest exposing (main)

import Components.Asana as Asana
import Html exposing (..)
import Html.App exposing (..)

main : Program Never
main =
    program
        { init = Asana.init "https://localhost:8000"
        , update = Asana.update
        , subscriptions = Asana.subscriptions
        , view = text << toString
        }
