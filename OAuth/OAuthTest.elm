module OAuth.OAuthTest exposing (main)

import OAuth.OAuth as OAuth
import Html exposing (..)
import Html.App exposing (..)

type alias Msg = OAuth.Msg
type alias Model = OAuth.Model

init : (Model, Cmd Msg)
init =
    OAuth.init "https://app.asana.com/-/oauth_authorize" "192968333753040" "https://localhost:8000"


view : Model -> Html Msg
view model =
    text <| toString model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    let
        (model', cmd) = OAuth.update msg model
    in
        case OAuth.getState model' of
            OAuth.Ready ->
                OAuth.authenticate model'
            _ ->
                (model', cmd)


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = OAuth.subscriptions
        , view = view
        }
