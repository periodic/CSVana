import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation exposing (Location, program)

import App exposing (..)
import Csv
import FileReader exposing (..)
import View
import OAuth
import Components.Asana.Model as Asana

authUrl = "https://app.asana.com/-/oauth_authorize?response_type=token&client_id=192968333753040&redirect_uri=https%3A%2F%2Flocalhost%3A8000%2F"

init : Location -> (Model, Cmd Msg)
init location =
    let
        (oauthModel, oauthCmd) = OAuth.init authUrl location
        model = 
            { csvData = Nothing
            , oauth = oauthModel
            , workspaces = Loading
            }
    in
       case oauthModel.token of
           Nothing ->
               (model, oauthCmd)
           Just token ->
               (model, Cmd.map ApiMe <| Asana.me token)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
        case msg of
            MoreData chunk ->
                ({model | csvData = Just (Csv.parse chunk) }, Cmd.none)
            NewFiles files ->
                case List.head files of
                    Just file ->
                        (model, readFile file)
                    Nothing ->
                        (model, Cmd.none)
            ApiMe (Ok user) ->
                case user.workspaces of
                    Just workspaces ->
                        ({ model | workspaces = Loaded workspaces }, Cmd.none)
                    Nothing -> 
                        (model, Cmd.none)
            ApiMe (Err _) ->
                (model, Cmd.none)

subscriptions :  Model -> Sub Msg
subscriptions model = fileChunk MoreData

urlUpdate : Location -> Model -> (Model, Cmd Msg)
urlUpdate location model =
  let
      (oauthModel, cmd) = OAuth.urlUpdate location (model.oauth)
  in
      ({ model | oauth = oauthModel }, cmd)

main =
    Navigation.program
        (Navigation.makeParser identity)
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        , view = View.view
        }
