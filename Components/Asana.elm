module Components.Asana exposing (..)

import Components.OAuth as OAuth

type Msg
    = OAuthMsg OAuth.Msg


type alias Model =
    { oauth: OAuth.Model
    }

init : String -> (Model, Cmd Msg)
init baseUrl =
    let
        (oauthModel, oauthCmd) =
            OAuth.init
                "https://app.asana.com/-/oauth_authorize"
                "192968333753040"
                baseUrl
        model =
            { oauth = oauthModel
            }
        cmd = Cmd.batch [ Cmd.map OAuthMsg oauthCmd ]
    in
        (model, cmd)

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        oauthSubs = OAuth.subscriptions (model.oauth)
        subs = Sub.batch [ Sub.map OAuthMsg oauthSubs ]
    in
        subs

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OAuthMsg msg' ->
            let
                (oauthModel, cmd1) = OAuth.update msg' model.oauth
                (oauthModel', cmd2) =
                    case OAuth.getState oauthModel of
                        OAuth.Ready ->
                            OAuth.authenticate oauthModel
                        _ ->
                            (oauthModel, Cmd.none)
                cmd = Cmd.map OAuthMsg <| Cmd.batch [cmd1, cmd2]
            in
                ({model | oauth = oauthModel'}, cmd)
