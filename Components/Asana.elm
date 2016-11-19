module Components.Asana exposing (..)

import Components.Asana.Model as Asana
import Components.Asana.ApiResource as ApiResource
import Components.Asana.Api as Api
import Components.OAuth as OAuth

type Msg
    = OAuthMsg OAuth.Msg
    | CurrentUser (ApiResource.Msg Asana.User)


type alias Model =
    { oauth: OAuth.Model
    , currentUser: ApiResource.ApiResource Asana.User
    }

init : String -> (Model, Cmd Msg)
init baseUrl =
    let
        (oauthModel, oauthCmd) =
            OAuth.init
                "https://app.asana.com/-/oauth_authorize"
                "192968333753040"
                baseUrl
        (currentUser, _) = ApiResource.init
        model =
            { oauth = oauthModel
            , currentUser = currentUser
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
    let
        (model2, msgCmd) = processMessage msg model
        (model3, bootstrapCmd) = bootstrap model2
        cmd = Cmd.batch [msgCmd, bootstrapCmd]
    in
        (model3, cmd)

processMessage : Msg -> Model -> (Model, Cmd Msg)
processMessage msg model =
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
        CurrentUser msg' ->
            let
                (currentUser', cmd) = ApiResource.update msg' model.currentUser
            in
                ({ model | currentUser = currentUser' }, cmd)

bootstrap : Model -> (Model, Cmd Msg)
bootstrap model =
    let
        oauthToken = OAuth.getToken model.oauth
        userUnloaded = ApiResource.isUnloaded model.currentUser
    in
        case (oauthToken, userUnloaded) of
            (Just token, True) ->
                let
                    (currentUser, cmd) = ApiResource.load (Api.me token)
                in
                    ({ model | currentUser = currentUser }, Cmd.map CurrentUser cmd)
            _ ->
                (model, Cmd.none)
