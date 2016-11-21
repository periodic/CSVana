module Components.Asana exposing (..)

import Components.Asana.Model as Asana
import Components.Asana.ApiResource as ApiResource
import Components.Asana.Api as Api
import Components.OAuth as OAuth
import Components.Asana.Form as Form

type Msg
    = OAuthMsg OAuth.Msg
    | FormMsg (ApiResource.Msg Asana.User Form.Msg)


type alias FormResource = ApiResource.ApiResource Asana.User Form.Model Form.Msg

type alias Model =
    { oauth: OAuth.Model
    , form: FormResource
    }

init : String -> (Model, Cmd Msg)
init baseUrl =
    let
        (oauthModel, oauthCmd) =
            OAuth.init
                "https://app.asana.com/-/oauth_authorize"
                "192968333753040"
                baseUrl
        (form, _) = ApiResource.init Form.init Form.update
        model =
            { oauth = oauthModel
            , form = form
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
        FormMsg msg' ->
            let
                (form', formCmd) = ApiResource.update msg' model.form
                cmd = Cmd.map FormMsg formCmd
            in
                ({ model | form = form' }, cmd)

bootstrap : Model -> (Model, Cmd Msg)
bootstrap model =
    let
        oauthToken = OAuth.getToken model.oauth
        formUnloaded = ApiResource.isUnloaded model.form
    in
        case (oauthToken, formUnloaded) of
            (Just token, True) ->
                let
                    (form', cmd) = ApiResource.load (Api.me token) model.form
                in
                    ({ model | form = form' }, Cmd.map FormMsg cmd)
            _ ->
                (model, Cmd.none)
