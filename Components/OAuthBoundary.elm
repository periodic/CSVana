module Components.OAuthBoundary exposing (Component, Spec, Msg, Props, spec, getChild)

import Html.App
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Asana.Api exposing (Token)
import Base
import OAuth.OAuth as OAuth

type alias Model model msg =
    { child : Maybe (Base.Component model msg)
    , oauth : OAuth.Model
    }

type Msg msg
    = OAuthMsg OAuth.Msg
    | ChildMsg msg

type alias Props model msg =
    { baseAuthUrl : String
    , clientId : String
    , baseRedirectUrl : String
    , childSpec : Token -> Base.Spec model msg
    }

type alias Spec model msg = Base.Spec (Model model msg) (Msg msg)
type alias Component model msg = Base.Component (Model model msg) (Msg msg)

spec : Props model msg -> Spec model msg
spec props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = subscriptions props
    }

getChild : Component model msg -> Maybe (Base.Component model msg)
getChild = Base.stateC >> .child

--------------------------------------------------------------------------------
-- Private

init : Props model msg -> (Model model msg, Cmd (Msg msg))
init {baseAuthUrl, clientId, baseRedirectUrl} =
    let
        (oauthModel, oauthCmd) = OAuth.init baseAuthUrl clientId baseRedirectUrl
        model =
            { child = Nothing
            , oauth = oauthModel
            }
    in
        (model, Cmd.map OAuthMsg oauthCmd)

update : Props model msg -> Msg msg -> Model model msg -> (Model model msg, Cmd (Msg msg))
update props msg model =
    case msg of
        OAuthMsg msg' ->
            updateOAuth props msg' model
        ChildMsg msg' ->
            updateChild msg' model

updateOAuth : Props model msg -> OAuth.Msg -> Model model msg -> (Model model msg, Cmd (Msg msg))
updateOAuth props msg model =
            let
                (oauth', oauthCmd1) = Base.mapCmd OAuthMsg <| OAuth.update msg model.oauth
            in
                case OAuth.getToken oauth' of
                    Just token ->
                        let
                            (child, childCmd) = Base.mapCmd ChildMsg <| Base.initC (props.childSpec token)
                            cmd = Cmd.batch [ oauthCmd1, childCmd ]
                        in
                            ({ model | oauth = oauth', child = Just child }, cmd)
                    Nothing ->
                        case OAuth.getState oauth' of
                            OAuth.Ready ->
                                let
                                    (oauth2, oauthCmd2) = Base.mapCmd OAuthMsg <| OAuth.authenticate oauth'
                                in
                                    ({ model | oauth = oauth2 }, Cmd.batch [oauthCmd1, oauthCmd2])
                            _ ->
                                ({ model | oauth = oauth' }, oauthCmd1)

updateChild : msg -> Model model msg -> (Model model msg, Cmd (Msg msg))
updateChild msg model =
    case model.child of
        Just child ->
            let
                (child', childCmd) = Base.mapCmd ChildMsg <| Base.updateC msg child
            in
                ({ model | child = Just child' }, childCmd)
        _ ->
            (model, Cmd.none)

view : Props model msg -> Model model msg -> Html (Msg msg)
view _ model =
    case model.child of
        Just child ->
            Html.App.map ChildMsg <| Base.viewC child
        _ ->
            div [ class "OAuthBoundary--authorizing" ]
                [ text "Authorizing" ]

subscriptions : Props model msg -> Model model msg -> Sub (Msg msg)
subscriptions _ {oauth, child} =
    let
        oauthSubs = Sub.map OAuthMsg <| OAuth.subscriptions oauth
        childSubs = 
            case child of
                Just child ->
                    Sub.map ChildMsg <| Base.subscriptionsC child
                _ ->
                    Sub.none
    in
        Sub.batch [oauthSubs, childSubs]

