module Components.OAuthBoundary exposing (Model, Msg, Props, component, getChild)

import Base exposing (..)
import Components.Asana.Api exposing (Token)
import Components.OAuth as OAuth
import Html.App
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

type alias Model model =
    { child : Maybe model
    , oauth : OAuth.Model
    }

type Msg msg
    = OAuthMsg OAuth.Msg
    | ChildMsg msg

type alias Props model msg =
    { baseAuthUrl : String
    , clientId : String
    , baseRedirectUrl : String
    , childComponent : Token -> Component model msg
    }

component : Props model msg -> Component (Model model) (Msg msg)
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = subscriptions props
    }

init : Props model msg -> (Model model, Cmd (Msg msg))
init {baseAuthUrl, clientId, baseRedirectUrl} =
    let
        (oauthModel, oauthCmd) = OAuth.init baseAuthUrl clientId baseRedirectUrl
        model =
            { child = Nothing
            , oauth = oauthModel
            }
    in
        (model, Cmd.map OAuthMsg oauthCmd)

update : Props model msg -> Msg msg -> Model model -> (Model model, Cmd (Msg msg))
update {childComponent} msg model =
    case msg of
        OAuthMsg msg' ->
            let
                (oauth', oauthCmd) = OAuth.update msg' model.oauth
            in
                case OAuth.getToken oauth' of
                    Just token ->
                        let
                            (child, childCmd) = childComponent token |> .init
                            cmd = Cmd.batch [ Cmd.map OAuthMsg oauthCmd, Cmd.map ChildMsg childCmd ]
                        in
                            ({ model | oauth = oauth', child = Just child }, cmd)
                    Nothing ->
                        case OAuth.getState oauth' of
                            OAuth.Ready ->
                                let
                                    (oauth2, oauthCmd) = OAuth.authenticate oauth'
                                in
                                    ({ model | oauth = oauth2 }, Cmd.map OAuthMsg oauthCmd)
                            _ ->
                                ({ model | oauth = oauth' }, Cmd.map OAuthMsg oauthCmd)
        ChildMsg msg' ->
            case (model.child, OAuth.getToken model.oauth) of
                (Just child, Just token) ->
                    let
                        (child', childCmd) = childComponent token |> \component -> component.update msg' child
                    in
                        ({ model | child = Just child' }, Cmd.map ChildMsg childCmd)
                _ ->
                    (model, Cmd.none)

view : Props model msg -> Model model -> Html (Msg msg)
view {childComponent} model =
    case (model.child, OAuth.getToken model.oauth) of
        (Just child, Just token) ->
            Html.App.map ChildMsg <| (\comp -> comp.view child) <| childComponent token
        _ ->
            div [ class "OAuthBoundary--authorizing" ]
                [ text "Authorizing" ]

subscriptions : Props model msg -> Model model -> Sub (Msg msg)
subscriptions _ model =
    Sub.map OAuthMsg <| OAuth.subscriptions model.oauth

getChild : Model model -> Maybe model
getChild = .child
