module Components.OAuthBoundary exposing (Instance, Msg, Data, Props, create)

import Html.App
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Asana.Api exposing (Token)
import Base
import OAuth.OAuth as OAuth

type Msg msg
    = OAuthMsg OAuth.Msg
    | ChildMsg msg

type alias Props data msg =
    { baseAuthUrl : String
    , clientId : String
    , baseRedirectUrl : String
    , child : Token -> (Base.Instance data msg, Cmd msg)
    }

type alias Data model = Maybe model
type alias Instance data msg = Base.Instance (Data data) (Msg msg)

create : Props data msg -> (Instance data msg, Cmd (Msg msg))
create props =
    Base.create
        { init = init props
        , update = update props
        , view = view props
        , subscriptions = subscriptions props
        , get = get props
        }

--------------------------------------------------------------------------------
-- Private

type alias Model data msg =
    { child : Maybe (Base.Instance data msg)
    , oauth : OAuth.Model
    }

init : Props data msg -> (Model data msg, Cmd (Msg msg))
init {baseAuthUrl, clientId, baseRedirectUrl} =
    let
        (oauthModel, oauthCmd) = OAuth.init baseAuthUrl clientId baseRedirectUrl
        model =
            { child = Nothing
            , oauth = oauthModel
            }
    in
        (model, Cmd.map OAuthMsg oauthCmd)

update : Props data msg -> Msg msg -> Model data msg -> (Model data msg, Cmd (Msg msg))
update props msg model =
    case msg of
        OAuthMsg msg' ->
            updateOAuth props msg' model
        ChildMsg msg' ->
            updateChild msg' model

updateOAuth : Props data msg -> OAuth.Msg -> Model data msg -> (Model data msg, Cmd (Msg msg))
updateOAuth props msg model =
            let
                (oauth', oauthCmd1) = Base.mapCmd OAuthMsg <| OAuth.update msg model.oauth
            in
                case OAuth.getToken oauth' of
                    Just token ->
                        let
                            (child, childCmd) =
                                props.child token
                                    |> Base.mapCmd ChildMsg
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

updateChild : msg -> Model data msg -> (Model data msg, Cmd (Msg msg))
updateChild msg model =
    case model.child of
        Just child ->
            let
                (child', childCmd) = Base.mapCmd ChildMsg <| Base.update msg child
            in
                ({ model | child = Just child' }, childCmd)
        _ ->
            (model, Cmd.none)

view : Props data msg -> Model data msg -> Html (Msg msg)
view _ model =
    case model.child of
        Just child ->
            Html.App.map ChildMsg <| Base.view child
        _ ->
            div [ class "OAuthBoundary--authorizing" ]
                [ text "Authorizing" ]

subscriptions : Props data msg -> Model data msg -> Sub (Msg msg)
subscriptions _ {oauth, child} =
    let
        oauthSubs = Sub.map OAuthMsg <| OAuth.subscriptions oauth
        childSubs = 
            case child of
                Just child ->
                    Sub.map ChildMsg <| Base.subscriptions child
                _ ->
                    Sub.none
    in
        Sub.batch [oauthSubs, childSubs]

get : Props data msg -> Model data msg -> Maybe data
get _ { child } =
    Maybe.map Base.get child
