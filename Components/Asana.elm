module Components.Asana exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)

import Base exposing (..)
import Components.Asana.Model as Asana
import Components.Asana.ApiResource as ApiResource
import Components.Asana.Api as Api
import Components.OAuthBoundary as OAuthBoundary
import Components.Asana.Form as Form
import Components.Asana.UserLoader as UserLoader

type alias Props =
    { baseUrl : String
    }

type alias Msg = OAuthBoundary.Msg (UserLoader.Msg (Form.Msg))
type alias ChildModel = OAuthBoundary.Model (UserLoader.Model Form.Model Form.Msg)

type alias Model =
    { oauthBoundary : ChildModel
    , oauthComponent : Component ChildModel Msg
    }

init : Props -> (Model, Cmd Msg)
init props =
    let
        form token user =
            Form.component
                { token = token
                , user = user
                }
        userLoader token =
            UserLoader.component
                { childComponent = form token
                , token = token
                }
        oauthProps =
                { baseAuthUrl = "https://app.asana.com/-/oauth_authorize"
                , clientId = "192968333753040"
                , baseRedirectUrl = props.baseUrl
                , childComponent = userLoader
                }
        oauthComponent = OAuthBoundary.component oauthProps
        (boundary, boundaryCmd) = oauthComponent.init
    in
        ({ oauthBoundary = boundary, oauthComponent = oauthComponent }, boundaryCmd)

subscriptions : Props -> Model -> Sub Msg
subscriptions _ { oauthBoundary, oauthComponent } =
    oauthComponent.subscriptions oauthBoundary

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update _ msg model =
    let
        (oauthBoundary', cmd) = model.oauthComponent.update msg model.oauthBoundary
    in
        ({ model | oauthBoundary = oauthBoundary' }, cmd)

view : Props -> Model -> Html Msg
view _ model =
    div [ class "Asana" ]
        [ div [ class "Asana-form" ]
            [ model.oauthComponent.view model.oauthBoundary ]
        ]

getSelectedProject : Model -> Maybe Asana.Project
getSelectedProject model =
    OAuthBoundary.getChild model.oauthBoundary
        `Maybe.andThen` UserLoader.getChild
        `Maybe.andThen` Form.getSelectedProject
