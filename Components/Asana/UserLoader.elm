module Components.Asana.UserLoader exposing (Model, Msg, Props, component, getChild, updateChild)

import Http
import Html exposing (Html)

import Base exposing (..)
import Components.Asana.Model as Asana
import Components.Asana.Api as Api exposing (Token)
import Components.Asana.ApiResource as ApiResource exposing (ApiResource)
import Components.Asana.CommonViews exposing (..)

type alias Model model msg =
    ApiResource.ApiResource Asana.User model msg

type alias Msg msg =
    ApiResource.Msg Asana.User msg

type alias Props model msg =
    { token : Token
    , childComponent : Asana.User -> Component model msg
    }

component : Props model msg -> Component (Model model msg) (Msg msg)
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = always Sub.none
    }

init : Props model msg -> (ApiResource Asana.User model msg, Cmd (Msg msg))
init { token, childComponent } =
    let
        (resource, cmd1) = ApiResource.init { child = childComponent }
        (resource', cmd2) = ApiResource.load (Api.me token) resource
        cmd = Cmd.batch [cmd1, cmd2]
    in
        (resource', cmd)

update : Props model msg -> Msg msg -> Model model msg -> (Model model msg, Cmd (Msg msg))
update props = ApiResource.update

view : Props model msg -> Model model msg -> Html (Msg msg)
view _ model =
    ApiResource.view unloadedView loadingIndicator errorView model

getChild : Model model msg -> Maybe model
getChild =
    ApiResource.getChild

updateChild : (model -> (model, Cmd msg)) -> Model model msg -> (Model model msg, Cmd (Msg msg))
updateChild =
    ApiResource.updateChild

