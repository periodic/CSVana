module Components.Asana.ProjectLoader exposing (Props, Model, Msg, component, getChild, load)

import Html exposing (Html)

import Base exposing (..)
import Components.Asana.Api as Api
import Components.Asana.Model as Asana
import Components.Asana.ApiResource as ApiResource exposing (ApiResource)
import Components.Asana.CommonViews exposing (..)

type alias Props model msg =
    { token : Api.Token
    , childComponent : Asana.Project -> Component model msg
    }

type alias Model model msg =
    ApiResource.ApiResource Asana.Project model msg

type alias Msg msg =
    ApiResource.Msg Asana.Project msg

component : Props model msg -> Component (Model model msg) (Msg msg)
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = always Sub.none
    }

init : Props model msg -> (ApiResource Asana.Project model msg, Cmd (Msg msg))
init { token, childComponent } =
    ApiResource.init { child = childComponent }

update : Props model msg -> Msg msg -> Model model msg -> (Model model msg, Cmd (Msg msg))
update props = ApiResource.update

view : Props model msg -> Model model msg -> Html (Msg msg)
view _ model =
    ApiResource.view unloadedView loadingIndicator errorView model

getChild : Model model msg -> Maybe model
getChild =
    ApiResource.getChild

load : Asana.ProjectId -> Api.Token -> Model model msg -> (Model model msg, Cmd (Msg msg))
load projectId token model =
    -- TODO: Only load the project if the project ID has changed.
    ApiResource.load (Api.project projectId token) model
