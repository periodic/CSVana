module Components.Project.Summary exposing (Props, Msg, Data, Instance, create)

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (class)

import Base
import Asana.Model as Asana

type alias Props =
    { project : (Asana.WorkspaceId, Asana.ProjectResource)
    }

type alias Msg =
    ()

type alias Data =
    ()

type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create { project } =
    Base.staticComponent <| view project

--------------------------------------------------------------------------------
-- Private

view : (Asana.WorkspaceId, Asana.ProjectResource) -> Html Msg
view (_, project) =
    div [ class "CsvSummary" ]
        [ text project.name ]
