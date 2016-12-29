module Components.MatcherSection exposing (Props, Msg, Data, Instance, create)

import Asana.Api as Api
import Asana.Model as Asana
import Asana.Target as Target
import Base
import CommonViews
import Components.ApiParallelResource as ApiParallelResource
import Components.ApiResource as ApiResource
import Components.FieldMatcher as FieldMatcher
import Components.Fields.Summary as FieldSummary
import Components.FormSection as FormSection
import Util


type alias Props =
    { projectId : Asana.ProjectId
    , headers : List String
    , records : List (List String)
    , apiContext : Api.Context
    }

type alias Msg =
    FormSection.Msg
        (ApiResource.Msg
            Asana.Project
            (ApiParallelResource.Msg Asana.CustomFieldInfo FieldMatcher.Msg))
        (ApiResource.Msg Asana.Project FieldSummary.Msg)

type alias Data = Maybe (List (Maybe Target.Target))
type alias Instance =
    Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = Base.update
        , subscriptions = Base.subscriptions
        , get = Base.get
        , view = Base.view
        }


-------------------------------------------------------------------------------
-- Private


type alias Model =
    FormSection.Instance
        FieldMatcher.Data
        (ApiResource.Msg
            Asana.Project
            (ApiParallelResource.Msg Asana.CustomFieldInfo FieldMatcher.Msg))
        (ApiResource.Msg Asana.Project FieldSummary.Msg)

init : Props -> (Model, Cmd Msg)
init props =
    -- TODO: Move project fetch to the top level.
    FormSection.create
        { value = Nothing
        , incompleteChild = \targets ->
            Util.mapComponent (Base.mapOutput (Maybe.andThen identity))
            <| ApiResource.create
                { child = \project ->
                    let
                        customFieldIds = List.map (.customField >> .id) project.customFieldSettings
                        numFields = List.length props.headers
                    in
                        ApiParallelResource.create
                            { child = \customFieldInfos ->
                                FieldMatcher.create
                                    { projectId = project.id
                                    , csvHeaders = props.headers
                                    , csvRecords = props.records
                                    , customFields = customFieldInfos
                                    , apiContext = props.apiContext
                                    }
                            , fetches = List.map (flip Api.customField props.apiContext.token) customFieldIds
                            , loadingView = CommonViews.loadingIndicator
                            , errorView = CommonViews.errorView
                            }
                , fetch = Api.project props.projectId props.apiContext.token
                , loadingView = CommonViews.loadingIndicator
                , errorView = CommonViews.errorView
                }
        , completeChild = \targets ->
            Util.mapComponent (Base.mapOutput (always ()))
            <| ApiResource.create
                { child = \project ->
                    FieldSummary.create
                        { headers = props.headers
                        , targets = targets
                        , customFields =
                            List.map
                                (\setting -> { id = setting.customField.id, name = setting.customField.name })
                                project.customFieldSettings
                        }
                , fetch = Api.project props.projectId props.apiContext.token
                , loadingView = CommonViews.loadingIndicator
                , errorView = CommonViews.errorView
                }
        }

