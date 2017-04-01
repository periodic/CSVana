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
    ApiResource.Msg
        Asana.Project
        (FormSection.Msg
            (ApiParallelResource.Msg Asana.CustomFieldInfo FieldMatcher.Msg)
            FieldSummary.Msg)

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
    ApiResource.Instance
        Asana.Project
        (List (Maybe (Target.Target)))
        (FormSection.Msg
            (ApiParallelResource.Msg Asana.CustomFieldInfo FieldMatcher.Msg)
            FieldSummary.Msg)
    {-
    FormSection.Instance
        FieldMatcher.Data
        (ApiResource.Msg
            Asana.Project
            (ApiParallelResource.Msg Asana.CustomFieldInfo FieldMatcher.Msg))
        (ApiResource.Msg Asana.Project FieldSummary.Msg)
    -}

init : Props -> (Model, Cmd Msg)
init props =
    Util.mapComponent (Base.mapOutput (Maybe.andThen identity))
    <| ApiResource.create
        { child = \project ->
            FormSection.create
                { value = Nothing
                , incompleteChild = \targets ->
                    let
                        customFieldIds =
                            project.customFieldSettings
                                |> Maybe.withDefault []
                                |> List.map (.customField >> .id)
                        numFields = List.length props.headers
                    in
                        ApiParallelResource.create
                            { child = \customFieldInfos ->
                                FieldMatcher.create
                                    { projectId = project.id
                                    , csvHeaders = props.headers
                                    , csvRecords = props.records
                                    , customFields =
                                        -- Make this map the maybe-ness of customFieldSettings
                                        Maybe.map (always customFieldInfos) project.customFieldSettings
                                    , apiContext = props.apiContext
                                    }
                            , fetches =
                                List.map (flip Api.customField props.apiContext.token) customFieldIds
                            , loadingView = CommonViews.loadingIndicator
                            , errorView = CommonViews.errorView
                            }
                , completeChild = \targets ->
                    FieldSummary.create
                        { headers = props.headers
                        , targets = targets
                        , customFields =
                            project.customFieldSettings
                                |> Maybe.withDefault []
                                |> List.map (\setting ->
                                    { id = setting.customField.id, name = setting.customField.name })
                        }
                }
        , fetch = Api.project props.projectId props.apiContext.token
        , loadingView = CommonViews.loadingIndicator
        , errorView = CommonViews.errorView
        }

