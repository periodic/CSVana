module Components.Uploader exposing (Props, Msg, Component, component)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import String

import Asana.Api as Api
import Asana.Model as Asana
import Base exposing (..)
import Components.FieldOptions as FieldOptions

type alias Record = List String

type alias Props =
    { token : Api.Token
    , projectId : Asana.ProjectId
    , records : List Record
    , fieldTargets : List FieldOptions.Target
    }

type alias Model =
    { recordsProcessed : Int
    }

type Msg =
    RecordProcessed (Api.ApiResult Asana.Task)

type alias Spec = Base.Spec Model Msg
type alias Component = Base.Component Model Msg

component : Props -> Spec
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = always Sub.none
    }

--------------------------------------------------------------------------------
-- Private

init : Props -> (Model, Cmd Msg)
init props =
    let
        model =
            { recordsProcessed = 0
            }
        cmd =
            Cmd.batch <| List.map (uploadRecord props) props.records
    in
        (model, cmd)


update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case Debug.log "Updater msg" msg of
        RecordProcessed (Ok _) ->
            ({ model | recordsProcessed = model.recordsProcessed + 1 }, Cmd.none)
        RecordProcessed (Err _) ->
            -- TODO: do something about errors.
            (model, Cmd.none)


view : Props -> Model -> Html Msg
view { records } { recordsProcessed } =
    div [ class "Uploader" ]
        [ text <| String.concat [ toString recordsProcessed, " / ", toString <| List.length records ] ]

uploadRecord : Props -> Record -> Cmd Msg
uploadRecord props record =
    let
        fieldSpecs = List.map2 (,) record props.fieldTargets
        newTask = List.foldr addRecordToTask (emptyTask props.projectId) fieldSpecs
    in
        Cmd.map RecordProcessed <| Api.createTask (Debug.log "Creating task" newTask) props.token

addRecordToTask : (String, FieldOptions.Target) -> Api.NewTask -> Api.NewTask
addRecordToTask (value, target) task =
    case target of
        FieldOptions.NameTarget ->
            { task | name = Just value }
        FieldOptions.DescriptionTarget ->
            { task | description = Just value }
        FieldOptions.DueDateTarget ->
            { task | dueDate = Just value }
        _ ->
            task

emptyTask : Asana.ProjectId -> Api.NewTask
emptyTask projectId =
    { name = Nothing
    , description = Nothing
    , dueDate = Nothing
    , projects = Just [projectId]
    }

