module Asana.Target exposing (Target(..), emptyTask, updateTask)

import Date exposing (Date)
import Dict exposing (Dict)
import Date.Extra.Format as Format
import Date.Extra.Config.Config_en_us as En_us
import String

import Asana.Model as Asana

type Target
    = Name
    | Description
    | Assignee (Dict String Asana.UserId)
    | Completion (Dict String Bool)
    | DueDate
    | DueTime
    | CustomText Asana.CustomFieldId
    | CustomNumber Asana.CustomFieldId
    | CustomEnum Asana.CustomFieldId (Dict String Asana.CustomFieldEnumValue)

type alias Mapping a =
    String -> Result (Maybe a)

emptyTask : Asana.ProjectId -> Asana.NewTask
emptyTask projectId =
    { name = Nothing
    , description = Nothing
    , assignee = Nothing
    , completed = False
    , dueOn = Nothing
    , dueAt = Nothing
    , projects = Just [projectId]
    , customFields = []
    }

updateTask : Target -> String -> Asana.NewTask -> Result String Asana.NewTask
updateTask target value task =
    case target of
        Name ->
            Ok { task | name = Just value }
        Description ->
            Ok { task | description = Just value }
        Assignee mappings ->
            case Dict.get value mappings of
                Just val ->
                    Ok { task | assignee = Just val }
                Nothing ->
                    Ok task
        Completion mappings ->
            case Dict.get value mappings of
                Just val ->
                    Ok { task | completed = val }
                Nothing ->
                    Err <| "No config for how to convert '" ++ value ++ "' into a completed status."
        DueDate ->
            if String.isEmpty value then Ok task else
            case Date.fromString value of
                Ok date ->
                    Ok { task | dueOn = Just <| dateToDueOn date }
                Err msg ->
                    Err <| "Could not parse date from '" ++ value ++ "'"
        DueTime ->
            if String.isEmpty value then Ok task else
            case Date.fromString value of
                Ok date ->
                    Ok { task | dueAt = Just <| dateToDueAt date }
                Err msg ->
                    Err <| "Could not parse date from '" ++ value ++ "'"
        CustomText id ->
            let
                newField = (id, Asana.TextValue value)
                customFields = newField :: task.customFields
            in
                Ok { task | customFields = customFields }
        CustomNumber id ->
            case String.toFloat value of
                Ok num ->
                    let
                        newField = (id, Asana.NumberValue num)
                        customFields = newField :: task.customFields
                    in
                        Ok { task | customFields = customFields }
                Err msg ->
                    Err <| "Could not parse number from '" ++ value ++ "'"
        CustomEnum id mappings ->
            case Dict.get value mappings of
                Just option ->
                    Ok { task | customFields = (id, Asana.EnumValue option) :: task.customFields }
                Nothing ->
                    Ok task

dateToDueOn : Date -> String
dateToDueOn date =
    Format.formatOffset En_us.config 0 Format.isoDateFormat date

dateToDueAt : Date -> String
dateToDueAt date =
    Format.format En_us.config Format.isoOffsetFormat date

