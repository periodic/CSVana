module Asana.Target exposing (Target(..), emptyTask, updateTask)

import Date exposing (Date)
import Date.Extra.Format as Format
import Date.Extra.Config.Config_en_us as En_us
import String

import Asana.Model as Asana

type Target
    = None
    | Name
    | Description
    | DueDate
    | DueTime
    | CustomField Asana.CustomFieldInfo

emptyTask : Asana.ProjectId -> Asana.NewTask
emptyTask projectId =
    { name = Nothing
    , description = Nothing
    , dueOn = Nothing
    , dueAt = Nothing
    , projects = Just [projectId]
    , customFields = []
    }

updateTask : Target -> String -> Asana.NewTask -> Asana.NewTask
updateTask target value task =
    case target of
        None ->
            task
        Name ->
            { task | name = Just value }
        Description ->
            { task | description = Just value }
        DueDate ->
            case Date.fromString value of
                Ok date ->
                    { task | dueOn = Just <| dateToDueOn date }
                Err msg ->
                    -- TODO: Accumulate errors.
                    Debug.log msg task
        DueTime ->
            case Date.fromString value of
                Ok date ->
                    { task | dueAt = Just <| dateToDueAt date }
                Err msg ->
                    -- TODO: Accumulate errors.
                    Debug.log msg task
        CustomField field ->
            case field of
                Asana.CustomTextFieldInfo id _ ->
                    let
                        newField = (id, Asana.TextValue value)
                        customFields = newField :: task.customFields
                    in
                        { task | customFields = customFields }
                Asana.CustomNumberFieldInfo id _ _ ->
                    case String.toFloat value of
                        Ok num ->
                            let
                                newField = (id, Asana.NumberValue num)
                                customFields = newField :: task.customFields
                            in
                                { task | customFields = customFields }
                        Err msg ->
                            -- TODO: Accumulate errors.
                            Debug.log msg task
                Asana.CustomEnumFieldInfo id _ options ->
                    let
                        matchingOptions = List.filter (.name >> (==) value) (Debug.log "All options" options)
                    in
                        case List.head (Debug.log ("Matching options for '" ++ value ++ "'") matchingOptions) of
                            Just option ->
                                { task | customFields = (id, Asana.EnumValue option) :: task.customFields }
                            Nothing ->
                                task

dateToDueOn : Date -> String
dateToDueOn date =
    Format.formatOffset En_us.config 0 Format.isoDateFormat date

dateToDueAt : Date -> String
dateToDueAt date =
    Format.format En_us.config Format.isoOffsetFormat date

