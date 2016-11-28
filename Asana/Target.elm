module Asana.Target exposing (Target(..), emptyTask, updateTask)

import Date exposing (Date)
import Date.Extra.Format as Format
import Date.Extra.Create as DateCreate
import Date.Extra.Config.Config_en_us as En_us
import Asana.Model as Asana

type Target
    = None
    | Name
    | Description
    | DueDate
    | DueTime
    | CustomField Asana.CustomField

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
            case Date.fromString (Debug.log "============= Raw date" value) of
                Err msg ->
                    Debug.log ("Unable to parse date: " ++ msg) task
                Ok date ->
                    { task | dueOn = Just (Debug.log "Formatted due_on" <| dateToDueOn date) }
        DueTime ->
            case Date.fromString (Debug.log "============= Raw date+time" value) of
                Err msg ->
                    Debug.log ("Unable to parse date: " ++ msg) task
                Ok date ->
                    { task | dueAt = Just (Debug.log "Formatted due_at" <| dateToDueAt date) }
        CustomField field ->
            case field.fieldType of
                Asana.CustomText ->
                    let
                        newField = (field.id, Asana.TextValue value)
                        customFields = newField :: task.customFields
                    in
                        { task | customFields = customFields }
                Asana.CustomNumber ->
                    task
                Asana.CustomEnum ->
                    task
                Asana.CustomUnknown ->
                    task

dateToDueOn : Date -> String
dateToDueOn date =
    Format.formatOffset En_us.config 0 Format.isoDateFormat date

dateToDueAt : Date -> String
dateToDueAt date =
    Format.format En_us.config Format.isoOffsetFormat date


hasTime : Date -> Bool
hasTime date =
    Debug.log ("Checking if date has time: " ++ toString date ++ " -> ") <|
    let 
        tz = Debug.log "timezone" <| DateCreate.getTimezoneOffset date
        time = (Date.hour date * 60 + Date.minute date - tz) % (24 * 60 * 60)
    in
        (Debug.log "Parsed time" (time, Date.second date, Date.millisecond date)) /= (0, 0, 0)

