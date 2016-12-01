module Asana.Target exposing (Target(..), emptyTask, updateTask)

import Date exposing (Date)
import Date.Extra.Format as Format
import Date.Extra.Config.Config_en_us as En_us
import String

import Asana.Model as Asana

type Target
    = Name
    | Description
    | Completed (Dict String Bool)
    | DueDate
    | DueTime
    | CustomField Asana.CustomFieldInfo

type alias Mapping a =
    String -> Result (Maybe a)

emptyTask : Asana.ProjectId -> Asana.NewTask
emptyTask projectId =
    { name = Nothing
    , description = Nothing
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
        DueDate ->
            case Date.fromString value of
                Ok date ->
                    Ok { task | dueOn = Just <| dateToDueOn date }
                Err msg ->
                    Err <| "Could not parse date from '" ++ value ++ "'"
        DueTime ->
            case Date.fromString value of
                Ok date ->
                    Ok { task | dueAt = Just <| dateToDueAt date }
                Err msg ->
                    Err <| "Could not parse date from '" ++ value ++ "'"
        CustomField field ->
            case field of
                Asana.CustomTextFieldInfo id _ ->
                    let
                        newField = (id, Asana.TextValue value)
                        customFields = newField :: task.customFields
                    in
                        Ok { task | customFields = customFields }
                Asana.CustomNumberFieldInfo id _ _ ->
                    case String.toFloat value of
                        Ok num ->
                            let
                                newField = (id, Asana.NumberValue num)
                                customFields = newField :: task.customFields
                            in
                                Ok { task | customFields = customFields }
                        Err msg ->
                            Err <| "Could not parse number from '" ++ value ++ "'"
                Asana.CustomEnumFieldInfo id _ options ->
                    let
                        matchingOptions = List.filter (.name >> (==) value) options
                    in
                        case List.head matchingOptions of
                            Just option ->
                                Ok { task | customFields = (id, Asana.EnumValue option) :: task.customFields }
                            Nothing ->
                                if String.isEmpty value
                                    then Ok task
                                    else Err <| "Could not parse a valid enum option from '" ++ value ++ "'"

dateToDueOn : Date -> String
dateToDueOn date =
    Format.formatOffset En_us.config 0 Format.isoDateFormat date

dateToDueAt : Date -> String
dateToDueAt date =
    Format.format En_us.config Format.isoOffsetFormat date

