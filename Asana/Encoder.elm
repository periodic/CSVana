module Asana.Encoder exposing (..)

import Json.Encode as Encode exposing (Value)
import Maybe
import Date.Extra.Format as Format
import Date.Extra.Config.Config_en_us as En_us

import Asana.Model as Asana

encodeDueOn : Asana.DueOn -> Value
encodeDueOn (Asana.DueOn date) =
    Format.formatOffset En_us.config 0 Format.isoDateFormat date
        |> Encode.string

encodeDueAt : Asana.DueAt -> Value
encodeDueAt (Asana.DueAt date) =
    Format.format En_us.config Format.isoOffsetFormat date
        |> Encode.string

encodeCustomFieldData : Asana.CustomFieldData -> Value
encodeCustomFieldData data =
    case data of
        Asana.TextValue text ->
            Encode.string text
        Asana.NumberValue num ->
            Encode.float num
        Asana.EnumValue enum ->
            Encode.string enum.id

encodeTask : Asana.NewTask -> Value
encodeTask task =
    Encode.object <| List.filterMap identity
        [ Maybe.map (Encode.string >> (,) "name") task.name
        , Maybe.map (Encode.string >> (,) "assignee") task.assignee
        , Maybe.map (Encode.string >> (,) "notes") task.description
        , Maybe.map (Encode.string >> (,) "due_at") task.dueAt
        , Maybe.map (Encode.string >> (,) "due_on") task.dueOn
        , Just ("completed", Encode.bool task.completed)
        , Maybe.map (List.map Encode.string >> Encode.list >> (,) "projects") task.projects
        , List.map (Tuple.mapSecond encodeCustomFieldData) task.customFields |> Encode.object |> (,) "custom_fields" |> Just
        ]

