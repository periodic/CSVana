module Asana.Decoder exposing (..)

import Date
import Json.Decode exposing (..)
import Maybe
import Result
import String

import Asana.Model as Asana

type DecodeResult a
    = NoResult
    | Value a
    | WorkRequired (Cmd (Maybe a))

dateDecoder : Decoder Date.Date
dateDecoder =
    string |> andThen (\str ->
        case Date.fromString str of
            Ok date ->
                succeed date
            Err msg ->
                fail msg)

dueOnDecoder : Decoder Asana.DueOn
dueOnDecoder =
    map Asana.DueOn dateDecoder

dueAtDecoder : Decoder Asana.DueAt
dueAtDecoder =
    map Asana.DueAt dateDecoder


resourceDecoder : Decoder Asana.Resource
resourceDecoder = map2 Asana.Resource
  (field "id" <| map toString int)
  (field "name" string)

userDecoder : Decoder Asana.User
userDecoder = map5 Asana.User
  (field "id" <| map toString int)
  (field "name" string)
  (maybe <| field "email" string)
  (maybe <| field "photo" photosDecoder)
  (maybe <| field "workspaces" <| list workspaceDecoder)

photosDecoder : Decoder Asana.Photos
photosDecoder = map5 Asana.Photos
  (maybe <| field "image_21x21" string)
  (maybe <| field "image_27x27" string)
  (maybe <| field "image_36x36" string)
  (maybe <| field "image_60x60" string)
  (maybe <| field "image_128x128" string)

workspaceDecoder : Decoder Asana.Workspace
workspaceDecoder = map2 Asana.Workspace
  (field "id" <| map toString int)
  (field "name" string)


projectDecoder : Decoder Asana.Project
projectDecoder = map3 Asana.Project
  (field "id" <| map toString int)
  (field "name" string)
  (maybe <| field "custom_field_settings" <| list customFieldSettingDecoder)


customFieldSettingDecoder : Decoder Asana.CustomFieldSetting
customFieldSettingDecoder = map2 Asana.CustomFieldSetting
    (field "id" <| map toString int)
    (field "custom_field" customFieldDecoder)

customFieldTypeDecoder : Decoder Asana.CustomFieldType
customFieldTypeDecoder = 
    map (\str -> case str of
            "text" ->
                Asana.CustomText
            "number" ->
                Asana.CustomNumber
            "enum" ->
                Asana.CustomEnum
            _ ->
                Asana.CustomUnknown str)
        string

enumOptionDecoder : Decoder Asana.EnumOption
enumOptionDecoder = resourceDecoder

customFieldDecoder : Decoder Asana.CustomField
customFieldDecoder = map3 Asana.CustomField
    (field "id" <| map toString int)
    (field "type" customFieldTypeDecoder)
    (field "name" string)

customFieldInfoDecoder : Decoder Asana.CustomFieldInfo
customFieldInfoDecoder =
    (field "type" customFieldTypeDecoder)
    |> andThen (\fieldType ->
        case fieldType of
            Asana.CustomText ->
                map2 Asana.CustomTextFieldInfo
                    (field "id" <| map toString int)
                    (field "name" string)
            Asana.CustomNumber ->
                map3 Asana.CustomNumberFieldInfo
                    (field "id" <| map toString int)
                    (field "name" string)
                    (field "precision" int)
            Asana.CustomEnum ->
                map3 Asana.CustomEnumFieldInfo
                    (field "id" <| map toString int)
                    (field "name" string)
                    (field "enum_options" <| list enumOptionDecoder)
            Asana.CustomUnknown str ->
                fail <| "Got an unknown custom field type '" ++ str ++ "'.")

taskDecoder : Decoder Asana.Task
taskDecoder = map5 Asana.Task
    (field "id" <| map toString int)
    (maybe <| field "name" string)
    (maybe <| field "description" string)
    (maybe <| field "due_on" dueOnDecoder)
    (maybe <| field "due_at" dueAtDecoder)

enumValueDecoder : Decoder Asana.CustomFieldEnumValue
enumValueDecoder = resourceDecoder

customFieldDataDecoder : Decoder Asana.CustomFieldData
customFieldDataDecoder =
    oneOf
        [ (map Asana.TextValue <| field "text_value" string)
        , (map Asana.NumberValue <| field "number_value" float)
        , (map Asana.EnumValue <| field "enum_value" enumValueDecoder)
        ]

customFieldValueDecoder : Decoder Asana.CustomFieldValue
customFieldValueDecoder = map4 Asana.CustomFieldValue
    (field "id" <| map toString int)
    (field "name" string)
    (field "type" customFieldTypeDecoder)
    (customFieldDataDecoder)

--------------------------------------------------------------------------------
-- Private

emptyAsNull : Decoder String -> Decoder (Maybe String)
emptyAsNull =
    map (\str ->
        if String.isEmpty str
            then Nothing
            else Just str)

nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
    oneOf [null Nothing, map Just decoder]

emptyOrNull : Decoder a -> Decoder (Maybe a)
emptyOrNull decoder =
    oneOf [null Nothing, emptyString Nothing, map Just decoder]

emptyString : a -> Decoder a
emptyString a =
    string |> andThen (\str ->
        if String.isEmpty str
            then succeed a
            else fail "String is not empty.")
