module Asana.Decoder exposing (..)

import Date
import Http
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
    string `andThen` (\str ->
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
resourceDecoder = object2 Asana.Resource
  ("id" := map toString int)
  ("name" := string)

userDecoder : Decoder Asana.User
userDecoder = object5 Asana.User
  ("id" := map toString int)
  ("name" := string)
  (maybe <| "email" := string)
  (maybe <| "photo" := photosDecoder)
  (maybe <| "workspaces" := list workspaceDecoder)

photosDecoder : Decoder Asana.Photos
photosDecoder = object5 Asana.Photos
  (maybe <| "image_21x21" := string)
  (maybe <| "image_27x27" := string)
  (maybe <| "image_36x36" := string)
  (maybe <| "image_60x60" := string)
  (maybe <| "image_128x128" := string)

workspaceDecoder : Decoder Asana.Workspace
workspaceDecoder = object2 Asana.Workspace
  ("id" := map toString int)
  ("name" := string)


projectDecoder : Decoder Asana.Project
projectDecoder = object3 Asana.Project
  ("id" := map toString int)
  ("name" := string)
  ("custom_field_settings" := list customFieldSettingDecoder)


customFieldSettingDecoder : Decoder Asana.CustomFieldSetting
customFieldSettingDecoder = object2 Asana.CustomFieldSetting
    ("id" := map toString int)
    ("custom_field" := customFieldDecoder)

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
customFieldDecoder = object3 Asana.CustomField
    ("id" := map toString int)
    ("type" := customFieldTypeDecoder)
    ("name" := string)

customFieldInfoDecoder : Decoder Asana.CustomFieldInfo
customFieldInfoDecoder =
    ("type" := customFieldTypeDecoder)
    |> flip andThen (\fieldType ->
        case fieldType of
            Asana.CustomText ->
                object2 Asana.CustomTextFieldInfo
                    ("id" := map toString int)
                    ("name" := string)
            Asana.CustomNumber ->
                object3 Asana.CustomNumberFieldInfo
                    ("id" := map toString int)
                    ("name" := string)
                    ("precision" := int)
            Asana.CustomEnum ->
                object3 Asana.CustomEnumFieldInfo
                    ("id" := map toString int)
                    ("name" := string)
                    ("enum_options" := list enumOptionDecoder)
            Asana.CustomUnknown str ->
                fail <| "Got an unknown custom field type '" ++ str ++ "'.")

taskDecoder : Decoder Asana.Task
taskDecoder = object5 Asana.Task
    ("id" := map toString int)
    (maybe <| "name" := string)
    (maybe <| "description" := string)
    (maybe <| "due_on" := dueOnDecoder)
    (maybe <| "due_at" := dueAtDecoder)

enumValueDecoder : Decoder Asana.CustomFieldEnumValue
enumValueDecoder = resourceDecoder

customFieldDataDecoder : Decoder Asana.CustomFieldData
customFieldDataDecoder =
    oneOf
        [ (map Asana.TextValue <| "text_value" := string)
        , (map Asana.NumberValue <| "number_value" := float)
        , (map Asana.EnumValue <| "enum_value" := enumValueDecoder)
        ]

customFieldValueDecoder : Decoder Asana.CustomFieldValue
customFieldValueDecoder = object4 Asana.CustomFieldValue
    ("id" := map toString int)
    ("name" := string)
    ("type" := customFieldTypeDecoder)
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
    string `andThen` (\str ->
        if String.isEmpty str
            then succeed a
            else fail "String is not empty.")
