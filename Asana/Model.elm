module Asana.Model exposing (..)

import Json.Decode exposing (..)

-- TODO: move decoders to Api.elm.

type alias Id = String

type alias Named a =
    { a | name : String }

type alias Resource =
    { id: Id
    , name: String
    }

resourceDecoder : Decoder Resource
resourceDecoder = object2 Resource
  ("id" := map toString int)
  ("name" := string)

type alias UserId = Id
type alias UserResource = Resource
type alias User =
  { id: Id
  , name: String
  , email: Maybe String
  , photo: Maybe Photos
  , workspaces: Maybe (List Workspace)
  }

userDecoder : Decoder User
userDecoder = object5 User
  ("id" := map toString int)
  ("name" := string)
  (maybe <| "email" := string)
  (maybe <| "photo" := photosDecoder)
  (maybe <| "workspaces" := list workspaceDecoder)

type alias Photos =
  { image_21x21: Maybe String
  , image_27x27: Maybe String
  , image_36x36: Maybe String
  , image_60x60: Maybe String
  , image_128x128: Maybe String
  }

photosDecoder : Decoder Photos
photosDecoder = object5 Photos
  (maybe <| "image_21x21" := string)
  (maybe <| "image_27x27" := string)
  (maybe <| "image_36x36" := string)
  (maybe <| "image_60x60" := string)
  (maybe <| "image_128x128" := string)

type alias WorkspaceId = Id
type alias WorkspaceResource = Resource
type alias Workspace =
  { id: Id
  , name: String
  }

workspaceDecoder : Decoder Workspace
workspaceDecoder = object2 Workspace
  ("id" := map toString int)
  ("name" := string)

type alias ProjectId = Id
type alias ProjectResource = Resource
type alias Project =
    { id : Id
    , name : String
    , customFieldSettings : List CustomFieldSetting
    }

projectDecoder : Decoder Project
projectDecoder = object3 Project
  ("id" := map toString int)
  ("name" := string)
  ("custom_field_settings" := list customFieldSettingDecoder)

type alias CustomFieldSettingId = Id
type alias CustomFieldSetting =
    { id : Id
    , customField : CustomField
    }

customFieldSettingDecoder : Decoder CustomFieldSetting
customFieldSettingDecoder = object2 CustomFieldSetting
    ("id" := map toString int)
    ("custom_field" := customFieldDecoder)

type CustomFieldType
    = CustomText
    | CustomNumber
    | CustomEnum
    | CustomUnknown String

customFieldTypeDecoder : Decoder CustomFieldType
customFieldTypeDecoder = 
    map (\str -> case str of
            "text" ->
                CustomText
            "number" ->
                CustomNumber
            "enum" ->
                CustomEnum
            _ ->
                CustomUnknown str)
        string

type alias EnumOption =
    { id : Id
    , name : String
    -- , enabled : Bool
    -- , color : String
    }

enumOptionDecoder : Decoder EnumOption
enumOptionDecoder = resourceDecoder

type alias CustomFieldId = Id
type alias CustomField =
    { id : Id
    , fieldType : CustomFieldType
    , name : String
    }

customFieldDecoder : Decoder CustomField
customFieldDecoder = object3 CustomField
    ("id" := map toString int)
    ("type" := customFieldTypeDecoder)
    ("name" := string)

type alias CustomFieldInfoId = Id
type CustomFieldInfo
    = CustomTextFieldInfo Id String
    | CustomNumberFieldInfo Id String Int
    | CustomEnumFieldInfo Id String (List EnumOption)

customFieldInfoDecoder : Decoder CustomFieldInfo
customFieldInfoDecoder =
    ("type" := customFieldTypeDecoder)
    |> flip andThen (\fieldType ->
        case fieldType of
            CustomText ->
                object2 CustomTextFieldInfo
                    ("id" := map toString int)
                    ("name" := string)
            CustomNumber ->
                object3 CustomNumberFieldInfo
                    ("id" := map toString int)
                    ("name" := string)
                    ("precision" := int)
            CustomEnum ->
                object3 CustomEnumFieldInfo
                    ("id" := map toString int)
                    ("name" := string)
                    ("enum_options" := list enumOptionDecoder)
            CustomUnknown str ->
                fail <| "Got an unknown custom field type '" ++ str ++ "'.")

type alias TaskId = Id
type alias Task =
    { id : Id
    , name : Maybe String
    , description : Maybe String
    , dueDate : Maybe String
    }

taskDecoder : Decoder Task
taskDecoder = object4 Task
    ("id" := map toString int)
    (maybe <| "name" := string)
    (maybe <| "description" := string)
    (maybe <| "due_date" := string)

type alias CustomFieldEnumValueId = Id
type alias CustomFieldEnumValue =
    { id : CustomFieldEnumValueId
    , name : String
    }

enumValueDecoder : Decoder CustomFieldEnumValue
enumValueDecoder = resourceDecoder

type CustomFieldData
    = TextValue String
    | NumberValue Float
    | EnumValue CustomFieldEnumValue

customFieldDataDecoder : Decoder CustomFieldData
customFieldDataDecoder =
    oneOf
        [ (map TextValue <| "text_value" := string)
        , (map NumberValue <| "number_value" := float)
        , (map EnumValue <| "enum_value" := enumValueDecoder)
        ]

type alias CustomFieldValue =
    { id : CustomFieldId
    , name : String
    , fieldType: CustomFieldType
    , value : CustomFieldData
    }

customFieldValueDecoder : Decoder CustomFieldValue
customFieldValueDecoder = object4 CustomFieldValue
    ("id" := map toString int)
    ("name" := string)
    ("type" := customFieldTypeDecoder)
    (customFieldDataDecoder)

type alias NewTask =
    { name : Maybe String
    , dueOn : Maybe String
    , dueAt : Maybe String
    , description : Maybe String
    , projects : Maybe (List ProjectId)
    , customFields : List (CustomFieldId, CustomFieldData)
    }

