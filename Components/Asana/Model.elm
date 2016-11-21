module Components.Asana.Model exposing (..)

import Json.Decode exposing (..)

-- TODO: move decoders to Api.elm.

type alias Id = String

type alias Resource =
    { id: Id
    , name: String
    }

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

projectDecoder = object3 Project
  ("id" := map toString int)
  ("name" := string)
  ("custom_field_settings" := list customFieldSettingDecoder)

type alias CustomFieldSettingId = Id
type alias CustomFieldSetting =
    { id : Id
    , customField : CustomField
    }

customFieldSettingDecoder = object2 CustomFieldSetting
    ("id" := map toString int)
    ("custom_field" := customFieldDecoder)

type CustomFieldType
    = CustomText
    | CustomNumber
    | CustomEnum
    | CustomUnknown

customFieldTypeDecoder = 
    map (\str -> case str of
            "text" ->
                CustomText
            "number" ->
                CustomNumber
            "enum" ->
                CustomEnum
            _ ->
                CustomUnknown)
        string

type alias CustomFieldId = Id
type alias CustomField =
    { id : Id
    , fieldType : CustomFieldType
    , name : String
    }

customFieldDecoder = object3 CustomField
    ("id" := map toString int)
    ("type" := customFieldTypeDecoder)
    ("name" := string)
