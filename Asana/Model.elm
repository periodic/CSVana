module Asana.Model exposing (..)

import Date

-- TODO: move decoders to Api.elm.

type alias Id = String

type alias Named a =
    { a | name : String }

type alias Resource =
    { id: Id
    , name: String
    }

type alias UserId = Id
type alias UserResource = Resource
type alias User =
  { id: Id
  , name: String
  , email: Maybe String
  , photo: Maybe Photos
  , workspaces: Maybe (List Workspace)
  }

type alias Photos =
  { image_21x21: Maybe String
  , image_27x27: Maybe String
  , image_36x36: Maybe String
  , image_60x60: Maybe String
  , image_128x128: Maybe String
  }

type alias WorkspaceId = Id
type alias WorkspaceResource = Resource
type alias Workspace =
  { id: Id
  , name: String
  }

type alias ProjectId = Id
type alias ProjectResource = Resource
type alias Project =
    { id : Id
    , name : String
    , customFieldSettings : List CustomFieldSetting
    }

type alias CustomFieldSettingId = Id
type alias CustomFieldSetting =
    { id : Id
    , customField : CustomField
    }

type CustomFieldType
    = CustomText
    | CustomNumber
    | CustomEnum
    | CustomUnknown String

type alias EnumOption =
    { id : Id
    , name : String
    -- , enabled : Bool
    -- , color : String
    }

type alias CustomFieldId = Id
type alias CustomField =
    { id : Id
    , fieldType : CustomFieldType
    , name : String
    }

type alias CustomFieldInfoId = Id
type CustomFieldInfo
    = CustomTextFieldInfo Id String
    | CustomNumberFieldInfo Id String Int
    | CustomEnumFieldInfo Id String (List EnumOption)

type DueOn =
    DueOn Date.Date

type DueAt =
    DueAt Date.Date

type alias TaskId = Id
type alias Task =
    { id : Id
    , name : Maybe String
    , description : Maybe String
    , dueOn : Maybe DueOn
    , dueAt : Maybe DueAt
    }

type alias CustomFieldEnumValueId = Id
type alias CustomFieldEnumValue =
    { id : CustomFieldEnumValueId
    , name : String
    }

type CustomFieldData
    = TextValue String
    | NumberValue Float
    | EnumValue CustomFieldEnumValue

type alias CustomFieldValue =
    { id : CustomFieldId
    , name : String
    , fieldType: CustomFieldType
    , value : CustomFieldData
    }

type alias NewTask =
    { name : Maybe String
    , assignee : Maybe UserId
    , completed : Bool
    , dueOn : Maybe String
    , dueAt : Maybe String
    , description : Maybe String
    , projects : Maybe (List ProjectId)
    , customFields : List (CustomFieldId, CustomFieldData)
    }

customFieldInfoToCustomField : CustomFieldInfo -> CustomField
customFieldInfoToCustomField info =
    case info of
        CustomTextFieldInfo id name ->
            { id = id, fieldType = CustomText, name = name}
        CustomNumberFieldInfo id name _ ->
            { id = id, fieldType = CustomNumber, name = name}
        CustomEnumFieldInfo id name _ ->
            { id = id, fieldType = CustomEnum, name = name}

customFieldId : CustomFieldInfo -> Id
customFieldId = customFieldInfoToCustomField >> .id

customFieldName : CustomFieldInfo -> String
customFieldName = customFieldInfoToCustomField >> .name

