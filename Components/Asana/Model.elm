module Components.Asana.Model exposing (..)

import Json.Decode exposing (..)

-- TODO: move decoders to Api.elm.

type alias Id = String

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
    { id: Id
    , name: String
    }

projectDecoder = object2 Project
  ("id" := map toString int)
  ("name" := string)

