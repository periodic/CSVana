module Asana exposing (..)

import Json.Decode exposing (..)
import Http
import Task

apiRoot = "https://app.asana.com/api/1.0"

type ApiResult
  = ApiFail Http.Error
  | ApiMe User
  | ApiUsers (List User)

type alias Resource =
    { id: Int
    , name: String
    }

type alias User =
  { id: Int
  , name: String
  , email: Maybe String
  , photo: Maybe Photos
  , workspaces: Maybe (List Workspace)
  }

userDecoder = object5 User
  ("id" := int)
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

type alias Workspace =
  { id: Int
  , name: String
  }

workspaceDecoder = object2 Workspace
  ("id" := int)
  ("name" := string)

me : String -> Cmd ApiResult
me token  =
  let
      url = apiRoot ++ "/users/me"
      headers = [("Authorization", "Bearer " ++ token)]
      request =
        { url = url
        , headers = headers
        , verb = "GET"
        , body = Http.empty
        }
  in
      Task.perform ApiFail ApiMe
        (Http.send Http.defaultSettings request
        |> Http.fromJson ("data" := userDecoder))

users : Int -> String -> Cmd ApiResult
users workspaceId token =
  let
      url = apiRoot ++ "/workspaces/" ++ toString workspaceId ++ "/users?opt_fields=email,name,photo.image_128x128"
      headers = [("Authorization", "Bearer " ++ token)]
      request =
        { url = url
        , headers = headers
        , verb = "GET"
        , body = Http.empty
        }
  in
      Task.perform ApiFail ApiUsers
        (Http.send Http.defaultSettings request
        |> Http.fromJson ("data" := list userDecoder))



