module Components.Asana.Model exposing (..)

import Json.Decode exposing (..)
import Http
import Task

apiRoot = "https://app.asana.com/api/1.0"

type Id = Id Int

toInt : Id -> Int
toInt (Id i) = i

type alias ApiResult a = (Result Http.Error a)

type alias Resource =
    { id: Id
    , name: String
    }

type alias User =
  { id: Id
  , name: String
  , email: Maybe String
  , photo: Maybe Photos
  , workspaces: Maybe (List Workspace)
  }

userDecoder = object5 User
  ("id" := map Id int)
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
  { id: Id
  , name: String
  }

workspaceDecoder = object2 Workspace
  ("id" := map Id int)
  ("name" := string)

type alias Project =
    { id: Id
    , name: String
    }

projectDecoder = object2 Project
  ("id" := map Id int)
  ("name" := string)



type alias Query = List (String, String)

apiGetRequest : String -> Query -> Decoder a -> String -> Cmd (ApiResult a)
apiGetRequest path query decoder token =
  let
      url = Http.url (apiRoot ++ path) query
      headers = [("Authorization", "Bearer " ++ token)]
      request =
          { url = url
          , headers = headers
          , verb = "GET"
          , body = Http.empty
          }
      httpRequest =
          Http.send Http.defaultSettings request
          |> Http.fromJson ("data" := decoder)
  in
      Task.perform Err Ok httpRequest


me : String -> Cmd (ApiResult User)
me =
    apiGetRequest "/users/me" [] userDecoder
--  let
--      url = apiRoot ++ "/users/me"
--      headers = [("Authorization", "Bearer " ++ token)]
--      request =
--        { url = url
--        , headers = headers
--        , verb = "GET"
--        , body = Http.empty
--        }
--  in
--      Task.perform ApiFail ApiMe
--        (Http.send Http.defaultSettings request
--        |> Http.fromJson ("data" := userDecoder))

users : Int -> String -> Cmd (ApiResult (List User))
users workspaceId =
    let
        path = "/workspaces/" ++ toString workspaceId ++ "/users"
        query = [("opt_fields", "email,name,photo.image_128x128")]
    in
        apiGetRequest path query (list userDecoder)
--  let
--      url = apiRoot ++ "/workspaces/" ++ toString workspaceId ++ "/users?opt_fields=email,name,photo.image_128x128"
--      headers = [("Authorization", "Bearer " ++ token)]
--      request =
--        { url = url
--        , headers = headers
--        , verb = "GET"
--        , body = Http.empty
--        }
--  in
--      Task.perform ApiFail ApiUsers
--        (Http.send Http.defaultSettings request
--        |> Http.fromJson ("data" := list userDecoder))
