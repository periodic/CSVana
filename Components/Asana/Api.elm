module Components.Asana.Api exposing (Token, ApiResult, me, users, projectTypeahead)

import Http
import Json.Decode exposing (Decoder, (:=), list)
import Task

import Components.Asana.Model as Asana

apiRoot = "https://app.asana.com/api/1.0"

type alias Token = String
type alias ApiResult a = (Result Http.Error a)
type alias Query = List (String, String)

apiGetRequest : String -> Query -> Decoder a -> Token -> Cmd (ApiResult a)
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


me : Token -> Cmd (ApiResult Asana.User)
me =
    apiGetRequest "/users/me" [] Asana.userDecoder
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

users : Asana.WorkspaceId -> Token -> Cmd (ApiResult (List Asana.User))
users workspaceId =
    let
        path = "/workspaces/" ++ workspaceId ++ "/users"
        query = [("opt_fields", "email,name,photo.image_128x128")]
    in
        apiGetRequest path query (list Asana.userDecoder)

type TypeaheadType
    = TypeaheadProject
    | TypeaheadUser
    | TypeaheadTag
    | TypeaheadTask

typeaheadTypeStr : TypeaheadType -> String
typeaheadTypeStr resourceType =
    case resourceType of
        TypeaheadProject ->
            "project"
        TypeaheadUser ->
            "user"
        TypeaheadTag ->
            "tag"
        TypeaheadTask ->
            "task"

getTypeaheadOptions : TypeaheadType -> Decoder a -> Asana.Id -> String -> Token -> Cmd (ApiResult (List a))
getTypeaheadOptions resourceType decoder (workspaceId) fragment  =
    let
        resourceTypeStr = typeaheadTypeStr resourceType
        path = "/workspaces/" ++ workspaceId ++ "/typeahead"
        query =
            [ ("type", resourceTypeStr)
            , ("query", fragment)
            ]
    in
       apiGetRequest path query (Json.Decode.list decoder)

projectTypeahead : Asana.WorkspaceId -> String -> Token -> Cmd (ApiResult (List Asana.Project))
projectTypeahead =
    getTypeaheadOptions TypeaheadProject Asana.projectDecoder
