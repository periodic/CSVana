module Asana.Api exposing (Token, ApiResult, me, users, projectTypeahead, project, createTask)

import Http
import Json.Decode exposing (Decoder, (:=), list, oneOf)
import Json.Encode as Encode exposing (Value)
import Task

import Base
import Asana.Model as Asana

apiRoot : String
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

apiPostRequest : String -> Query -> Value -> Decoder a -> Token -> Cmd (ApiResult a)
apiPostRequest path query body decoder token =
  let
      url = Http.url (apiRoot ++ path) query
      headers = [("Authorization", "Bearer " ++ token)]
      request =
          { url = url
          , headers = headers
          , verb = "POST"
          , body = Http.string <| Encode.encode 4 <| Encode.object [("data", body)]
          }
      httpRequest =
          Http.send Http.defaultSettings request
          |> Http.fromJson ("data" := decoder)
  in
      Task.perform Err Ok httpRequest


me : Token -> Cmd (ApiResult Asana.User)
me =
    apiGetRequest "/users/me" [] Asana.userDecoder

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

projectTypeahead : Asana.WorkspaceId -> String -> Token -> Cmd (ApiResult (List Asana.ProjectResource))
projectTypeahead =
    getTypeaheadOptions TypeaheadProject Asana.resourceDecoder

project : Asana.ProjectId -> Token -> Cmd (ApiResult Asana.Project)
project projectId =
    let
        path = "/projects/" ++ projectId
        query = []
    in
        apiGetRequest path query Asana.projectDecoder

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
encodeTask { name, dueAt, dueOn, description, projects, customFields } =
    Encode.object <| List.filterMap identity
        [ Maybe.map (Encode.string >> (,) "name") name
        , Maybe.map (Encode.string >> (,) "notes") description
        , Maybe.map (Encode.string >> (,) "due_at") dueAt
        , Maybe.map (Encode.string >> (,) "due_on") dueOn
        , Maybe.map (List.map Encode.string >> Encode.list >> (,) "projects") projects
        , List.map (Base.mapSnd encodeCustomFieldData) customFields |> Encode.object |> (,) "custom_fields" |> Just
        ]

createTask : Asana.NewTask -> Token -> Cmd (ApiResult Asana.Task)
createTask newTask =
    let
        path = "/tasks"
        query = []
        body = encodeTask newTask
    in
       apiPostRequest path query body Asana.taskDecoder



