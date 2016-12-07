module Asana.Api exposing (Token, ApiResult, ApiRequest, Context, UserQuery(..), me, user, users, projectTypeahead, userTypeahead, project, customField, createTask)

import Http
import Json.Decode exposing (Decoder, (:=), list, oneOf)
import Json.Encode as Encode exposing (Value)
import Task

import Asana.Model as Asana
import Asana.Encoder as Encoder
import Asana.Decoder as Decoder

apiRoot : String
apiRoot = "https://app.asana.com/api/1.0"

type alias Token = String
type alias ApiResult a = (Result Http.Error a)
type alias Query = List (String, String)
type alias ApiRequest a = Token -> Cmd (ApiResult a)

type alias Context =
    { token : Token
    , workspaceId : Asana.WorkspaceId
    }

apiGetRequest : String -> Query -> Decoder a -> ApiRequest a
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

apiPostRequest : String -> Query -> Value -> Decoder a -> ApiRequest a
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

type UserQuery
    = Me
    | Id Asana.UserId
    | Email String

user : UserQuery -> ApiRequest Asana.User
user user =
    case user of
        Me ->
            apiGetRequest "/users/me" [] Decoder.userDecoder
        Id id ->
            apiGetRequest ("/users/" ++ id) [] Decoder.userDecoder
        Email email ->
            apiGetRequest ("/users/" ++ email) [] Decoder.userDecoder

me : ApiRequest Asana.User
me =
    user Me

users : Asana.WorkspaceId -> ApiRequest (List Asana.User)
users workspaceId =
    let
        path = "/workspaces/" ++ workspaceId ++ "/users"
        query = [("opt_fields", "email,name,photo.image_128x128")]
    in
        apiGetRequest path query (list Decoder.userDecoder)

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

typeaheadQueryFields : TypeaheadType -> List (String, String)
typeaheadQueryFields resourceType =
    case resourceType of
        TypeaheadProject ->
            []
        TypeaheadUser ->
            [("opt_fields", "name,email,photo")]
        TypeaheadTag ->
            []
        TypeaheadTask ->
            []

getTypeaheadOptions : TypeaheadType -> Decoder a -> Asana.Id -> String -> ApiRequest (List a)
getTypeaheadOptions resourceType decoder (workspaceId) fragment  =
    let
        resourceTypeStr = typeaheadTypeStr resourceType
        path = "/workspaces/" ++ workspaceId ++ "/typeahead"
        query = typeaheadQueryFields resourceType ++
            [ ("type", resourceTypeStr)
            , ("query", fragment)
            ]
    in
       apiGetRequest path query (Json.Decode.list decoder)

projectTypeahead : Asana.WorkspaceId -> String -> ApiRequest (List Asana.ProjectResource)
projectTypeahead =
    getTypeaheadOptions TypeaheadProject Decoder.resourceDecoder

userTypeahead : Asana.WorkspaceId -> String -> ApiRequest (List Asana.User)
userTypeahead =
    getTypeaheadOptions TypeaheadUser Decoder.userDecoder

project : Asana.ProjectId -> ApiRequest Asana.Project
project projectId =
    let
        path = "/projects/" ++ projectId
    in
        apiGetRequest path [] Decoder.projectDecoder

customField : Asana.CustomFieldId -> ApiRequest Asana.CustomFieldInfo
customField customFieldId =
    let
        path = "/custom_fields/" ++ customFieldId
    in
        apiGetRequest path [] Decoder.customFieldInfoDecoder

createTask : Asana.NewTask -> ApiRequest Asana.Task
createTask newTask =
    let
        path = "/tasks"
        query = []
        body = Encoder.encodeTask newTask
    in
       apiPostRequest path query body Decoder.taskDecoder



