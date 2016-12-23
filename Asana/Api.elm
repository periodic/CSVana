module Asana.Api exposing (Token, ApiResult, ApiRequest, Context, UserQuery(..), me, user, users, projectTypeahead, userTypeahead, project, customField, createTask)

import Http
import Json.Decode exposing (Decoder, field, list, oneOf, decodeString)
import Json.Encode as Encode exposing (Value)

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

buildUrl : String -> String -> Query -> String
buildUrl origin path query =
    let
        base = origin ++ path
        queryString = query
            |> List.map (\(k,v) -> Http.encodeUri k ++ "=" ++ Http.encodeUri v)
            |> String.join "&"
    in
        if String.isEmpty queryString
            then base
            else base ++ "?" ++ queryString

apiGetRequest : String -> Query -> Decoder a -> ApiRequest a
apiGetRequest path query decoder token =
  let
      url = buildUrl apiRoot path query
      headers =
          [ Http.header "Authorization" ("Bearer " ++ token)
          ]
      request = Http.request
          { method = "GET"
          , url = url
          , headers = headers
          , body = Http.emptyBody
          , expect = Http.expectJson (field "data" decoder)
          , timeout = Nothing
          , withCredentials = False
          }
  in
      Http.send identity request

apiPostRequest : String -> Query -> Value -> Decoder a -> ApiRequest a
apiPostRequest path query body decoder token =
  let
      url = buildUrl apiRoot path query
      headers =
          [ Http.header "Authorization" ("Bearer " ++ token)
          ]
      request = Http.request
          { method = "POST"
          , url = url
          , headers = headers
          , body = Http.jsonBody <| Encode.object [("data", body)]
          , expect = Http.expectJson (field "data" decoder)
          , timeout = Nothing
          , withCredentials = False
          }
  in
      Http.send identity request

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



