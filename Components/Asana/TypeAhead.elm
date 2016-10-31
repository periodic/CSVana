module Components.Asana.TypeAhead exposing (..)

import Components.Asana.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, list)

type ResourceStatus a
    = Unloaded
    | Loading
    | Loaded a

type alias TypeAhead a =
    { options : ResourceStatus (List a)
    , fragment : String
    , token : String
    , workspaceId : Id
    , fetcher : Id -> String -> String -> Cmd (ApiResult (List a))
    }

init : String -> Id -> (Id -> String -> String -> Cmd (ApiResult (List a))) -> TypeAhead a
init token workspaceId fetcher =
    { options = Unloaded
    , fragment = ""
    , token = token
    , workspaceId = workspaceId
    , fetcher = fetcher
    }

type Msg a
    = NewTypeAheadOptions (ApiResult (List a))
    | TypeAheadInput String
    | TypeAheadSelection a

type TypeAheadType
    = TypeAheadProject
    | TypeAheadUser
    | TypeAheadTag
    | TypeAheadTask

typeAheadTypeStr : TypeAheadType -> String
typeAheadTypeStr resourceType =
    case resourceType of
        TypeAheadProject ->
            "project"
        TypeAheadUser ->
            "user"
        TypeAheadTag ->
            "tag"
        TypeAheadTask ->
            "task"

getTypeAheadOptions : TypeAheadType -> Decoder a -> Id -> String -> String -> Cmd (ApiResult (List a))
getTypeAheadOptions resourceType decoder (Id workspaceId) fragment  =
    let
        resourceTypeStr = typeAheadTypeStr resourceType
        path = "/workspaces/" ++ toString workspaceId ++ "/typeahead"
        query =
            [ ("type", resourceTypeStr)
            , ("query", fragment)
            ]
    in
       apiGetRequest path query (Json.Decode.list decoder)

getProjectOptions : Id -> String -> String -> Cmd (ApiResult (List Project))
getProjectOptions =
    getTypeAheadOptions TypeAheadProject projectDecoder

update : Msg a -> TypeAhead a -> (TypeAhead a, Cmd (Msg a))
update msg model =
    case msg of
        NewTypeAheadOptions (Ok items) ->
            ({model | options = Loaded items}, Cmd.none)
        TypeAheadInput fragment ->
            if fragment == ""
               then ({ model | options = Unloaded, fragment = fragment }, Cmd.none)
               else ({ model | options = Loading, fragment = fragment },
                    Cmd.map NewTypeAheadOptions <| model.fetcher model.workspaceId fragment model.token)
        _ ->
            (model, Cmd.none)

render : TypeAhead { a | name : String} -> Html Msg
render model =
    div []
        [ input [ onInput TypeAheadInput, value fragment ] []
        , renderOptions typeAhead
        ]

renderOptions : TypeAhead { a | name : String } -> Html (Msg { a | name : String })
renderOptions {options} =
    div [ class "TypeAhead" ]
        (case options of
            Loaded items ->
                List.map (renderOption) items
            Loading ->
                [ renderLoadingOption ]
            Unloaded ->
                [])

renderOption : { a | name : String } -> Html (Msg { a | name : String })
renderOption resource =
    div [ class "TypeAhead_option"
        , onClick (TypeAheadSelection resource)
        ]
        [ text resource.name ]

renderLoadingOption : Html (Msg { a | name : String  })
renderLoadingOption =
    div [ class "TypeAhead_option__loading"
        ]
        [ text "Loading..." ]

