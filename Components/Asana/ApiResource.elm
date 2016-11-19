module Components.Asana.ApiResource exposing (ApiResource, Msg, init, update, isLoaded, isUnloaded, load, view)

import Components.Asana.Api exposing (ApiResult)
import Html exposing (Html)
import Http

type ApiResource a
    = Unloaded
    | Loading
    | Error Http.Error
    | Loaded a

type alias Msg a = ApiResult a

init : (ApiResource a, Cmd msg)
init =
    (Unloaded, Cmd.none)

-- Note, the type of the resource currently doesn't matter because it gets replaced...
update : Msg a -> ApiResource b -> (ApiResource a, Cmd msg)
update msg _ =
    case msg of
        Ok a ->
            (Loaded a, Cmd.none)
        Err msg ->
            (Error msg, Cmd.none)

isLoaded : ApiResource a -> Bool
isLoaded resource =
    case resource of
        Loaded _ ->
            True
        _ ->
            False

isUnloaded : ApiResource a -> Bool
isUnloaded resource =
    case resource of
        Unloaded ->
            True
        _ ->
            False

view : Html msg -> Html msg -> (Http.Error -> Html msg) -> (a -> Html msg) -> ApiResource a -> Html msg
view unloaded loading error subView model =
    case model of
        Unloaded ->
            unloaded
        Loading ->
            loading
        Error msg ->
            error msg
        Loaded a ->
            subView a

load : Cmd (ApiResult a) -> (ApiResource a, Cmd (Msg a))
load fetch =
    (Loading, fetch)
