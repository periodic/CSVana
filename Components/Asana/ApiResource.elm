module Components.Asana.ApiResource exposing (ApiResource, Msg, init, update, view, isLoaded, isUnloaded, load, getChild)

import Base exposing (..)
import Components.Asana.Api exposing (ApiResult)
import Html exposing (Html)
import Html.App as App exposing (map)
import Http

type alias Props data submodel submsg =
    { child : data -> Component submodel submsg
    }

type ResourceState a
    = Unloaded
    | Loading
    | Error Http.Error
    | Loaded a

type alias ApiResource data submodel submsg =
    { state : ResourceState (data, submodel)
    , child : data -> Component submodel submsg
    }

type Msg data submsg
    = ApiMsg (ApiResult data)
    | SubMsg submsg

init : Props data submodel submsg
    -> (ApiResource data submodel submsg, Cmd (Msg data submsg))
init props =
    ({ state = Unloaded, child = props.child }, Cmd.none)

-- Note, the type of the resource currently doesn't matter because it gets replaced...
update : Msg data submsg -> ApiResource data submodel submsg -> (ApiResource data submodel submsg, Cmd (Msg data submsg))
update msg model =
    case msg of
        -- TODO: Should only log an error if we aren't expecting an API message.
        ApiMsg apiResult ->
            case apiResult of 
                Ok data ->
                    let
                        (submodel, subcmd) = model.child data |> .init
                    in
                        ({ model | state = Loaded (data, submodel) }, Cmd.map SubMsg subcmd)
                Err msg ->
                    ({ model | state = Error msg }, Cmd.none)
        SubMsg submsg ->
            case model.state of
                Loaded (data, submodel) ->
                    let
                        (submodel', subcmd) = model.child data |> \comp -> comp.update submsg submodel
                    in
                        ({ model | state = Loaded (data, submodel') }, Cmd.map SubMsg subcmd)
                _ ->
                    -- TODO: Log something here.
                    (model, Cmd.none)

view : Html (Msg data submsg)
    -> Html (Msg data submsg)
    -> (Http.Error -> Html (Msg data submsg))
    -> ApiResource data submodel submsg
    -> Html (Msg data submsg)
view unloaded loading error model =
    case model.state of
        Unloaded ->
            unloaded
        Loading ->
            loading
        Error msg ->
            error msg
        Loaded (data, submodel) ->
            App.map SubMsg <| (\comp -> comp.view submodel) <| model.child data

getChild : ApiResource data submodel submsg -> Maybe submodel
getChild model =
    case model.state of
        Loaded (_, submodel) ->
            Just submodel
        _ ->
            Nothing

isLoaded : ApiResource data submodel submsg -> Bool
isLoaded resource =
    case resource.state of
        Loaded _ ->
            True
        _ ->
            False

isUnloaded : ApiResource data submodel submsg -> Bool
isUnloaded resource =
    case resource.state of
        Unloaded ->
            True
        _ ->
            False

load : Cmd (ApiResult data) -> ApiResource data submodel submsg -> (ApiResource data submodel submsg, Cmd (Msg data submsg))
load fetch model =
    ({ model | state = Loading }, Cmd.map ApiMsg fetch)
