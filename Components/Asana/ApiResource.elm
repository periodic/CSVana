module Components.Asana.ApiResource exposing (ApiResource, Msg, init, update, view, isLoaded, isUnloaded, load)

import Components.Asana.Api exposing (ApiResult)
import Html exposing (Html)
import Html.App as App exposing (map)
import Http

type ResourceState a
    = Unloaded
    | Loading
    | Error Http.Error
    | Loaded a

type alias ApiResource data submodel submsg =
    { state : ResourceState submodel
    , subinit : data -> (submodel, Cmd submsg)
    , subupdate : submsg -> submodel -> (submodel, Cmd submsg)
    }

type Msg data submsg
    = ApiMsg (ApiResult data)
    | SubMsg submsg

init : (data -> (submodel, Cmd submsg))
    -> (submsg -> submodel -> (submodel, Cmd submsg))
    -> (ApiResource data submodel submsg, Cmd (Msg data submsg))
init subinit subupdate =
    ({ state = Unloaded, subinit = subinit, subupdate = subupdate }, Cmd.none)

-- Note, the type of the resource currently doesn't matter because it gets replaced...
update : Msg data submsg -> ApiResource data submodel submsg -> (ApiResource data submodel submsg, Cmd (Msg data submsg))
update msg model =
    case msg of
        -- TODO: Should only log an error if we aren't expecting an API message.
        ApiMsg apiResult ->
            case apiResult of 
                Ok a ->
                    let
                        (submodel, subcmd) = model.subinit a
                    in
                        ({ model | state = Loaded submodel }, Cmd.map SubMsg subcmd)
                Err msg ->
                    ({ model | state = Error msg }, Cmd.none)
        SubMsg submsg ->
            case model.state of
                Loaded submodel ->
                    let
                        (submodel', subcmd) = model.subupdate submsg submodel
                    in
                        ({ model | state = Loaded submodel' }, Cmd.map SubMsg subcmd)
                _ ->
                    -- TODO: Log something here.
                    (model, Cmd.none)

view : Html (Msg data submsg)
    -> Html (Msg data submsg)
    -> (Http.Error -> Html (Msg data submsg))
    -> (submodel -> Html submsg)
    -> ApiResource data submodel submsg
    -> Html (Msg data submsg)
view unloaded loading error subView model =
    case model.state of
        Unloaded ->
            unloaded
        Loading ->
            loading
        Error msg ->
            error msg
        Loaded submodel ->
            App.map SubMsg <| subView submodel

getState : ApiResource data submodel submsg -> ResourceState submodel
getState = .state

getValue : ApiResource data submodel submsg -> Maybe submodel
getValue model =
    case model.state of
        Loaded submodel ->
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
