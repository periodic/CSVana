module Components.ApiResource exposing (Props, Instance, Msg, Data, create)

import Html exposing (Html)
import Http

import Asana.Api exposing (ApiResult)
import Base

type alias Props data model msg =
    { child: data -> (Base.Instance model msg, Cmd msg)
    , fetch : Cmd (ApiResult data)
    , loadingView : Html (Msg data msg)
    , errorView : Http.Error -> Html (Msg data msg)
    }

type Msg data msg
    = ApiMsg (ApiResult data)
    | ChildMsg msg

type alias Data model = Maybe model
type alias Instance data model msg = Base.Instance (Data model) (Msg data msg)

create : Props data model childMsg -> (Instance data model childMsg, Cmd (Msg data childMsg))
create props =
    Base.create
        { init = init props
        , update = update props
        , subscriptions = subscriptions props
        , view = view props
        , get = get
        }

isLoaded : Model model msg -> Bool
isLoaded model =
    case model of
        Loaded _ ->
            True
        _ ->
            False

--------------------------------------------------------------------------------
-- Private

max_retries : Int
max_retries = 3

type Model model msg
    = Loading Int
    | Error Http.Error
    | Loaded (Base.Instance model msg)

get : Model model msg -> Maybe model
get model =
    case model of
        Loaded child ->
            Just <| Base.get child
        _ ->
            Nothing

init : Props data model msg -> (Model model msg, Cmd (Msg data msg))
init { fetch } =
    (Loading 1, Cmd.map ApiMsg fetch)

-- Note, the type of the resource currently doesn_t matter because it gets replaced...
update : Props data model msg -> Msg data msg -> Model model msg -> (Model model msg, Cmd (Msg data msg))
update props msg model =
    case (msg, model) of
        (ApiMsg apiResult, Loading attempts) ->
            case apiResult of
                Ok data ->
                    let
                        (child, childCmd) = props.child data
                    in
                        (Loaded child, Cmd.map ChildMsg childCmd)
                Err err ->
                    if attempts >= max_retries
                        then
                            (Error (Debug.log "ApiResource received an HTTP error" err), Cmd.none)
                        else
                            case err of
                                Http.BadStatus response ->
                                    if response.status.code == 404
                                        then (Error (Debug.log "Could not find the expected resource" err), Cmd.none)
                                        else (Loading (attempts + 1), Cmd.map ApiMsg props.fetch)
                                Http.Timeout ->
                                    (Loading (attempts + 1), Cmd.map ApiMsg props.fetch)
                                Http.NetworkError ->
                                    (Loading (attempts + 1), Cmd.map ApiMsg props.fetch)
                                _ ->
                                    (Error (Debug.log "ApiResource received an HTTP error" err), Cmd.none)
        (ChildMsg msg, Loaded child) ->
            let
                (child_, childCmd) = Base.update msg child
            in
                (Loaded child_, Cmd.map ChildMsg childCmd)
        _ ->
            -- TODO: Log something here.
            (model, Cmd.none)

subscriptions : Props data model msg -> Model model msg -> Sub (Msg data msg)
subscriptions _ model =
    case model of
        Loaded child ->
            Sub.map ChildMsg <| Base.subscriptions child
        _ ->
            Sub.none

view : Props data model msg -> Model model msg -> Html (Msg data msg)
view props resource =
    case resource of
        Loading _ ->
            props.loadingView
        Error error ->
            props.errorView error
        Loaded child ->
            Base.viewWith ChildMsg child
