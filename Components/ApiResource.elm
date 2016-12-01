module Components.ApiResource exposing (Props, Instance, Msg, create, isLoaded, isUnloaded)

import Html exposing (Html)
import Html.App as App exposing (map)
import Http

import Asana.Api exposing (ApiResult)
import Base

type alias Props data model msg =
    { child: data -> (Base.Instance model msg, Cmd msg)
    , fetch : Cmd (ApiResult data)
    , unloadedView : Html (Msg data msg)
    , loadingView : Html (Msg data msg)
    , errorView : Http.Error -> Html (Msg data msg)
    }

type Msg data msg
    = ApiMsg (ApiResult data)
    | ChildMsg msg

type alias Instance data model msg = Base.Instance (Maybe model) (Msg data msg)

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

isUnloaded : Model model msg -> Bool
isUnloaded model =
    case model of
        Unloaded ->
            True
        _ ->
            False

--------------------------------------------------------------------------------
-- Private

type Model model msg
    = Unloaded
    | Loading
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
    (Unloaded, Cmd.map ApiMsg fetch)

-- Note, the type of the resource currently doesn't matter because it gets replaced...
update : Props data model msg -> Msg data msg -> Model model msg -> (Model model msg, Cmd (Msg data msg))
update props msg model =
    case msg of
        ApiMsg apiResult ->
            case apiResult of 
                Ok data ->
                    let
                        (child, childCmd) = props.child data
                    in
                        (Loaded child, Cmd.map ChildMsg childCmd)
                Err msg ->
                    (Error (Debug.log "ApiResource received an HTTP error" msg), Cmd.none)
        ChildMsg msg ->
            case model of
                Loaded child ->
                    let
                        (child', childCmd) = Base.update msg child
                    in
                        (Loaded child', Cmd.map ChildMsg childCmd)
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
        Unloaded ->
            props.unloadedView
        Loading ->
            props.loadingView
        Error error ->
            props.errorView error
        Loaded child ->
            App.map ChildMsg <| Base.view child
