module Components.ApiResource exposing (Component, Spec, Msg, component, isLoaded, isUnloaded, getChild)

import Html exposing (Html)
import Html.App as App exposing (map)
import Http

import Asana.Api exposing (ApiResult)
import Base exposing (initC, updateC, viewC, subscriptionsC, stateC)

type alias Props data model msg =
    { childSpec : data -> Base.Spec model msg
    , fetch : Cmd (ApiResult data)
    , unloadedView : Html (Msg data msg)
    , loadingView : Html (Msg data msg)
    , errorView : Http.Error -> Html (Msg data msg)
    }

type Msg data msg
    = ApiMsg (ApiResult data)
    | ChildMsg msg

type alias Spec data model msg = Base.Spec (Model model msg) (Msg data msg)
type alias Component data model msg = Base.Component (Model model msg) (Msg data msg)

component : Props data model msg -> Spec data model msg
component props =
    { init = init props
    , update = update props
    , subscriptions = subscriptions props
    , view = view props
    }

getChild : Component data model msg -> Maybe (Base.Component model msg)
getChild resource =
    case stateC resource of
        Loaded child ->
            Just child
        _ ->
            Nothing

isLoaded : Component data model msg -> Bool
isLoaded resource =
    case stateC resource of
        Loaded _ ->
            True
        _ ->
            False

isUnloaded : Component data model msg -> Bool
isUnloaded resource =
    case stateC resource of
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
    | Loaded (Base.Component model msg)

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
                        (child, childCmd) = initC <| props.childSpec data
                    in
                        (Loaded child, Cmd.map ChildMsg childCmd)
                Err msg ->
                    (Error (Debug.log "ApiResource received an HTTP error" msg), Cmd.none)
        ChildMsg msg ->
            case model of
                Loaded child ->
                    let
                        (child', childCmd) = updateC msg child
                    in
                        (Loaded child', Cmd.map ChildMsg childCmd)
                _ ->
                    -- TODO: Log something here.
                    (model, Cmd.none)

subscriptions : Props data model msg -> Model model msg -> Sub (Msg data msg)
subscriptions _ model =
    case model of
        Loaded child ->
            Sub.map ChildMsg <| subscriptionsC child
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
            App.map ChildMsg <| viewC child
