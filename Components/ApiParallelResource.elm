module Components.ApiParallelResource exposing (Props, Data, Msg, Instance, create)

import Array
import Html exposing (Html)
import Http

import Asana.Api exposing (ApiResult)
import Base

type alias Props data model msg =
    { child : List data -> (Base.Instance model msg, Cmd msg)
    , fetches : List (Cmd (ApiResult data))
    , unloadedView : Html (Msg data msg)
    , loadingView : Html (Msg data msg)
    , errorView : Http.Error -> Html (Msg data msg)
    }

type Msg data msg
    = ApiMsg Int (ApiResult data)
    | ChildMsg msg

-- TODO: deal with the parallel fetches.

type alias Data model = Maybe model
type alias Instance data model msg = Base.Instance (Data model) (Msg data msg)

create : Props data model msg -> (Instance data model msg, Cmd (Msg data msg))
create props =
    Base.create
        { init = init props
        , update = update props
        , subscriptions = subscriptions props
        , view = view props
        , get = get
        }

--------------------------------------------------------------------------------
-- Private

type Model data model msg
    = Unloaded
    | Loading (Array.Array (Maybe data))
    | Error Http.Error
    | Loaded (Base.Instance model msg)

get : Model data model msg -> Maybe model
get resource =
    case resource of
        Loaded child ->
            Just <| Base.get child
        _ ->
            Nothing

init : Props data model msg -> (Model data model msg, Cmd (Msg data msg))
init { fetches, child } =
    if List.isEmpty fetches
        -- Automatically load if there is nothing to fetch.
        -- TODO: Combine this with the logic in update.
        then Base.pairMap Loaded (Cmd.map ChildMsg) <| child []
        else
            let
                model = Loading <| Array.repeat (List.length fetches) Nothing
                cmd = Cmd.batch <| List.indexedMap (Cmd.map << ApiMsg) fetches
            in
                (model , cmd)

-- Note, the type of the resource currently doesn't matter because it gets replaced...
update : Props data model msg -> Msg data msg -> Model data model msg -> (Model data model msg, Cmd (Msg data msg))
update props msg model =
    case msg of
        ApiMsg index apiResult ->
            case (model, apiResult) of 
                (Loading data, Ok datum) ->
                    let
                        loadingData = Array.set index (Just datum) data
                    in
                        if List.all Base.isJust <| Array.toList loadingData
                            then 
                                let
                                    loadedData = List.filterMap identity <| Array.toList loadingData
                                    (child, childCmd) = props.child loadedData
                                in
                                    (Loaded child, Cmd.map ChildMsg childCmd)
                            else
                                (Loading loadingData, Cmd.none)
                (_, Err msg) ->
                    (Error (Debug.log "ApiResource received an HTTP error" msg), Cmd.none)
                (_, _) ->
                    (model, Cmd.none)
        ChildMsg msg ->
            case model of
                Loaded child ->
                    Base.mapFst Loaded <| Base.updateWith ChildMsg msg child
                _ ->
                    -- TODO: Log something here.
                    (model, Cmd.none)

subscriptions : Props data model msg -> Model data model msg -> Sub (Msg data msg)
subscriptions _ model =
    case model of
        Loaded child ->
            Base.subscriptionsWith ChildMsg child
        _ ->
            Sub.none

view : Props data model msg -> Model data model msg -> Html (Msg data msg)
view props resource =
    case resource of
        Unloaded ->
            props.unloadedView
        Loading _ ->
            props.loadingView
        Error error ->
            props.errorView error
        Loaded child ->
            Base.viewWith ChildMsg child
