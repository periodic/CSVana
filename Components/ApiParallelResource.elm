module Components.ApiParallelResource exposing (Props, Data, Msg, Instance, create)

import Array
import Html exposing (Html)
import Http

import Asana.Api exposing (ApiResult)
import Base
import Util

type alias Props data model msg =
    { child : List data -> (Base.Instance model msg, Cmd msg)
    , fetches : List (Cmd (ApiResult data))
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

max_retries : Int
max_retries = 3

type Fetch data
    = InProgress Int
    | Done data

isLoaded : Fetch a -> Bool
isLoaded fetch =
    case fetch of
        InProgress _ ->
            False
        Done _ ->
            True

getLoaded : List (Fetch a) -> List a
getLoaded fetches =
    case List.head fetches of
        Just (Done a) ->
            a :: (getLoaded <| List.drop 1 fetches)
        Just (InProgress _) ->
            getLoaded <| List.drop 1 fetches
        Nothing ->
            []

type Model data model msg
    = Loading (Array.Array (Fetch data))
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
    then child []
        |> Util.mapCmd ChildMsg
        |> Util.mapComponent Loaded
    else
        let
            model = Loading <| Array.repeat (List.length fetches) (InProgress 1)
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
                        loadingData = Array.set index (Done datum) data
                    in
                        if List.all isLoaded <| Array.toList loadingData
                        then 
                            let
                                loadedData = getLoaded <| Array.toList loadingData
                                (child, childCmd) = props.child loadedData
                            in
                                (Loaded child, Cmd.map ChildMsg childCmd)
                        else
                            (Loading loadingData, Cmd.none)
                (Loading data, Err err) ->
                    let
                        fetch =
                            List.drop index props.fetches
                            |> List.head
                            |> Maybe.withDefault (Cmd.none)
                            |> Cmd.map (ApiMsg index)
                    in
                        case (Array.get index data, err) of
                            (Just (InProgress attempts), Http.BadStatus response) ->
                                if attempts < max_retries && response.status.code /= 404
                                then
                                    (Loading (Array.set index (InProgress (attempts + 1)) data), fetch)
                                else
                                    (Error (Debug.log "ApiResource received an HTTP error" err), Cmd.none)
                            (Just (InProgress attempts), Http.Timeout) ->
                                (Loading (Array.set index (InProgress (attempts + 1)) data), fetch)
                            (Just (InProgress attempts), Http.NetworkError) ->
                                (Loading (Array.set index (InProgress (attempts + 1)) data), fetch)
                            _ ->
                                (Error (Debug.log "ApiResource received an HTTP error" err), Cmd.none)
                (_, _) ->
                    (model, Cmd.none)
        ChildMsg msg ->
            case model of
                Loaded child ->
                    Base.updateWith ChildMsg msg child
                        |> Util.mapComponent Loaded
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
        Loading _ ->
            props.loadingView
        Error error ->
            props.errorView error
        Loaded child ->
            Base.viewWith ChildMsg child
