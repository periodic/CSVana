module Components.TargetConfig exposing (Props, Msg, Data, Instance, MapResult(..), create)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Set exposing (Set)

import Base
import CommonViews

type MapResult data
    = Value (Maybe data)
    | NeedsWork (Cmd (Maybe data))

type alias Props data msg =
    { defaultMap : String -> MapResult data
    , dataView : Maybe data -> (Base.Instance (Maybe data) msg, Cmd msg)
    , records : Set String -- Single column
    }

type Msg data msg
    = ChildMsg Int msg
    | WorkComplete Int (Maybe data)
    | OpenPopup
    | ClosePopup

type alias Data data = Dict String data
type alias Instance data msg = Base.Instance (Data data) (Msg data msg)

create : Props data msg -> (Instance data msg, Cmd (Msg data msg))
create props =
    Base.create
        { init = init props
        , update = update props
        , subscriptions = subscriptions
        , view = view props
        , get = get props
        }

--------------------------------------------------------------------------------
-- Private

type alias Model data msg =
    { views : Array (Maybe (Base.Instance (Maybe data) msg))
    , isOpen : Bool
    }

init : Props data msg-> (Model data msg, Cmd (Msg data msg))
init { defaultMap, dataView, records } =
    let
        mappedRecords = List.map defaultMap <| Set.toList records
        (views, cmds) =
            List.unzip
                <| flip List.indexedMap mappedRecords
                <| (\index result ->
                    case result of
                        Value data ->
                            dataView data |> Base.pairMap Just (Cmd.map <| ChildMsg index)
                        NeedsWork cmd ->
                            (Nothing, Cmd.map (WorkComplete index) cmd))
        cmd = Cmd.batch cmds
    in
        ({ views = Array.fromList views, isOpen = False }, cmd)

update : Props data msg -> Msg data msg -> Model data msg -> (Model data msg, Cmd (Msg data msg))
update { dataView } msg model =
    case msg of
        ChildMsg index msg' ->
            case Array.get index model.views of
                Just (Just child) ->
                    let
                        (child', cmd) = Base.updateWith (ChildMsg index) msg' child
                    in
                        ({ model | views = Array.set index (Just child') model.views }, cmd)
                Just Nothing ->
                    (model, Cmd.none)
                Nothing ->
                    (model, Cmd.none)
        WorkComplete index data ->
            let
                (child, cmd) = dataView <| Debug.log ("Data fetched for " ++ toString index) data
            in
                ({ model | views = Array.set index (Just child) model.views }, Cmd.map (ChildMsg index) cmd)
        OpenPopup ->
            ({ model | isOpen = True }, Cmd.none)
        ClosePopup ->
            ({ model | isOpen = False }, Cmd.none)

subscriptions : Model data msg -> Sub (Msg data msg)
subscriptions { views } =
    Array.toList views
        |> List.indexedMap (ChildMsg >> Base.subscriptionsWith >> Maybe.map)
        |> List.map (Maybe.withDefault Sub.none)
        |> Sub.batch

view : Props data msg -> Model data msg -> Html (Msg data msg)
view { records } { views, isOpen } =
    div [ class "TargetConfig" ]
        (if isOpen
            then [ CommonViews.configButton OpenPopup, popupView records views ]
            else [ CommonViews.configButton OpenPopup ])

popupView : Set String -> Array (Maybe (Base.Instance (Maybe data) msg)) -> Html (Msg data msg)
popupView records views =
    CommonViews.popup "Configure Data Mapping" ClosePopup
        <| div [ class "TargetConfig-content" ]
            [ div [ class "TargetConfig-recordViews" ]
                -- Note: Set.toList creates a sorted list.
                (List.indexedMap (,) (Set.toList records) |> List.map (uncurry (recordView views)))
            , div [ class "TargetConfig-actions" ]
                [ input [ type' "button"
                        , class "TargetConfig-closeButton button primary"
                        , value "Done"
                        , Events.onClick ClosePopup
                        ]
                        [ text "Done" ]
                ]
            ]

recordView : Array (Maybe (Base.Instance (Maybe data) msg)) -> Int -> String -> Html (Msg data msg)
recordView views index record =
    div [ class "TargetConfig-recordView" ]
        [ div [ class "TargetConfig-recordView-record" ]
            [ text record ]
        , div [ class "TargetConfig-recordView-view" ]
            [ case Array.get index views of
                Just (Just view) ->
                    Base.viewWith (ChildMsg index) view
                Just (Nothing) ->
                    CommonViews.loadingIndicator
                Nothing ->
                    Debug.crash "Attempting to render a record for which there is no view."
            ]
        ]

get : Props data msg -> Model data msg -> Dict String data
get { records } { views } =
    let
        recordViewPairs = List.map2 (,) (Set.toList records) (Array.toList views)
        validMappings = flip List.filterMap recordViewPairs
            <| \(record, view) ->
                        case Maybe.andThen view Base.get of
                            Just value ->
                                Just (record, value)
                            Nothing ->
                                Nothing
    in
        Dict.fromList validMappings
