module Components.TargetConfig exposing (Props, Msg, Data, Instance, create)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Set exposing (Set)

import Base
import Asana.Model as Asana


type alias Props data msg =
    { buttonText : String
    , defaultMap : String -> Maybe data
    , dataView : Maybe data -> (Base.Instance (Maybe data) msg, Cmd msg)
    , records : Set String -- Single column
    }

type Msg msg
    = ChildMsg Int msg
    | OpenPopup
    | ClosePopup

type alias Data data = Dict String data
type alias Instance data msg = Base.Instance (Data data) (Msg msg)

create : Props data msg -> (Instance data msg, Cmd (Msg msg))
create props =
    Base.create
        { init = init props
        , update = update
        , subscriptions = subscriptions
        , view = view props
        , get = get props
        }

--------------------------------------------------------------------------------
-- Private

type alias Model data msg =
    { views : Array (Base.Instance (Maybe data) msg)
    , isOpen : Bool
    }

init : Props data msg-> (Model data msg, Cmd (Msg msg))
init { defaultMap, dataView, records } =
    let
        mappedRecords = List.map defaultMap <| Set.toList records
        (views, cmds) = List.foldr (\value (views, cmds) ->
            let 
                (view, cmd) = dataView value
            in
                (view :: views, cmd :: cmds)) ([], []) mappedRecords
        cmd = Cmd.batch <| List.indexedMap (Cmd.map << ChildMsg) cmds
    in
        ({ views = Array.fromList views, isOpen = False }, cmd)

update : Msg msg -> Model data msg -> (Model data msg, Cmd (Msg msg))
update msg model =
    case msg of
        ChildMsg index msg' ->
            case Array.get index model.views of
                Just child ->
                    let
                        (child', cmd) = Base.updateWith (ChildMsg index) msg' child
                    in
                        ({ model | views = Array.set index child' model.views }, cmd)
                Nothing ->
                    (model, Cmd.none)
        OpenPopup ->
            ({ model | isOpen = True }, Cmd.none)
        ClosePopup ->
            ({ model | isOpen = False }, Cmd.none)

subscriptions : Model data msg -> Sub (Msg msg)
subscriptions { views } = 
    Array.toList views |> List.indexedMap (ChildMsg >> Base.subscriptionsWith) |> Sub.batch

view : Props data msg -> Model data msg -> Html (Msg msg)
view { buttonText, records } { views, isOpen } =
    div [ class "TargetConfig" ]
        (if isOpen
            then [ openButton buttonText, popupView records views ]
            else [ openButton buttonText ])

openButton : String -> Html (Msg msg)
openButton buttonText =
    a [ class "TargetConfig-openButton", Events.onClick OpenPopup ] [ text buttonText ]

closeButton : Html (Msg msg)
closeButton =
    a [ class "TargetConfig-closeButton", Events.onClick ClosePopup ] [ text "x" ]


popupView : Set String -> Array (Base.Instance (Maybe data) msg) -> Html (Msg msg)
popupView records views =
    div [ class "TargetConfig-popup" ]
        [ closeButton
        , div [ class "TargetConfig-recordViews" ]
            -- Note: Set.toList creates a sorted list.
            (List.indexedMap (,) (Set.toList records) |> List.map (uncurry (recordView views)))
        ]

recordView : Array (Base.Instance (Maybe data) msg) -> Int -> String -> Html (Msg msg)
recordView views index record =
    case Array.get index views of
        Just view ->
            div [ class "TargetConfig-recordView" ]
                [ div [ class "TargetConfig-recordView-record" ]
                    [ text record ]
                , div [ class "TargetConfig-recordView-view" ]
                    [ Base.viewWith (ChildMsg index) view ]
                ]
        Nothing ->
            Debug.crash "Attempting to render a record for which there is no view."

get : Props data msg -> Model data msg -> Dict String data
get { records } { views } =
    let 
        recordViewPairs = List.map2 (,) (Set.toList records) (Array.toList views)
        validMappings = List.filterMap (\(record, view) ->
            case Base.get view of
                Just value ->
                    Just (record, value)
                Nothing ->
                    Nothing) recordViewPairs
    in
        Dict.fromList validMappings
