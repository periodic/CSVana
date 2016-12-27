module Components.Clearable exposing (Props, Msg, Data, Instance, create)

import Base
import Html exposing (Html, div, a, text)
import Html.Attributes exposing (class)
import Html.Events as Events


type alias Props data msg1 msg2 =
    { value : Maybe data
    , emptyView : (Base.Instance (Maybe data) msg1, Cmd msg1)
    , fullView : data -> (Base.Instance () msg2, Cmd msg2)
    }

type Msg msg1 msg2
    = Clear
    | EmptyMsg msg1
    | FullMsg msg2

type Data data = Maybe data

type alias Instance data msg1 msg2 = Base.Instance (Data data) (Msg msg1 msg2)

-- create : Props data msg1 msg2 -> (Instance data msg1 msg2, Cmd (Msg msg1 msg2))
create props =
    Base.create
        { init = init props
        , update = update props
        , view = view props
        , subscriptions = subscriptions props
        , get = get props
        }

--------------------------------------------------------------------------------
-- Private

type Model data msg1 msg2
    = Full data (Base.Instance () msg2)
    | Empty (Base.Instance (Maybe data) msg1)

init : Props data msg1 msg2 -> (Model data msg1 msg2, Cmd (Msg msg1 msg2))
init props =
    case props.value of
        Just data ->
            props.fullView data
                |> Base.mapPair (Full data) (Cmd.map FullMsg)
        Nothing ->
            props.emptyView
                |> Base.mapPair Empty (Cmd.map EmptyMsg)

update : Props data msg1 msg2 -> Msg msg1 msg2 -> Model data msg1 msg2 -> (Model data msg1 msg2, Cmd (Msg msg1 msg2))
update props msg model =
    case (msg, model) of
        (EmptyMsg msg', Empty inst) ->
            let
                (emptyInst', emptyCmd) = Base.updateWith EmptyMsg msg' inst
            in
                case Base.get emptyInst' of
                    Just val ->
                        props.fullView val
                            |> Base.mapPair (Full val) (Cmd.map FullMsg)
                    Nothing ->
                        (Empty emptyInst', emptyCmd)
        (FullMsg msg', Full value inst) ->
            Base.updateWith FullMsg msg' inst
                |> Base.mapFirst (Full value)
        (Clear, Full _ inst) ->
            props.emptyView
                |> Base.mapPair Empty (Cmd.map EmptyMsg)
        _ ->
            (model, Cmd.none)

subscriptions : Props data msg1 msg2 -> Model data msg1 msg2 -> Sub (Msg msg1 msg2)
subscriptions props model =
    case model of
        Full _ inst ->
            Base.subscriptionsWith FullMsg inst
        Empty inst ->
            Base.subscriptionsWith EmptyMsg inst

view : Props data msg1 msg2 -> Model data msg1 msg2 -> Html (Msg msg1 msg2)
view props model =
    case model of
        Full _ inst->
            div [ class "Clearable" ]
                [ Base.viewWith FullMsg inst
                , a [ class "Clearable-clearButton", Events.onClick Clear ] [ text "x" ]
                ]
        Empty inst ->
            div [ class "Clearable" ]
                [ Base.viewWith EmptyMsg inst
                ]

get : Props data msg1 msg2 -> Model data msg1 msg2 -> Maybe data
get props model =
    case model of
        Full value _ ->
            Just value
        Empty _ ->
            Nothing
