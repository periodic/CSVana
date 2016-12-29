module Components.FormSection exposing (Props, Data, Msg, Instance, create)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, value, type_, classList, disabled)

import Base
import Util

type alias Props a msg1 msg2 =
    { incompleteChild : Maybe a -> (Base.Instance (Maybe a) msg1, Cmd msg1)
    , completeChild : a -> (Base.Instance () msg2, Cmd msg2)
    , value : Maybe a
    }

type Msg msg1 msg2
    = IncompleteMsg msg1
    | ReconfigureMsg msg1
    | CompleteMsg msg2
    | Reconfigure
    | Cancel
    | Submit

type alias Data a = Maybe a
type alias Instance a msg1 msg2 = Base.Instance (Data a) (Msg msg1 msg2)

create : Props a msg1 msg2 -> (Instance a msg1 msg2, Cmd (Msg msg1 msg2))
create props =
    Base.create
        { init = init props
        , update = update props
        , view = view props
        , subscriptions = subscriptions props
        , get = get
        }

--------------------------------------------------------------------------------
-- Private

type Model a msg1 msg2
    = Incomplete (Base.Instance (Maybe a) msg1)
    | Reconfiguring a (Base.Instance (Maybe a) msg1)
    | Complete a (Base.Instance () msg2)

init : Props a msg1 msg2 -> (Model a msg1 msg2, Cmd (Msg msg1 msg2))
init { value, incompleteChild, completeChild } =
    case value of
        Just a ->
            completeChild a
                |> Util.mapComponent (Complete a)
                |> Util.mapCmd CompleteMsg
        Nothing ->
            incompleteChild Nothing
                |> Util.mapComponent Incomplete
                |> Util.mapCmd IncompleteMsg

update : Props a msg1 msg2
   -> Msg msg1 msg2
   -> Model a msg1 msg2
   -> (Model a msg1 msg2, Cmd (Msg msg1 msg2))
update { incompleteChild, completeChild } msg model =
    case (msg, model) of
        (IncompleteMsg msg_, Incomplete inst) ->
            Base.update msg_ inst
                |> Util.mapComponent Incomplete
                |> Util.mapCmd IncompleteMsg
        (ReconfigureMsg msg_, Reconfiguring val inst) ->
            Base.update msg_ inst
                |> Util.mapComponent (Reconfiguring val)
                |> Util.mapCmd ReconfigureMsg
        (CompleteMsg msg_, Complete val inst) ->
            Base.update msg_ inst
                |> Util.mapComponent (Complete val)
                |> Util.mapCmd CompleteMsg
        (Submit, Incomplete inst) ->
            case Base.get inst of
                Just val ->
                    completeChild val
                        |> Util.mapComponent (Complete val)
                        |> Util.mapCmd CompleteMsg
                Nothing ->
                    (model, Cmd.none)
        (Submit, Reconfiguring _ inst) ->
            case Base.get inst of
                Just val ->
                    completeChild val
                        |> Util.mapComponent (Complete val)
                        |> Util.mapCmd CompleteMsg
                Nothing ->
                    (model, Cmd.none)
        (Cancel, Reconfiguring val _) ->
            completeChild val
                |> Util.mapComponent (Complete val)
                |> Util.mapCmd CompleteMsg
        (Reconfigure, Complete val _) ->
            incompleteChild (Just val)
                |> Util.mapComponent (Reconfiguring val)
                |> Util.mapCmd ReconfigureMsg
        _ ->
            (model, Cmd.none)


view : Props a msg1 msg2 -> Model a msg1 msg2 -> Html (Msg msg1 msg2)
view _ model =
    case model of
        Incomplete inst ->
            viewIncomplete inst
        Reconfiguring _ inst ->
            viewReconfigure inst
        Complete _ inst ->
            viewComplete inst

viewIncomplete : Base.Instance (Maybe a) msg1 -> Html (Msg msg1 msg2)
viewIncomplete inst =
    div [ class "FormSection FormSection--incomplete" ]
        [ div [ class "FormSection-content" ] [ Base.viewWith IncompleteMsg inst ]
        , button
            [ value "Submit"
            , onClick Submit
            , classList [ ("disabled", Util.isNothing (Base.get inst)) ]
            , disabled (Util.isNothing (Base.get inst))
            ]
            [ text "Submit" ]
        ]

viewReconfigure : Base.Instance (Maybe a) msg1 -> Html (Msg msg1 msg2)
viewReconfigure inst =
    div [ class "FormSection FormSection--incomplete" ]
        [ div [ class "FormSection-content" ] [ Base.viewWith ReconfigureMsg inst ]
        , button
            [ value "Submit"
            , onClick Submit
            , classList [ ("disabled", Util.isNothing (Base.get inst)) ]
            , disabled (Util.isNothing (Base.get inst))
            ]
            [ text "Submit" ]
        , button
            [ value "Cancel"
            , onClick Cancel
            ]
            [ text "Cancel" ]
        ]

viewComplete : Base.Instance a msg2 -> Html (Msg msg1 msg2)
viewComplete inst =
    div [ class "FormSection FormSection--complete" ]
        [ div [ class "FormSection-content" ] [ Base.viewWith CompleteMsg inst ]
        , button [ value "Change" , onClick Reconfigure ] [ text "Change" ]
        ]

subscriptions : Props a msg1 msg2 -> Model a msg1 msg2 -> Sub (Msg msg1 msg2)
subscriptions _ model =
    case model of
        Incomplete inst ->
            Base.subscriptionsWith IncompleteMsg inst
        Reconfiguring _ inst ->
            Base.subscriptionsWith ReconfigureMsg inst
        Complete _ inst ->
            Base.subscriptionsWith CompleteMsg inst

get : Model a msg1 msg2 -> Maybe a 
get model =
    case model of
        Incomplete _ ->
            Nothing
        Reconfiguring val _ ->
            Just val
        Complete val _ ->
            Just val

