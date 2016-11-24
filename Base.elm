module Base exposing (..)

import Html exposing (Html)
import Html.App

-- TODO: Bundle with the model, create easy functions.
-- create : Props -> Spec -> Component
-- update : Msg -> Component -> (Component, Cmd Msg)
-- view : Component -> Html Msg
type alias Spec model msg =
    { init : (model, Cmd msg)
    , update : msg -> model -> (model, Cmd msg)
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }

type alias Component model msg =
    { spec : Spec model msg
    , state : model
    }

initC : Spec model msg -> (Component model msg, Cmd msg)
initC spec =
    let
        (state, cmd) = spec.init
    in
        ({ spec = spec, state = state }, cmd)

initWrapped : (msg1 -> msg2) -> Spec model msg1 -> (Component model msg1, Cmd msg2)
initWrapped f = mapCmd f << initC

updateC : msg -> Component model msg -> (Component model msg, Cmd msg)
updateC msg {spec, state} =
    let
        (state', cmd) = spec.update msg state
    in
        ({ spec = spec, state = state' }, cmd)

updateWrapped : (msg1 -> msg2) -> msg1 -> Component model msg1 -> (Component model msg1, Cmd msg2)
updateWrapped f msg = mapCmd f << updateC msg

subscriptionsC : Component model msg -> Sub msg
subscriptionsC { spec, state } =
    spec.subscriptions state

subscriptionsWrapped : (msg1 -> msg2) -> Component model msg1 -> Sub msg2
subscriptionsWrapped f = Sub.map f << subscriptionsC

viewC : Component model msg -> Html msg
viewC { spec, state } =
    spec.view state

viewWrapped : (msg1 -> msg2) -> Component model msg1 -> Html msg2
viewWrapped f =
    Html.App.map f << viewC

-- TODO: This should probably be managed by something like an OutMsg.
stateC : Component model msg -> model
stateC { state } = state

mapFst : (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) =
    (f a, c)

mapSnd : (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) =
    (c, f a)

mapCmd : (a -> b) -> (c, Cmd a) -> (c, Cmd b)
mapCmd f = mapSnd (Cmd.map f)
