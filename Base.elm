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

updateC : msg -> Component model msg -> (Component model msg, Cmd msg)
updateC msg {spec, state} =
    let
        (state', cmd) = spec.update msg state
    in
        ({ spec = spec, state = state' }, cmd)

subscriptionsC : Component model msg -> Sub msg
subscriptionsC { spec, state } =
    spec.subscriptions state

viewC : Component model msg -> Html msg
viewC { spec, state } =
    spec.view state

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
