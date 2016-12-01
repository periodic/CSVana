module Base exposing (..)

import Html exposing (Html)
import Html.App

type alias Program model msg =
    { init : (model, Cmd msg)
    , update : msg -> model -> (model, Cmd msg)
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }

type alias WithOutput model data a =
    { a | get : model -> data }

type alias Component model data msg =
    WithOutput model data (Program model msg)

type Instance data msg
    = Instance
        { update : msg -> (Instance data msg, Cmd msg)
        , view : Html msg
        , subscriptions : Sub msg
        , get : data
        }

createWithState : Component model data msg -> model -> Instance data msg
createWithState component state =
    Instance
        { update = mapFst (createWithState component) << flip component.update state
        , view = component.view state
        , subscriptions = component.subscriptions state
        , get = component.get state
        }

create : Component model data msg -> (Instance data msg, Cmd msg)
create component =
    let
        (state, cmd) = component.init
        instance = createWithState component state
    in
        (instance, cmd)

update : msg -> Instance model msg -> (Instance model msg, Cmd msg)
update msg (Instance { update }) =
    update msg

updateWith : (msg1 -> msg2) -> msg1 -> Instance model msg1 -> (Instance model msg1, Cmd msg2)
updateWith f msg =
    update msg >> mapCmd f

subscriptions : Instance model msg -> Sub msg
subscriptions (Instance { subscriptions }) =
    subscriptions

subscriptionsWith : (msg1 -> msg2) -> Instance model msg1 -> Sub msg2
subscriptionsWith f =
    subscriptions >> Sub.map f

view : Instance model msg -> Html msg
view (Instance { view }) =
    view

viewWith : (msg1 -> msg2) -> Instance model msg1 -> Html msg2
viewWith f =
    view >> Html.App.map f

get : Instance data msg -> data
get (Instance { get }) =
    get

asRoot : (Instance data msg, Cmd msg) -> Program (Instance data msg) msg
asRoot (instance, cmd) =
    { init = (instance, cmd)
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- Utility
-- TODO: move to it's own file.

mapFst : (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) =
    (f a, c)

mapSnd : (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) =
    (c, f a)

pairMap : (a -> c) -> (b -> d) -> (a, b) -> (c, d)
pairMap f g (a, b) =
    (f a, g b)

mapCmd : (a -> b) -> (c, Cmd a) -> (c, Cmd b)
mapCmd f = mapSnd (Cmd.map f)

isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True
        Nothing ->
            False
