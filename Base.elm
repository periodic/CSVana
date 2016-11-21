module Base exposing (..)

import Html exposing (Html)

type alias Component model msg =
    { init : (model, Cmd msg)
    , update : msg -> model -> (model, Cmd msg)
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }

mapFst : (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) =
    (f a, c)

mapSnd : (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) =
    (c, f a)
