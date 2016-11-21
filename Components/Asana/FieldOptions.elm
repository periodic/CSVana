module Components.Asana.FieldOptions exposing (Props, Model, Msg, component)

import Html exposing (Html, text)

import Base exposing (..)
import Components.Asana.Api as Api
import Components.Asana.Model as Asana
import Components.Asana.ApiResource as ApiResource

type alias Props =
    { customFields : List Asana.CustomField
    , maxValues : Int
    }

type alias Model =
    {}

type alias Msg =
    {}

component : Props -> Component Model Msg
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = always Sub.none
    }

init : Props -> (Model, Cmd Msg)
init _ =
    ({}, Cmd.none)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    (model, Cmd.none)

view : Props -> Model -> Html Msg
view props model =
    text <| toString (props, model)

