module Components.Configs.CompletedConfig exposing (Props, Msg, Data, Instance, create)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Json

import Base

type alias Props =
    { value : Bool
    }

type Msg
    = NewValue Bool

type alias Data = Bool

type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update
        , subscriptions = always Sub.none
        , view = view
        , get = identity
        }

--------------------------------------------------------------------------------
-- Private

type alias Model = Bool

init : Props -> (Model, Cmd Msg)
init { value } =
    (value, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewValue value ->
            (value, Cmd.none)

view : Model -> Html Msg
view val =
    div [ class "CompletedConfig" ]
        [ select [ class "CompletedConfig-select", Events.on "change" onChange ]
            [ option [ value "true", selected val] [ text "Complete" ]
            , option [ value "false", selected (not val) ] [ text "Incomplete" ]
            ]
        ]

onChange : Json.Decoder Msg
onChange =
    Json.map ((==) "true" >> NewValue) <| Json.at ["target", "value"] Json.string


