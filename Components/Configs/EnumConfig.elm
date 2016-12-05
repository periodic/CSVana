module Components.Configs.EnumConfig exposing (Props, Msg, Data, Instance, create)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Json

import Base
import Asana.Model as Asana

type alias Props =
    { selectedId : Maybe Asana.CustomFieldEnumValueId
    , enumOptions : List Asana.CustomFieldEnumValue
    }

type Msg
    = NewValue (Maybe Asana.CustomFieldEnumValueId)

type alias Data = Maybe Asana.CustomFieldEnumValueId

type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update
        , subscriptions = always Sub.none
        , view = view props
        , get = identity
        }

--------------------------------------------------------------------------------
-- Private

type alias Model =
     Maybe Asana.CustomFieldEnumValueId

init : Props -> (Model, Cmd Msg)
init { selectedId } =
    (selectedId, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewValue selectedId ->
            (selectedId, Cmd.none)

view : Props -> Model -> Html Msg
view props val =
    div [ class "EnumConfig" ]
        [ select [ class "EnumConfig-select", Events.on "change" onChange ]
            (emptyOption val :: List.map (enumOption val) props.enumOptions)
        ]

onChange : Json.Decoder Msg
onChange =
    Json.at ["target", "value"] Json.string
        |> Json.map (\id -> if id == "" then Nothing else Just id)
        |> Json.map NewValue

emptyOption : Maybe Asana.CustomFieldEnumValueId -> Html msg
emptyOption selectedId =
    option [ value "", selected (selectedId == Nothing) ]
        [ text "" ]

enumOption : Maybe Asana.CustomFieldEnumValueId -> Asana.CustomFieldEnumValue -> Html msg
enumOption selectedId enumValue =
    option [ value enumValue.id, selected (Just enumValue.id == selectedId) ]
        [ text enumValue.name ]

