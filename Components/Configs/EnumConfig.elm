module Components.Configs.EnumConfig exposing (Props, Msg, Data, Instance, create)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Json

import Base
import Asana.Model as Asana
import Util

type alias Props =
    { selectedId : Maybe Asana.CustomFieldEnumValue
    , enumOptions : List Asana.CustomFieldEnumValue
    }

type Msg
    = NewValue (Maybe Asana.CustomFieldEnumValueId)

type alias Data = Maybe Asana.CustomFieldEnumValue

type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update props
        , subscriptions = always Sub.none
        , view = view props
        , get = identity
        }

--------------------------------------------------------------------------------
-- Private

type alias Model =
     Maybe Asana.CustomFieldEnumValue

init : Props -> (Model, Cmd Msg)
init { selectedId } =
    (selectedId, Cmd.none)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update { enumOptions } msg model =
    case msg of
        NewValue selectedId ->
            (selectedId `Maybe.andThen` \id -> Util.find (.id >> (==) id) enumOptions, Cmd.none)

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

emptyOption : Maybe Asana.CustomFieldEnumValue -> Html msg
emptyOption selectedValue =
    option [ value "", selected (selectedValue == Nothing) ]
        [ text "" ]

enumOption : Maybe Asana.CustomFieldEnumValue -> Asana.CustomFieldEnumValue -> Html msg
enumOption selectedValue enumValue =
    option [ value enumValue.id, selected (Just enumValue == selectedValue) ]
        [ text enumValue.name ]

