module Components.WorkspaceSelector exposing (Props, Msg, Instance, create)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json

import Base
import Asana.Model as Asana

type alias Props =
    { workspaces : List Asana.WorkspaceResource
    }

type Msg =
    Selected (Maybe Asana.WorkspaceId)

type alias Data = Maybe Asana.WorkspaceId
type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update
        , view = view props
        , subscriptions = always Sub.none
        , get = get
        }

--------------------------------------------------------------------------------
-- Private

type alias Model =
    Maybe Asana.WorkspaceId

init : Props -> (Model, Cmd Msg)
init { workspaces } =
    (Nothing, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Selected mSelected ->
            (mSelected, Cmd.none)

view : Props -> Model -> Html Msg
view { workspaces } selected =
    let
        attrs =
            [ class "WorkspaceSelector"
            , on "change" onChange
            ]
        options =
            option [value ""] [] :: List.map workspaceOption workspaces
    in
        select attrs options

get : Model -> Maybe Asana.WorkspaceId
get = identity

onChange : Json.Decoder Msg
onChange =
    let
        strToMaybe str =
            if str == ""
                then Nothing
                else Just str
    in
        Json.map Selected <| Json.map strToMaybe <| Json.at ["target", "value"] Json.string

workspaceOption : Asana.WorkspaceResource -> Html Msg
workspaceOption {id, name} =
    option [ value id ] [ text name ]
