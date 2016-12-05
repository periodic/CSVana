module Components.Configs.UserConfig exposing (Props, Msg, Data, Instance, create)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Json

import Base
import Asana.Model as Asana
import Asana.Api as Api
import CommonViews
import Components.Configs.UserInfo as UserInfo
import Components.Typeahead as Typeahead

type alias Props =
    { selectedUser : Maybe Asana.User
    , token : Api.Token
    , workspaceId : Asana.WorkspaceId
    }

type Msg
    = Selection (Maybe Asana.User)
    | TypeaheadMsg (Typeahead.Msg Asana.User)

type alias Data = Maybe Asana.UserId

type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update props
        , subscriptions = subscriptions
        , view = view props
        , get = get
        }

--------------------------------------------------------------------------------
-- Private

type Model
    = Unselected (Typeahead.Instance Asana.User)
    | Selected Asana.User (UserInfo.Instance Msg)

init : Props -> (Model, Cmd Msg)
init props =
    makeModel props

makeModel : Props -> (Model, Cmd Msg)
makeModel { token, workspaceId, selectedUser } =
    case selectedUser of
        Just user ->
            UserInfo.create { user = user }
                |> Base.mapFst (Selected user)
        Nothing ->
            Typeahead.create { fetcher = flip (Api.userTypeahead workspaceId) token }
                |> Base.pairMap Unselected (Cmd.map TypeaheadMsg)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case msg of
        Selection selectedUser ->
            makeModel { props | selectedUser = selectedUser }
        TypeaheadMsg msg' ->
            case model of
                Unselected ta ->
                    Base.updateWith TypeaheadMsg msg' ta |> Base.mapFst Unselected
                _ ->
                    (model, Cmd.none)

view : Props -> Model -> Html Msg
view props model =
    div [ class "UserConfig" ]
        [ case model of
            Unselected typeahead ->
                Base.viewWith TypeaheadMsg typeahead
            Selected _ userInfo ->
                Base.view userInfo
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Unselected typeahead ->
                Base.subscriptionsWith TypeaheadMsg typeahead
        Selected _ userInfo ->
                Base.subscriptions userInfo

get : Model -> Data
get model =
    case model of
        Unselected _ ->
            Nothing
        Selected user _ ->
            Just user.id

