module Components.Uploader.Form exposing (Props, Msg, Data, Instance, create)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Asana.Api as Api
import Asana.Model as Asana
import Asana.Target as Target
import Base
import Components.Uploader.Uploader as Uploader
import Util

type alias Props =
    { projectId : Asana.ProjectId
    , records : List (List String)
    , fieldTargets : List (Maybe Target.Target)
    , apiContext : Api.Context
    }

type Msg
    = UploaderMsg Uploader.Msg
    | StartUpload

type alias Data = ()
type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update props
        , view = view props
        , subscriptions = subscriptions props
        , get = always ()
        }

--------------------------------------------------------------------------------
-- Private

type alias Model =
    Maybe Uploader.Instance

init : Props -> (Model, Cmd Msg)
init _ =
    (Nothing, Cmd.none)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case msg of
        UploaderMsg msg_ ->
            case model of
                Just uploader ->
                    Base.updateWith UploaderMsg msg_ uploader
                        |> Util.mapComponent Just
                Nothing ->
                    (model, Cmd.none)
        StartUpload ->
            Uploader.create
                { token = props.apiContext.token
                , projectId = props.projectId
                , records = props.records
                , fieldTargets = props.fieldTargets
                }
                    |> Util.mapComponent Just
                    |> Util.mapCmd UploaderMsg

view : Props -> Model -> Html Msg
view _ model =
    case model of
        Just uploader ->
            Base.viewWith UploaderMsg uploader
        Nothing ->
            div [ class "UploaderForm-button" ]
                [ button [ onClick StartUpload, class "button primary" ] [ text "Start Import" ] ]

subscriptions : Props -> Model -> Sub Msg
subscriptions _ model =
    Maybe.map (Base.subscriptionsWith UploaderMsg) model
        |> Maybe.withDefault Sub.none
