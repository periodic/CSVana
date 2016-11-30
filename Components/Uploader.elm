module Components.Uploader exposing (Props, Msg, Component, spec)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import String

import Asana.Api as Api
import Asana.Model as Asana
import Asana.Target as Target
import Base exposing (..)

type alias Record = List String

type alias Props =
    { token : Api.Token
    , projectId : Asana.ProjectId
    , records : List Record
    , fieldTargets : List Target.Target
    }

type Msg =
    RecordProcessed Int (Api.ApiResult Asana.Task)

type alias Spec = Base.Spec Model Msg
type alias Component = Base.Component Model Msg

spec : Props -> Spec
spec props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = always Sub.none
    }

--------------------------------------------------------------------------------
-- Private

type Error
    = ParseError
        { msg : String
        , row : Int
        , col : Int
        }
    | UploadError
        { msg : String
        , row : Int
        }

type alias Model =
    { recordsProcessed : Int
    , errors : List Error
    }

init : Props -> (Model, Cmd Msg)
init props =
    let
        model =
            { recordsProcessed = 0
            , errors = []
            }
        (model', cmds) =
            List.foldr
                (\(row, record) (model, cmds) ->
                    let
                        (model', cmd) = uploadRecord props row record model
                    in
                        (model', cmd :: cmds))
                (model, [])
                (List.indexedMap (,) props.records)
        cmd =
            Cmd.batch cmds
    in
        (model', cmd)

uploadRecord : Props -> Int -> Record -> Model -> (Model, Cmd Msg)
uploadRecord props row record model =
    let
        fieldSpecs = List.indexedMap (\i (t, r) -> (i, t, r)) <| List.map2 (,) props.fieldTargets record
        (newTask, errs) = List.foldr (updateTask row) (Target.emptyTask props.projectId, []) fieldSpecs
        model' = { model | errors = errs ++ model.errors }
        cmd = Cmd.map (RecordProcessed row) <| Api.createTask newTask props.token
    in
        (model', cmd)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case Debug.log "Updater msg" msg of
        RecordProcessed row (Ok _) ->
            ({ model | recordsProcessed = model.recordsProcessed + 1 }, Cmd.none)
        RecordProcessed row (Err httpErr) ->
            ({ model | errors = UploadError { msg = toString httpErr, row = row } :: model.errors }, Cmd.none)

updateTask : Int -> (Int, Target.Target, String) -> (Asana.NewTask, List Error) -> (Asana.NewTask, List Error)
updateTask row (col, target, value) (task, errors) =
    case Target.updateTask target value task of
        Ok task' ->
            (task', errors)
        Err msg ->
            (task, ParseError { msg = msg, row = row, col = col } :: errors)

view : Props -> Model -> Html Msg
view { records } { recordsProcessed, errors } =
    div [ class "Uploader" ]
        [ div [ class "Uploader-progress" ]
            [ text <| String.concat [ toString recordsProcessed, " / ", toString <| List.length records ] ]
        , div [ class "Uploader-errors" ]
            (List.map viewError errors)
        ]

viewError : Error -> Html Msg
viewError error =
    case error of
        ParseError { msg, row, col } ->
            div [ class "Uploader-error" ]
                [ text <| "Row " ++ toString row ++ ", Col " ++ toString col ++ ": " ++ msg ]
        UploadError { msg, row } ->
            div [ class "Uploader-error" ]
                [ text <| "Row " ++ toString row ++ ": " ++ msg ]
