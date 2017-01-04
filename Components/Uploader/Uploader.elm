module Components.Uploader.Uploader exposing (Props, Msg, Data, Instance, create)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import String

import Asana.Api as Api
import Asana.Model as Asana
import Asana.Target as Target
import Asana.Urls as Urls
import Base exposing (..)

type alias Record = List String

type alias Props =
    { token : Api.Token
    , projectId : Asana.ProjectId
    , records : List Record
    , fieldTargets : List (Maybe Target.Target)
    }

type Msg =
    RecordProcessed Int (Api.ApiResult Asana.Task)

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

type alias Data =
    { totalRecords : Int
    , recordsProcessed : Int
    , errors : List Error
    }

type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update props
        , view = view props
        , subscriptions = always Sub.none
        , get = get props
        }

--------------------------------------------------------------------------------
-- Private

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
        (model_, cmds) =
            List.foldr
                (\(row, record) (model, cmds) ->
                    let
                        (model_, cmd) = uploadRecord props row record model
                    in
                        (model_, cmd :: cmds))
                (model, [])
                (List.indexedMap (,) props.records)
        cmd =
            Cmd.batch cmds
    in
        (model_, cmd)

uploadRecord : Props -> Int -> Record -> Model -> (Model, Cmd Msg)
uploadRecord props row record model =
    let
        fieldDefs = List.indexedMap (\i (t, r) -> (i, t, r)) <| List.map2 (,) props.fieldTargets record
        (newTask, errs) = List.foldr (updateTask row) (Target.emptyTask props.projectId, []) fieldDefs
        model_ = { model | errors = errs ++ model.errors }
        cmd = Cmd.map (RecordProcessed row) <| Api.createTask newTask props.token
    in
        (model_, cmd)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case Debug.log "Updater msg" msg of
        RecordProcessed row (Ok _) ->
            ({ model | recordsProcessed = model.recordsProcessed + 1 }, Cmd.none)
        RecordProcessed row (Err httpErr) ->
            ({ model | errors = UploadError { msg = toString httpErr, row = row } :: model.errors }, Cmd.none)

updateTask : Int -> (Int, Maybe Target.Target, String) -> (Asana.NewTask, List Error) -> (Asana.NewTask, List Error)
updateTask row (col, mTarget, value) (task, errors) =
    case mTarget of
        Just target -> 
            case Target.updateTask target value task of
                Ok task_ ->
                    (task_, errors)
                Err msg ->
                    (task, ParseError { msg = msg, row = row, col = col } :: errors)
        Nothing ->
            (task, errors)

view : Props -> Model -> Html Msg
view props model =
    div [ class "Uploader" ]
        [ if model.recordsProcessed == List.length props.records
                then viewComplete props
                else viewProgress props model
        , div [ class "Uploader-errors" ]
            (List.map viewError model.errors)
        ]

viewComplete : Props -> Html Msg
viewComplete { projectId } = 
    div [ class "Uploader-progress Uploader-progress--complete" ]
        [ text "Complete. You can view your "
        , a [ href (Urls.project projectId) ] [ text "new tasks in Asana" ]
        , text "."
        ]

viewProgress : Props -> Model -> Html Msg
viewProgress { records } { recordsProcessed } =
    div [ class "Uploader-progress Uploader-progress--working" ]
        [ text <| String.concat [ toString recordsProcessed, " / ", toString <| List.length records ]
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

get : Props -> Model -> Data
get { records } { recordsProcessed, errors } =
    { totalRecords = List.length records
    , recordsProcessed = recordsProcessed
    , errors = errors
    }
