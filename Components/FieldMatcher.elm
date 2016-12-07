module Components.FieldMatcher exposing (Props, Msg, Data, Instance, create)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Asana.Api as Api
import Asana.Model as Asana
import Base
import Components.FieldOptions as FieldOptions
import Components.Uploader as Uploader

type alias Props =
    { projectId : Asana.ProjectId
    , csvHeaders : List String
    , csvRecords : List (List String)
    , customFields : List Asana.CustomFieldInfo
    , apiContext : Api.Context
    }

type Msg
    = FieldOptionsMsg FieldOptions.Msg
    | UploaderMsg Uploader.Msg
    | StartUpload

type alias Data = ()
type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update props
        , view = view props
        , subscriptions = always Sub.none
        , get = always ()
        }

--------------------------------------------------------------------------------
-- Private

type alias Model =
    { fieldOptions : FieldOptions.Instance
    , uploader : Maybe Uploader.Instance
    }

init : Props -> (Model, Cmd Msg)
init {csvHeaders, csvRecords, customFields, apiContext} =
    let
        (fieldOptions, fieldOptionsCmd) =
            FieldOptions.create
                { customFields = customFields
                , numFields = List.length csvHeaders
                , records = csvRecords
                , headers = csvHeaders
                , apiContext = apiContext
                }
        model =
            { fieldOptions = fieldOptions
            , uploader = Nothing
            }
    in
        (model, Cmd.map FieldOptionsMsg fieldOptionsCmd)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case msg of
        FieldOptionsMsg msg' ->
            let
                (fieldOptions', fieldOptionsCmd) = Base.update msg' model.fieldOptions
            in
                ({ model | fieldOptions = fieldOptions' }, Cmd.map FieldOptionsMsg fieldOptionsCmd)
        UploaderMsg msg' ->
            case model.uploader of
                Just uploader ->
                    let
                        (uploader', uploaderCmd) = Base.update msg' uploader
                    in
                        ({ model | uploader = Just uploader' }, Cmd.map UploaderMsg uploaderCmd)
                Nothing ->
                    (model, Cmd.none)
        StartUpload ->
            let
                (uploader, uploaderCmd) = Uploader.create
                    { token = props.apiContext.token
                    , projectId = props.projectId
                    , records = props.csvRecords
                    , fieldTargets = Base.get model.fieldOptions
                    }
            in
                ({ model | uploader = Just uploader }, Cmd.map UploaderMsg uploaderCmd)

view : Props -> Model -> Html Msg
view { csvHeaders} { fieldOptions, uploader } =
    div [ class "FieldMatcher" ]
        [ div [ class "FieldMatcher-fields" ]
            [ div [ class "FieldMatcher-options" ]
                [ Base.viewWith FieldOptionsMsg fieldOptions ]
            ]
        , div [ class "FieldMatcher-upload" ]
            [ renderUploader ]
        ]

renderHeaders : List String -> Html Msg
renderHeaders headers =
    div [ class "CsvHeaders" ]
        (List.map renderHeader headers)

renderHeader : String -> Html Msg
renderHeader header =
    div [ class "CsvHeaders-header" ]
        [ text header ]

renderUploader : Maybe Uploader.Instance -> Html Msg
renderUploader mUploader =
    case mUploader of
        Just uploader ->
            Html.App.map UploaderMsg <| Base.view uploader
        Nothing ->
            div [ class "FieldMatcher-button" ]
                [ button [ onClick StartUpload, class "button primary" ] [ text "Import" ] ]
