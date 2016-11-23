module Components.FieldMatcher exposing (Props, Model, Msg, component)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Base
import Components.Asana.FieldOptions as FieldOptions
import Components.Asana.Model as Asana
import Components.Asana.Api as Api
import Components.Uploader as Uploader

type alias Props =
    { token : Api.Token
    , projectId : Asana.ProjectId
    , csvHeaders : List String
    , csvRecords : List (List String)
    , customFields : List Asana.CustomField
    }

type alias Model =
    { fieldOptions : FieldOptions.Component
    , uploader : Maybe Uploader.Component
    }

type Msg 
    = FieldOptionsMsg FieldOptions.Msg
    | UploaderMsg Uploader.Msg
    | StartUpload

type alias Spec = Base.Spec Model Msg
type alias Component = Base.Component Model Msg

component : Props -> Spec
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = always Sub.none
    }

--------------------------------------------------------------------------------
-- Private

init : Props -> (Model, Cmd Msg)
init {csvHeaders, customFields} =
    let
        (fieldOptions, fieldOptionsCmd) =
            Base.initC <| FieldOptions.component
                { customFields = customFields
                , numFields = List.length csvHeaders
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
                (fieldOptions', fieldOptionsCmd) = Base.updateC msg' model.fieldOptions
            in
                ({ model | fieldOptions = fieldOptions' }, Cmd.map FieldOptionsMsg fieldOptionsCmd)
        UploaderMsg msg' ->
            case model.uploader of
                Just uploader ->
                    let
                        (uploader', uploaderCmd) = Base.updateC msg' uploader
                    in
                        ({ model | uploader = Just uploader' }, Cmd.map UploaderMsg uploaderCmd)
                Nothing ->
                    (model, Cmd.none)
        StartUpload ->
            let
                (uploader, uploaderCmd) = Base.initC <| Uploader.component
                    { token = props.token
                    , projectId = props.projectId
                    , records = props.csvRecords
                    , fieldTargets = FieldOptions.getTargets model.fieldOptions
                    }
            in
                ({ model | uploader = Just uploader }, Cmd.map UploaderMsg uploaderCmd)

view : Props -> Model -> Html Msg
view { csvHeaders} { fieldOptions, uploader } =
    div [ class "FieldMatcher" ]
        [ div [ class "FieldMatcher-fields" ]
            [ div [ class "FieldMatcher-csv" ]
                [ renderHeaders csvHeaders ]
            , div [ class "FieldMatcher-targets" ]
                [ Html.App.map FieldOptionsMsg <| Base.viewC fieldOptions ]
            ]
        , div [ class "FieldMatcher-upload" ]
            [ div [ class "FieldMatcher-button" ]
                [ button [ onClick StartUpload ] [ text "Import" ] ]
            , div [ class "FieldMatcher-progress" ]
                [ renderUploader uploader ]
            ]
        ]

renderHeaders : List String -> Html Msg
renderHeaders headers =
    div [ class "CsvHeaders" ]
        (List.map renderHeader headers)

renderHeader : String -> Html Msg
renderHeader header =
    div [ class "CsvHeaders-header" ]
        [ text header ]

renderUploader : Maybe Uploader.Component -> Html Msg
renderUploader mUploader =
    case mUploader of
        Just uploader ->
            Html.App.map UploaderMsg <| Base.viewC uploader
        Nothing ->
            text "Click to start the import."
