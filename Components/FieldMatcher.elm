module Components.FieldMatcher exposing (Props, Model, Msg, component)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Base exposing (..)
import Components.Asana.FieldOptions as FieldOptions
import Components.Asana.Model as Asana
import Components.Asana.Api as Api
import Components.Csv as Csv
import Components.Uploader as Uploader

type alias Props =
    { token : Api.Token
    , projectId : Asana.ProjectId
    , csvHeaders : List String
    , csvRecords : List (List String)
    , customFields : List Asana.CustomField
    }

type alias Model =
    { fieldOptions : FieldOptions.Model
    , fieldOptionsComponent : Component FieldOptions.Model FieldOptions.Msg
    , uploader : Maybe (Component Uploader.Model Uploader.Msg, Uploader.Model)
    }

type Msg 
    = FieldOptionsMsg FieldOptions.Msg
    | UploaderMsg Uploader.Msg
    | StartUpload

component : Props -> Component Model Msg
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
        fieldOptionsComponent =
            FieldOptions.component
                { customFields = customFields
                , numFields = List.length csvHeaders
                }
        (fieldOptions, fieldOptionsCmd) = fieldOptionsComponent.init
        model =
            { fieldOptions = fieldOptions
            , fieldOptionsComponent = fieldOptionsComponent
            , uploader = Nothing
            }
    in
        (model, Cmd.map FieldOptionsMsg fieldOptionsCmd)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case msg of
        FieldOptionsMsg msg' ->
            let
                (fieldOptions', fieldOptionsCmd) = model.fieldOptionsComponent.update msg' model.fieldOptions
            in
                ({ model | fieldOptions = fieldOptions' }, Cmd.map FieldOptionsMsg fieldOptionsCmd)
        UploaderMsg msg' ->
            case model.uploader of
                Just (uploaderComponent, uploaderModel) ->
                    let
                        (uploaderModel', uploaderCmd) = uploaderComponent.update msg' uploaderModel
                    in
                        ({ model | uploader = Just (uploaderComponent, uploaderModel') }, Cmd.map UploaderMsg uploaderCmd)
                Nothing ->
                    (model, Cmd.none)
        StartUpload ->
            let
                uploaderComponent = Uploader.component
                    { token = props.token
                    , projectId = props.projectId
                    , records = props.csvRecords
                    , fieldTargets = FieldOptions.getTargets model.fieldOptions
                    }
                (uploader, uploaderCmd) = uploaderComponent.init
                model' = { model | uploader = Just (uploaderComponent, uploader) }
                cmd = Cmd.map UploaderMsg uploaderCmd
            in
                (model', cmd)

view : Props -> Model -> Html Msg
view { csvHeaders} { fieldOptions, fieldOptionsComponent, uploader } =
    div [ class "FieldMatcher" ]
        [ div [ class "FieldMatcher-fields" ]
            [ div [ class "FieldMatcher-csv" ]
                [ renderHeaders csvHeaders ]
            , div [ class "FieldMatcher-targets" ]
                [ Html.App.map FieldOptionsMsg <| fieldOptionsComponent.view fieldOptions ]
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

renderUploader : Maybe (Component Uploader.Model Uploader.Msg, Uploader.Model) -> Html Msg
renderUploader mUploader =
    case mUploader of
        Just (component, model) ->
            Html.App.map UploaderMsg <| component.view model
        Nothing ->
            text "Click to start the import."
