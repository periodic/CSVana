module Components.Asana exposing (Props, Msg, Model, component)

import Html exposing (Html, div)
import Html.App
import Html.Attributes exposing (class)

import Base exposing (..)
import Components.Asana.Api as Api
import Components.Asana.ApiResource as ApiResource
import Components.Asana.Form as Form
import Components.Asana.Model as Asana
import Components.Asana.ProjectLoader as ProjectLoader
import Components.Asana.UserLoader as UserLoader
import Components.Csv as Csv
import Components.FieldMatcher as FieldMatcher

type alias Props =
    { token : Api.Token
    }

type Msg
    = FormMsg (UserLoader.Msg Form.Msg)
    | CsvMsg Csv.Msg
    | FieldMatcherMsg (ProjectLoader.Msg FieldMatcher.Msg)

type alias Model =
    { form : UserLoader.Model Form.Model Form.Msg
    , formComponent : Component (UserLoader.Model Form.Model Form.Msg) (UserLoader.Msg Form.Msg)
    , csv : Csv.Model
    , fieldMatcher : Maybe
        ( ProjectLoader.Model FieldMatcher.Model FieldMatcher.Msg
        , Component (ProjectLoader.Model FieldMatcher.Model FieldMatcher.Msg) (ProjectLoader.Msg FieldMatcher.Msg)
        )
    }

component : Props -> Component Model Msg
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = subscriptions props
    }

init : Props -> (Model, Cmd Msg)
init {token} =
    let
        form user =
            Form.component
                { token = token
                , user = user
                }
        userLoaderComponent =
            UserLoader.component
                { childComponent = form
                , token = token
                }
        (userLoader, userLoaderCmd) = userLoaderComponent.init
        (csv, csvCmd) =
            Csv.init {}
        cmd = Cmd.batch
            [ Cmd.map FormMsg userLoaderCmd
            , Cmd.map CsvMsg csvCmd
            ]
    in
        ({ form = userLoader
        , formComponent = userLoaderComponent
        , csv = csv
        , fieldMatcher = Nothing
        }, cmd)

subscriptions : Props -> Model -> Sub Msg
subscriptions _ { form, formComponent, csv } =
    let
        formSubs = Sub.map FormMsg <| formComponent.subscriptions form
        csvSubs = Sub.map CsvMsg <| Csv.subscriptions {} csv
    in
        Sub.batch [formSubs, csvSubs]

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update =
    processMessage


view : Props -> Model -> Html Msg
view props model =
    div [ class "Main" ]
        [ viewInputs props model
        , viewMatcher props model
        ]

--------------------------------------------------------------------------------
-- Private

processMessage : Props -> Msg -> Model -> (Model, Cmd Msg)
processMessage props msg model =
    case msg of
        FormMsg msg' ->
            let
                (form', formCmd) =
                     -- TODO: Bind the props in init.
                     model.formComponent.update msg' model.form
                model' = { model | form = form' }
                cmd = Cmd.map FormMsg formCmd
                project = getSelectedProject model
                project' = getSelectedProject model'
            in
                if (Debug.log "Old Project" project) /= (Debug.log "New project" project')
                    then updateMatcher props (model', cmd)
                    else (model', cmd)
        CsvMsg msg' ->
            let
                (csv', csvCmd) = Csv.update {} msg' model.csv
                model' = { model | csv = csv' }
                cmd = Cmd.map CsvMsg csvCmd
                headers = Csv.getHeaders model.csv
                headers' = Csv.getHeaders csv'
            in
                if (Debug.log "Old headers" headers) /= (Debug.log "New headers" headers')
                    then updateMatcher props (model', cmd)
                    else (model', cmd)
        FieldMatcherMsg msg' ->
            case model.fieldMatcher of
                Just (matcher, matcherComponent) ->
                    let
                        (matcher', matcherCmd) = matcherComponent.update msg' matcher
                        cmd = Cmd.map FieldMatcherMsg matcherCmd
                    in
                        ({ model | fieldMatcher = Just (matcher', matcherComponent) }, cmd)
                Nothing ->
                    (model, Cmd.none)

updateMatcher : Props -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMatcher {token} (model, cmd) =
    case ( getSelectedProject model, Csv.getHeaders model.csv, Csv.getRecords model.csv) of
        (Just project, Just headers, Just records) ->
            let
                matcherComponent =
                    ProjectLoader.component
                        { token = token
                        , childComponent = \project ->
                            let
                                customFields = List.map .customField project.customFieldSettings
                                numFields = List.length headers
                            in
                                FieldMatcher.component
                                    { token = token
                                    , projectId = project.id
                                    , csvHeaders = headers
                                    , csvRecords = records
                                    , customFields = customFields
                                    }
                        }
                (matcher, matcherCmd1) = matcherComponent.init
                (matcher', matcherCmd2) = ProjectLoader.load project.id token matcher
                cmd = Cmd.batch <| List.map (Cmd.map FieldMatcherMsg) [matcherCmd1, matcherCmd2]
            in
                ({ model | fieldMatcher = Just (matcher', matcherComponent) }, cmd)
        _ ->
            ({ model | fieldMatcher = Nothing }, Cmd.none)


getSelectedProject : Model -> Maybe Asana.ProjectResource
getSelectedProject model =
        UserLoader.getChild model.form
        `Maybe.andThen` Form.getSelectedProject


viewInputs props model =
    div [ class "Main-inputs" ]
        [ div [ class "Main-form" ]
            [ Html.App.map FormMsg <| model.formComponent.view model.form ]
        , div [ class "Main-csv" ]
            [ Html.App.map CsvMsg <| Csv.view {} model.csv ]
        ]

viewMatcher props { fieldMatcher } =
    case fieldMatcher of
        Just (model, component) ->
            div [ class "Main-matcher" ]
                [ Html.App.map FieldMatcherMsg <| component.view model ]
        Nothing ->
            div [ class "Main-matcher--disabled" ] []
