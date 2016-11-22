module Components.FieldMatcher exposing (Props, Model, Msg, component)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)

import Base exposing (..)
import Components.Asana.FieldOptions as FieldOptions
import Components.Asana.Model as Asana
import Components.Csv as Csv

type alias Props =
    { csvFields : List String
    , customFields : List Asana.CustomField
    }

type alias Model =
    { fieldOptions : FieldOptions.Model
    , fieldOptionsComponent : Component FieldOptions.Model FieldOptions.Msg
    }

type Msg 
    = FieldOptionsMsg FieldOptions.Msg

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
init {csvFields, customFields} =
    let
        fieldOptionsComponent =
            FieldOptions.component
                { customFields = customFields
                , numFields = List.length csvFields
                }
        (fieldOptions, fieldOptionsCmd) = fieldOptionsComponent.init
    in
        ({ fieldOptions = fieldOptions, fieldOptionsComponent = fieldOptionsComponent }, Cmd.map FieldOptionsMsg fieldOptionsCmd)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update _ msg model =
    case msg of
        FieldOptionsMsg msg' ->
            let
                (fieldOptions', fieldOptionsCmd) = model.fieldOptionsComponent.update msg' model.fieldOptions
            in
                ({ model | fieldOptions = fieldOptions' }, Cmd.map FieldOptionsMsg fieldOptionsCmd)

view : Props -> Model -> Html Msg
view { csvFields} { fieldOptions, fieldOptionsComponent } =
    div [ class "FieldMatcher" ]
        [ div [ class "FieldMatcher-csv" ]
            [ renderHeaders csvFields ]
        , div [ class "FieldMatcher-targets" ]
            [ Html.App.map FieldOptionsMsg <| fieldOptionsComponent.view fieldOptions ]
        ]

renderHeaders : List String -> Html Msg
renderHeaders headers =
    div [ class "CsvHeaders" ]
        (List.map renderHeader headers)

renderHeader : String -> Html Msg
renderHeader header =
    div [ class "CsvHeaders-header" ]
        [ text header ]
