module Components.FieldMatcher exposing (Props, Msg, Data, Instance, create)

import Html exposing (..)
import Html.Attributes exposing (..)

import Asana.Api as Api
import Asana.Model as Asana
import Asana.Target as Target
import Base
import Components.FieldOptions as FieldOptions

type alias Props =
    { projectId : Asana.ProjectId
    , csvHeaders : List String
    , csvRecords : List (List String)
    , customFields : List Asana.CustomFieldInfo
    , apiContext : Api.Context
    }

type Msg
    = FieldOptionsMsg FieldOptions.Msg

type alias Data = FieldOptions.Data
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
    { fieldOptions : FieldOptions.Instance
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
            }
    in
        (model, Cmd.map FieldOptionsMsg fieldOptionsCmd)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case msg of
        FieldOptionsMsg msg_ ->
            let
                (fieldOptions_, fieldOptionsCmd) = Base.update msg_ model.fieldOptions
            in
                ({ model | fieldOptions = fieldOptions_ }, Cmd.map FieldOptionsMsg fieldOptionsCmd)

view : Props -> Model -> Html Msg
view { csvHeaders} { fieldOptions } =
    div [ class "FieldMatcher" ]
        [ div [ class "FieldMatcher-fields" ]
            [ div [ class "FieldMatcher-options" ]
                [ Base.viewWith FieldOptionsMsg fieldOptions ]
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

get : Props -> Model -> List (Maybe Target.Target)
get _ { fieldOptions } =
    Base.get fieldOptions
