module Components.TargetSelector exposing (Props, Msg, Data, Instance, create)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Json
import Set exposing (Set)
import String

import Asana.Model as Asana
import Asana.Target as Target exposing (Target)
import Base
import CommonViews
import Components.Configs.CompletedConfig as CompletedConfig
import Components.Configs.EnumConfig as EnumConfig
import Components.TargetConfig as TargetConfig
import Util exposing (..)

type alias Props =
    { customFields : List Asana.CustomFieldInfo
    , records : Set String
    }

type Msg
    = Selection String
    | CompletionMsg (TargetConfig.Msg CompletedConfig.Msg)
    | EnumMsg (TargetConfig.Msg EnumConfig.Msg)

type alias Data = Maybe Target
type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = (None, Cmd.none)
        , update = update props
        , subscriptions = always Sub.none
        , view = view props
        , get = get
        }

--------------------------------------------------------------------------------
-- Private

type Model
    = None
    | Name
    | Description
    | Completion (TargetConfig.Instance CompletedConfig.Data CompletedConfig.Msg)
    | DueDate
    | DueDateWithTime
    | CustomEnumField
        Asana.CustomFieldId
        String
        (List Asana.CustomFieldEnumValue)
        (TargetConfig.Instance Asana.CustomFieldEnumValueId EnumConfig.Msg)
    | CustomField Asana.CustomFieldInfo

get : Model -> Data
get model =
    case model of
        None ->
            Nothing
        Name ->
            Just Target.Name
        Description ->
            Just Target.Description
        Completion inst ->
            Just <| Target.Completion <| Base.get inst
        DueDate ->
            Just Target.DueDate
        DueDateWithTime ->
            Just Target.DueTime
        CustomEnumField id name options inst ->
            Just <| Target.CustomField (Asana.CustomEnumFieldInfo id name options) -- (Base.get inst)
        CustomField customFieldInfo ->
            Just <| Target.CustomField customFieldInfo

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case (msg, model) of
        (Selection str, _) ->
            updateModel props str model
        (CompletionMsg msg', Completion inst) ->
            Base.updateWith CompletionMsg msg' inst |> Base.mapFst Completion
        _ ->
            (model, Cmd.none)

view : Props -> Model -> Html Msg
view props model =
    case model of
        None ->
            viewSelect props.customFields
        Name ->
            viewSimpleTarget "Name"
        Description ->
            viewSimpleTarget "Description"
        Completion config ->
            viewWithConfig "Completion" (Base.viewWith CompletionMsg config)
        DueDate ->
            viewSimpleTarget "Due Date"
        DueDateWithTime ->
            viewSimpleTarget "Due Date With Time"
        CustomEnumField _ name options inst ->
            viewWithConfig (formatCustomEnumName name) <| Base.viewWith EnumMsg inst
        CustomField info ->
            viewSimpleTarget <| customFieldName info

viewSelect : List Asana.CustomFieldInfo -> Html Msg
viewSelect customFields =
    let
        options = List.map viewOption (targetStrings customFields)
    in
        select [ class "FieldOptions-select", Events.on "change" (onChange customFields) ] options

onChange : List Asana.CustomFieldInfo -> Json.Decoder Msg
onChange customFields =
    Json.map Selection <| Json.at ["target", "value"] Json.string

viewOption : String -> Html Msg
viewOption name =
    option [ value name ] [ text name ]

customFieldName : Asana.CustomFieldInfo -> String
customFieldName info =
    case info of
        Asana.CustomTextFieldInfo _ name ->
            formatCustomTextName name
        Asana.CustomNumberFieldInfo _ name _ ->
            formatCustomNumberName name
        Asana.CustomEnumFieldInfo _ name _ ->
            formatCustomEnumName name

formatCustomTextName : String -> String
formatCustomTextName name =
    "Custom Text: " ++ name

formatCustomNumberName : String -> String
formatCustomNumberName name =
    "Custom Number: " ++ name

formatCustomEnumName : String -> String
formatCustomEnumName name =
    "Custom Enum: " ++ name

targetStrings : List Asana.CustomFieldInfo -> List String
targetStrings customFields =
    [ ""
    , "Name"
    , "Description"
    , "Completion"
    , "Due Date"
    , "Due Date With Time"
    ] ++ List.map customFieldName customFields

updateModel : Props -> String -> Model -> (Model, Cmd Msg)
updateModel {records, customFields} str model =
    case str of
        "Name" ->
            (Name, Cmd.none)
        "Description" ->
            (Description, Cmd.none)
        "Completion" ->
            TargetConfig.create
                -- TODO: This mapping should go in with the decoders.
                { defaultMap = \str -> Just <| String.isEmpty str || String.toLower str == "true" || String.toLower str == "done"
                , dataView = \mValue -> 
                    CompletedConfig.create { value = Maybe.withDefault False mValue }
                    -- Transform it to a Just Bool instance from a Bool instance.
                    |> Base.mapFst (Base.mapOutput Just)
                , records = records
                }
            |> Base.pairMap Completion (Cmd.map CompletionMsg)
        "Due Date" ->
            (DueDate, Cmd.none)
        "Due Date With Time" ->
            (DueDateWithTime, Cmd.none)
        str ->
            case matchCustomFieldName str customFields of
                (Just (Asana.CustomEnumFieldInfo id name options)) ->
                    Base.pairMap (CustomEnumField id name options) (Cmd.map EnumMsg)
                        <| TargetConfig.create
                            -- TODO: This mapping should go in with the decoders.
                            { defaultMap = \str -> find (.name >> (==) str) options |> Maybe.map .id
                            , dataView = \value -> 
                                EnumConfig.create
                                    { selectedId = value
                                    , enumOptions = options
                                    }
                            , records = records
                            }
                other ->
                    Maybe.map CustomField other |> Maybe.withDefault None |> flip (,) Cmd.none

matchCustomFieldName : String -> List Asana.CustomFieldInfo -> Maybe Asana.CustomFieldInfo
matchCustomFieldName str =
    List.head << List.filter (customFieldName >> (==) str)

viewSimpleTarget : String -> Html Msg
viewSimpleTarget name =
    withUnselect
        <| div [ class "TargetSelector-selectedField TargetSelector-simpleField" ]
            [ span [ class "TargetSelector-targetName" ] [ text name ]
            ]

viewWithConfig : String -> Html Msg -> Html Msg
viewWithConfig name config =
    withUnselect
        <| div [ class "TargetSelector-selectedField TargetSelector-configField" ]
            [ span [ class "TargetSelector-targetName" ] [ text name ]
            , config
            ]

withUnselect : Html Msg -> Html Msg
withUnselect inner =
    div [ class "TargetSelector-unselect" ]
        [ inner
        , a [ class "TargetSelector-unselectButton", Events.onClick (Selection "") ] [ CommonViews.closeIcon ]
        ]
