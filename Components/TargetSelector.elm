module Components.TargetSelector exposing (Props, Msg, Spec, Component, component, target)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Json

import Base
import Asana.Model as Asana
import Asana.Target as Target exposing (Target)

type alias Props =
    { customFields : List Asana.CustomFieldInfo
    }

type Msg
    = ClearSelection
    | Selection Target

type alias Spec = Base.Spec Model Msg
type alias Component = Base.Component Model Msg

component : Props -> Spec
component props =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view props
    }

target : Base.Instance msg -> Maybe Target
target instance =
    Nothing

--------------------------------------------------------------------------------
-- Private

type alias Model
    = Maybe Target

init : (Model, Cmd Msg)
init = (Unselected, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClearSelection ->
            (Unselected, Cmd.none)
        Selection target ->
            (target, Cmd.none)

subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none

view : Props -> Model -> Html Msg
view props model =
    case model of
        Nothing ->
            viewSelect props.customFields
        Just target ->
            viewTarget target

viewSelect : List Asana.CustomFieldInfo -> Html Msg
viewSelect customFields =
    let
        targets = allTargets customFields
        options = List.map viewOption targets
    in
        select [ class "FieldOptions-select", Events.on "change" (onChange customFields) ] options

onChange : List Asana.CustomFieldInfo -> Json.Decoder Msg
onChange customFields =
    Json.map Selection <| Json.map (targetFromString customFields) <| Json.at ["target", "value"] Json.string


viewOption : Target -> Html Msg
viewOption target =
    option [ value <| targetString target ] [ text <| targetString target ]

matchCustomFieldName : String -> List Asana.CustomFieldInfo -> Maybe Asana.CustomFieldInfo
matchCustomFieldName str =
    List.head << List.filter (Asana.customFieldName >> (++) "Custom Field: " >> (==) str)

targetString : Target -> String
targetString target =
    case target of
        Target.None ->
            "None"
        Target.Name ->
            "Name"
        Target.Description ->
            "Description"
        Target.DueDate ->
            "Due Date"
        Target.DueTime ->
            "Due Date with time"
        Target.CustomField customField ->
            "Custom Field: " ++ Asana.customFieldName customField

targetFromString : List Asana.CustomFieldInfo -> String -> Target
targetFromString customFields str =
    case str of
        "None" ->
            Target.None
        "Name" ->
            Target.Name
        "Description" ->
            Target.Description
        "Due Date" ->
            Target.DueDate
        "Due Date with time" ->
            Target.DueTime
        str ->
            matchCustomFieldName str customFields |> Maybe.map Target.CustomField |> Maybe.withDefault Target.None

allTargets : List Asana.CustomFieldInfo -> List Target
allTargets customFields =
    let
        customFieldTargets = List.map Target.CustomField customFields
        genericTargets =
            [ Target.None
            , Target.Name
            , Target.Description
            , Target.DueDate
            , Target.DueTime
            ]
    in
        genericTargets ++ customFieldTargets


viewTarget : Target -> Html Msg
viewTarget target =
    case target of
        Target.Name ->
            viewSimpleTarget  target
        Target.Description ->
            viewSimpleTarget  target
        Target.DueDate ->
            viewMappingTarget target
        Target.DueTime ->
            viewMappingTarget target
        CustomField fieldInfo ->
            viewMappingTarget target

viewSimpleTarget : Target -> Html Msg
viewSimpleTarget target =
    withUnselect <| text <| targetString target

viewMappingTarget : String ->  Html Msg
viewMappingTarget target =
    case target of 
        Target.Name ->
            viewSimpleTarget  target
        Target.Description ->
            viewSimpleTarget  target
        Target.DueDate ->
            viewMappingTarget target
        Target.DueTime ->
            viewMappingTarget target
        CustomField fieldInfo ->
            viewMappingTarget target

withUnselect : Html Msg -> Html Msg
withUnselect inner =
    div []
        [ inner
        , a [ Events.onClick ClearSelection ] [ text "x"]
        ]
