module Components.FieldOptions exposing (Props, Msg, Component, component, setNumFields, getTargets)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Json

import Base
import Asana.Model as Asana
import Asana.Target as Target exposing (Target)

type alias Props =
    { customFields : List Asana.CustomFieldInfo
    , numFields : Int
    }

type alias Model =
    { targets : Array (Maybe Target)
    }

type Msg
    = TargetUpdated Int (Maybe Target)
    | UpdateNumFields Int

type alias Spec = Base.Spec Model Msg
type alias Component = Base.Component Model Msg

component : Props -> Spec
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = always Sub.none
    }

getTargets : Component -> List (Maybe Target)
getTargets =
    Array.toList << .targets << Base.stateC

setNumFields : Int -> Component -> (Component, Cmd Msg)
setNumFields =
    Base.updateC << UpdateNumFields


--------------------------------------------------------------------------------
-- Private

init : Props -> (Model, Cmd Msg)
init { numFields } =
    let
        targets = Array.repeat numFields Nothing
    in
        ({ targets = targets }, Cmd.none)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case msg of
        TargetUpdated index target ->
            ({ model | targets = Array.set index target model.targets }, Cmd.none)
        UpdateNumFields numFields -> 
            let
                targets = model.targets
                targets' =
                    if (numFields <= Array.length targets)
                        then
                            Array.slice 0 numFields targets
                        else
                            Array.append targets <| Array.repeat (Array.length targets - numFields) Nothing
            in
                ({ model | targets = targets' }, Cmd.none)

view : Props -> Model -> Html Msg
view props {targets} =
    div [ class "FieldOptions" ]
        (Array.toList <| Array.indexedMap (viewSelect props.customFields) targets)

allTargets : List Asana.CustomFieldInfo -> List Target
allTargets customFields =
    let
        customFieldTargets = List.map Target.CustomField customFields
        genericTargets =
            [ Target.Name
            , Target.Description
            , Target.DueDate
            , Target.DueTime
            ]
    in
        genericTargets ++ customFieldTargets

viewSelect : List Asana.CustomFieldInfo -> Int -> Maybe Target -> Html Msg
viewSelect customFields index selectedTarget =
    let
        targets = allTargets customFields
        options = emptyOption selectedTarget :: List.map (viewOption selectedTarget) targets
    in
        select [ class "FieldOptions-select", Events.on "change" (onChange index customFields) ] options

emptyOption : Maybe Target -> Html Msg
emptyOption selectedTarget =
    option [ selected (selectedTarget == Nothing), value "" ] []

onChange : Int -> List Asana.CustomFieldInfo -> Json.Decoder Msg
onChange index customFields =
    Json.map (TargetUpdated index) <| Json.map (targetFromString customFields) <| Json.at ["target", "value"] Json.string


viewOption : Maybe Target -> Target -> Html Msg
viewOption selectedTarget target =
    option [ selected (selectedTarget == Just target), value <| targetString target ] [ text <| targetString target ]

targetString : Target -> String
targetString target =
    case target of
        Target.Name ->
            "Name"
        Target.Description ->
            "Description"
        Target.DueDate ->
            "Due Date"
        Target.DueTime ->
            "Due Time"
        Target.CustomField customField ->
            "CF: " ++ Asana.customFieldName customField

targetFromString : List Asana.CustomFieldInfo -> String -> Maybe Target
targetFromString customFields str =
    case str of
        "Name" ->
            Just Target.Name
        "Description" ->
            Just Target.Description
        "Due Date" ->
            Just Target.DueDate
        "Due Time" ->
            Just Target.DueTime
        str ->
            matchCustomFieldName str customFields |> Maybe.map Target.CustomField

matchCustomFieldName : String -> List Asana.CustomFieldInfo -> Maybe Asana.CustomFieldInfo
matchCustomFieldName str =
    List.head << List.filter (Asana.customFieldName >> (++) "CF: " >> (==) str)

