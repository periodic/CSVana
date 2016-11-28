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
    { customFields : List Asana.CustomField
    , numFields : Int
    }

type alias Model =
    { targets : Array Target
    }

type Msg
    = TargetUpdated Int Target
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

getTargets : Component -> List Target
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
        targets = Array.repeat numFields Target.None
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
                            Array.append targets <| Array.repeat (Array.length targets - numFields) Target.None
            in
                ({ model | targets = targets' }, Cmd.none)

view : Props -> Model -> Html Msg
view props {targets} =
    div [ class "FieldOptions" ]
        (Array.toList <| Array.indexedMap (viewSelect props.customFields) targets)

allTargets : List Asana.CustomField -> List Target
allTargets customFields =
    let
        customFieldTargets = List.map Target.CustomField customFields
        genericTargets =
            [ Target.None
            , Target.Name
            , Target.Description
            , Target.DueDate
            ]
    in
        genericTargets ++ customFieldTargets

viewSelect : List Asana.CustomField -> Int -> Target -> Html Msg
viewSelect customFields index selectedTarget =
    let
        targets = allTargets customFields
        options = List.map (viewOption selectedTarget) targets
    in
        select [ class "FieldOptions-select", Events.on "change" (onChange index customFields) ] options

onChange : Int -> List Asana.CustomField -> Json.Decoder Msg
onChange index customFields =
    Json.map (TargetUpdated index) <| Json.map (targetFromString customFields) <| Json.at ["target", "value"] Json.string


viewOption : Target -> Target -> Html Msg
viewOption selectedTarget target =
    option [ selected (selectedTarget == target), value <| targetString target ] [ text <| targetString target ]

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
        Target.CustomField customField ->
            "CF: " ++ customField.name

targetFromString : List Asana.CustomField -> String -> Target
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
        str ->
            List.foldr
                (\customField target ->
                    if str == "CF: " ++ customField.name
                        then Target.CustomField customField
                        else target)
                Target.None
                customFields
