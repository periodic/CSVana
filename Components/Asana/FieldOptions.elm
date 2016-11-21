module Components.Asana.FieldOptions exposing (Props, Model, Msg, component)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json

import Base exposing (..)
import Components.Asana.Api as Api
import Components.Asana.Model as Asana
import Components.Asana.ApiResource as ApiResource

type Target
    = NoTarget
    | NameTarget
    | DescriptionTarget
    | DueDateTarget
    | CustomFieldTarget Asana.CustomField

type alias Props =
    { customFields : List Asana.CustomField
    , maxValues : Int
    }

type alias Model =
    { targets : List Target
    }

type Msg =
    TargetUpdated Index Target

component : Props -> Component Model Msg
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = always Sub.none
    }

init : Props -> (Model, Cmd Msg)
init { maxValues } =
    let
        targets = List.repeat maxValues NoTarget
    in
        ({ targets = targets }, Cmd.none)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    (model, Cmd.none)

view : Props -> Model -> Html Msg
view props {targets} =
    div [ class "FieldOptions" ]
        (List.indexedMap (viewSelect props.customFields) targets)

getTargets : Model -> List Target
getTargets = .targets

--------------------------------------------------------------------------------
-- Private

allTargets : List Asana.CustomField -> List Target
allTargets customFields =
    let
        customFieldTargets = List.map CustomFieldTarget customFields
        genericTargets =
            [ NoTarget
            , NameTarget
            , DescriptionTarget
            , DueDateTarget
            ]
    in
        genericTargets ++ customFieldTargets

viewSelect : List Asana.CustomField -> Int -> Target -> Html Msg
viewSelect customFields index selectedTarget =
    let
        targets = allTargets customFields
        options = List.map (viewOption selectedTarget) targets
    in
        select [ class "FieldOptions-select", on "change" (onChange index customFields) ] options

onChange : Index -> List Asana.CustomField -> Json.Decoder Msg
onChange index customFields =
    Json.map (TargetUpdated index) <| Json.map (targetFromString customFields) <| Json.at ["target", "value"] Json.string


viewOption : Target -> Target -> Html Msg
viewOption selectedTarget target =
    option [ selected (selectedTarget == target), value <| targetString target ] [ text <| targetString target ]

targetString : Target -> String
targetString target =
    case target of
        NoTarget ->
            "None"
        NameTarget ->
            "Name"
        DescriptionTarget ->
            "Description"
        DueDateTarget ->
            "Due Date"
        CustomFieldTarget customField ->
            "CF: " ++ customField.name

targetFromString : List Asana.CustomField -> String -> Target
targetFromString customFields str =
    case str of
        "None" ->
            NoTarget
        "Name" ->
            NameTarget
        "Description" ->
            DescriptionTarget
        "Due Date" ->
            DueDateTarget
        str ->
            List.foldr
                (\customField target ->
                    if str == "CF: " ++ customField.name
                        then CustomFieldTarget customField
                        else target)
                NoTarget
                customFields
