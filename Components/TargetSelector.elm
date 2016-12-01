module Components.TargetSelector exposing (Props, Msg, Data, Instance, create)

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
    | Selection (Maybe Target)

type alias Data = Maybe Target
type alias Instance = Base.Instance Model Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = (Nothing, Cmd.none)
        , update = update
        , subscriptions = always Sub.none
        , view = view props
        , get = identity
        }

--------------------------------------------------------------------------------
-- Private

type Model
    = None
    | Name
    | Description
    | DueDate
    | DueTime
    | CustomText Asana.CustomFieldInfo
    | CustomNumber Asana.CustomFieldInfo
    | CustomEnum Asana.CustomFieldInfo

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClearSelection ->
            (Nothing, Cmd.none)
        Selection target ->
            (target, Cmd.none)

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
        options = emptyOption :: List.map viewOption targets
    in
        select [ class "FieldOptions-select", Events.on "change" (onChange customFields) ] options

onChange : List Asana.CustomFieldInfo -> Json.Decoder Msg
onChange customFields =
    Json.map Selection <| Json.map (targetFromString customFields) <| Json.at ["target", "value"] Json.string

emptyOption : Html Msg
emptyOption =
    option [ value "" ] []

viewOption : Target -> Html Msg
viewOption target =
    option [ value <| targetString target ] [ text <| targetString target ]

matchCustomFieldName : String -> List Asana.CustomFieldInfo -> Maybe Asana.CustomFieldInfo
matchCustomFieldName str =
    List.head << List.filter (Asana.customFieldName >> (++) "Custom Field: " >> (==) str)

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
            "Due Date with time"
        Target.CustomField customField ->
            "Custom Field: " ++ Asana.customFieldName customField

selectedModel : List Asana.CustomFieldInfo -> String -> Model
selectedModel customFields str =
    case str of
        "Name" ->
            Just Name
        "Description" ->
            Just Description
        "Due Date" ->
            Just Target.DueDate
        "Due Date with time" ->
            Just Target.DueTime
        str ->
            matchCustomFieldName str customFields |> Maybe.map customFieldToModel |> Maybe.withDefault None

customFieldToModel : Asana.CustomFieldInfo -> Model
customFieldToModel customField =
    case customField.fieldType of
        Asana.CustomText ->
            CustomText customField
        Asana.CustomNumber ->
            CustomNumber customField
        Asana.CustomEnum ->
            CustomEnum customField


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
        Target.CustomField fieldInfo ->
            viewMappingTarget target

viewSimpleTarget : Target -> Html Msg
viewSimpleTarget target =
    withUnselect <| text <| targetString target

viewMappingTarget : Target -> Html Msg
viewMappingTarget target =
    case target of 
        Target.Name ->
            Debug.crash "Simple viewed with mapping."
        Target.Description ->
            Debug.crash "Simple viewed with mapping."
        Target.DueDate ->
            Debug.crash "Unimplemented"
        Target.DueTime ->
            Debug.crash "Unimplemented"
        Target.CustomField fieldInfo ->
            Debug.crash "Unimplemented"

withUnselect : Html Msg -> Html Msg
withUnselect inner =
    div []
        [ inner
        , a [ Events.onClick ClearSelection ] [ text "x"]
        ]
