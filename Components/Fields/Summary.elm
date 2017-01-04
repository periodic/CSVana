module Components.Fields.Summary exposing (Props, Msg, Data, Instance, create)

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (class)

import Base
import Asana.Model as Asana
import Asana.Target as Target exposing (Target)
import Util

type alias Props =
    { headers : List String
    , customFields : List Asana.CustomFieldResource
    , targets : List (Maybe Target)
    }

type alias Msg =
    ()

type alias Data =
    ()

type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.staticComponent <| view props

--------------------------------------------------------------------------------
-- Private

view : Props -> Html Msg
view props =
    div [ class "FieldSummary" ]
        (List.map2 (,) props.headers props.targets
            |> List.map (viewRow props))

viewRow : Props -> (String, Maybe Target) -> Html Msg
viewRow props (header, target) =
    div [ class "FieldSummary-row Grid" ]
        [ div [ class "FieldSummary-header Cell -5of12" ]
            [ text header ]
        , div [ class "FieldSummary-target Cell -5of12" ]
            [ targetString props target |> text ]
        ]

targetString : Props -> Maybe Target -> String
targetString { customFields } target =
    case target of
        Nothing ->
            ""
        Just Target.Name ->
            "Name"
        Just Target.Description ->
            "Description"
        Just (Target.Assignee _) ->
            "Assignee"
        Just (Target.Completion _) ->
            "Completion"
        Just Target.DueDate ->
            "Due Date"
        Just Target.DueTime ->
            "Due Date With Time"
        Just (Target.CustomText id) ->
            case Util.find (.id >> (==) id) customFields of
                Just field ->
                    "Custom Text: " ++ field.name
                Nothing ->
                    "Unknown custom field"
        Just (Target.CustomNumber id) ->
            case Util.find (.id >> (==) id) customFields of
                Just field ->
                    "Custom Number: " ++ field.name
                Nothing ->
                    "Unknown custom field"
        Just (Target.CustomEnum id _) ->
            case Util.find (.id >> (==) id) customFields of
                Just field ->
                    "Custom Value: " ++ field.name
                Nothing ->
                    "Unknown custom field"
